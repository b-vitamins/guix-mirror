;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2025 Artur Wroblewski <wrobell@riseup.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services rabbitmq)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages rabbitmq)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:export (<rabbitmq-configuration>
             rabbitmq-configuration
             rabbitmq-configuration?
             rabbitmq-configuration-rabbitmq
             rabbitmq-configuration-config-file
             rabbitmq-configuration-plugins
             rabbitmq-service-type))

; by default start on local ipv4 and ipv6 interfaces only; see also:
;
;   https://www.rabbitmq.com/docs/networking
;
; NOTE: how to enable plugins to listen on localhost only
(define %default-rabbitmq-config-file
  (plain-file "rabbitmq.conf" "
listeners.tcp.1 = 127.0.0.1:5672
listeners.tcp.2 = ::1:5672
"))

(define-record-type* <rabbitmq-configuration>
  rabbitmq-configuration make-rabbitmq-configuration
  rabbitmq-configuration?
  (rabbitmq rabbitmq-configuration-rabbitmq
            (default rabbitmq))
  ; it can be a mnesia database or a khepri database, so use "metadata" instead
  ; of the traditional "mnesia"
  (metadata-dir rabbitmq-configuration-metadata-dir
                (default "/var/lib/rabbitmq/metadata"))
  (config-file rabbitmq-configuration-config-file
               (default %default-rabbitmq-config-file))
  (plugins rabbitmq-configuration-plugins (default '())))

(define %rabbitmq-accounts
  (list (user-group (name "rabbitmq") (system? #t))
        (user-account
         (name "rabbitmq")
         (group "rabbitmq")
         (system? #t)
         (comment "RabbitMQ server user")
         (home-directory "/var/lib/rabbitmq")
         (shell (file-append shadow "/sbin/nologin")))))

(define (rabbitmq-activation config)
  (let* ((metadata-dir (rabbitmq-configuration-metadata-dir config))
         (plugins (string-join (rabbitmq-configuration-plugins config) ",")))
    (with-imported-modules '((guix build utils))
      #~(begin
        (use-modules (guix build utils))
        (let ((user (getpwnam "rabbitmq")))
          (mkdir-p "/etc/rabbitmq")
          (mkdir-p "/var/lib/rabbitmq")
          (mkdir-p "/var/log/rabbitmq")
          (mkdir-p "/var/run/rabbitmq")
          (chown "/var/lib/rabbitmq" (passwd:uid user) (passwd:gid user))
          (chown "/var/log/rabbitmq" (passwd:uid user) (passwd:gid user))
          (chown "/var/run/rabbitmq" (passwd:uid user) (passwd:gid user))

          ; create metadata directory and file with the enabled plugins
          (mkdir-p #$metadata-dir)
          (with-output-to-file (string-append #$metadata-dir "/enabled_plugins")
            (lambda ()
              (display (format #f "[~a]." #$plugins))))
          (chown #$metadata-dir (passwd:uid user) (passwd:gid user))
          (chown (string-append #$metadata-dir "/enabled_plugins") (passwd:uid user) (passwd:gid user)))))))

(define (rabbitmq-shepherd-service config)
  (match-record config <rabbitmq-configuration>
    (rabbitmq metadata-dir config-file plugins)
    (with-imported-modules (source-module-closure '((gnu build shepherd)))
      (list (shepherd-service
              (provision '(rabbitmq))
              (documentation "Run the RabbitMQ daemon.")
              (requirement '(user-processes loopback))
              (modules '((gnu build shepherd)))
              (start #~(make-forkexec-constructor
                        `(#$(file-append rabbitmq "/sbin/rabbitmq-server"))
                        #:pid-file "/var/run/rabbitmq/pid"
                        #:user "rabbitmq"
                        #:group "rabbitmq"
                        #:environment-variables
                        (append
                          (list (string-append "RABBITMQ_CONFIG_FILE=" #$config-file)
                                "RABBITMQ_PID_FILE=/var/run/rabbitmq/pid"
                                "RABBITMQ_CONF_ENV_FILE=/run/current-system/profile/etc/rabbitmq/rabbitmq-env.conf"
                                (string-append "RABBITMQ_ENABLED_PLUGINS_FILE=" #$metadata-dir "/enabled_plugins")
                                (string-append "RABBITMQ_MNESIA_BASE=" #$metadata-dir)
                                "RABBITMQ_LOG_BASE=/var/log/rabbitmq")
                          (environ))))
              (stop #~(make-kill-destructor)))))))

(define rabbitmq-service-type
  (service-type
    (name 'rabbitmq)
    (description "RabbitMQ service type")
    (extensions
      (list
        (service-extension shepherd-root-service-type rabbitmq-shepherd-service)
        (service-extension activation-service-type rabbitmq-activation)
        (service-extension account-service-type (const %rabbitmq-accounts))))
    (default-value (rabbitmq-configuration))))

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu packages rabbitmq)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages elixir)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public rabbitmq
  (package
    (name "rabbitmq")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rabbitmq/rabbitmq-server/releases/download/v"
                    version
                    "/rabbitmq-server-" version ".tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (sha256
               (base32
                "0lhwg52mdch44a7qczak7nyn1y599cz3k45czdaxhm17kkqim2kq"))
              (patches (search-patches "rabbitmq-defaults.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(
       #:make-flags
       (list "-j1" (string-append "RMQ_ERLAPP_DIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
        ; erlang.mk contains the entries
        ;
        ;     ELIXIR_BIN ?= $(shell readlink -f `which elixir`)
        ;     ELIXIR_LIBS ?= $(abspath $(dir $(ELIXIR_BIN))/../lib)
        ;
        ; but fails to find elixir.app during build of plugins.
        ;
        ; Despite the failure, the build is successful. and not all plugins are
        ; built. Running rabbitmqctl gives error

        ;     undefined function Elixir.RabbitMQCtl':main/1
        ;
        ; After setting the environment variables, the command-line utilities
        ; build successfully.
        (add-before 'build 'set-elixir-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                             (let* ((elixir (assoc-ref inputs "elixir"))
                                    (elixir-bin (string-append elixir "/bin/elixir"))
                                    (elixir-lib (string-append elixir "/lib/elixir/lib")))
                               (setenv "ELIXIR_BIN" elixir-bin)
                               (setenv "ELIXIR_LIBS"
                                       (string-join
                                         (find-files elixir-lib "ebin$" #:directories? #t)
                                         ":"))
                               #t)))
         (add-after 'install 'wrap-rabbitmq
           (lambda* (#:key outputs inputs #:allow-other-keys)
                    (let* ((out-dir (assoc-ref outputs "out"))
                           (rabbitmq-server (string-append out-dir "/sbin/rabbitmq-server"))
                           (rabbitmq-programs (list
                                                "rabbitmqctl"
                                                "rabbitmq-diagnostics"
                                                "rabbitmq-plugins"
                                                "rabbitmq-queues"
                                                "rabbitmq-server"
                                                "rabbitmq-streams"
                                                "rabbitmq-upgrade"))
                           (erlang-bin (string-append (assoc-ref inputs "erlang") "/bin"))
                           (glibc-bin (string-append (assoc-ref inputs "glibc") "/bin"))
                           (coreutils-bin (string-append (assoc-ref inputs "coreutils") "/bin")))
                      ; starting rabbitmq server requires: getconf, df, erl
                      (wrap-program rabbitmq-server
                                    `("PATH" ":" prefix
                                    (,glibc-bin ,coreutils-bin ,erlang-bin)))
                      ; each of the rabbitmq-programs requires erlang cookie
                      ; stored in rabbitmq's user home directory
                      (for-each
                        (lambda (prog)
                          (wrap-program (string-append out-dir "/sbin/" prog)
                                        `("HOME" = ("/var/lib/rabbitmq"))))
                        rabbitmq-programs)
                      #t)))
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'patch-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out-dir (assoc-ref outputs "out")))
               (substitute* (string-append out-dir "/sbin/rabbitmq-env")
                 (("basename") (which "basename"))
                 (("dirname") (which "dirname"))
                 (("readlink") (which "readlink")))
               #t))))))
    (native-inputs
     `(("erlang" ,erlang)
       ("elixir" ,elixir)
       ("coreutils" ,coreutils)
       ("glibc" ,glibc)
       ("python" ,python-wrapper)
       ("which" ,which)
       ("p7zip" ,p7zip)))
    (propagated-inputs `(("erlang" ,erlang)))
    (synopsis "Reliable messaging and streaming broker")
    (description "RabbitMQ is a robust messaging and streaming broker that
supports multiple messaging protocols, including AMQP 1.0 and MQTT 5.0. It
offers features such as message routing, filtering, streaming, federation, and
clustering. It is designed for reliability and flexibility.")
    (home-page "https://www.rabbitmq.com/")
    (license license:asl2.0)))

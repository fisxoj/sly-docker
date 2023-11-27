;;; sly-docker.el --- Support for development using docker for SLY  -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; URL: https://github.com/fisxoj/sly-docker
;; Keywords: lisp, sly, docker, docker-compose
;; Package-Requires: ((emacs "24") (sly "1.0.0-beta2") (tramp-container "0.1"))
;; Author: Matt Novenstern <fisxoj@gmail.com>
;;
;; Copyright (C) 2019 Matt Novenstern
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `sly-docker' is an external contrib for sly that supports
;; development in a dockerized environment.  Develop your code in a
;; container running an image that exposes a slynk connection (like
;; [parentheticalenterprises/sbcl-quicklisp-slynk](https://github.com/fisxoj/lisp-images/#slynk))
;; and connect to it using `sly-connect`.  This package will perform
;; the pathanme translations to work on code thats running in the
;; container but mounted from the host.
;;
;; See README.org
;;
;;; Code:

(require 'sly)
(require 'cl-lib)
(require 'tramp-container)
(require 'json)


;; FIXME: Remove once the PR lands in sly to allow function tests for
;; filename translators
(defun sly-find-filename-translators (hostname)
  (cond ((cdr (cl-assoc-if (lambda (predicate)
                             (typecase predicate
                               ((or function symbol) (funcall predicate hostname))
                               (string (string-match-p predicate hostname))))
                           sly-filename-translations)))
        (t (list #'identity #'identity))))


(define-sly-contrib sly-docker
  "Path translation for docker containers."
  (:authors "Matt Novenstern <fisxoj@gmail.com>")
  (:license "GPL")
  (:sly-dependencies sly-tramp)
  (:on-load (add-to-list 'sly-filename-translations
			 (sly-docker-create-filename-translator))))

(defcustom sly-docker-container-runtime "podman"
  "Container runtime to interact with (podman or docker)"
  :group 'sly-docker
  :version "29.1"
  :type '(choice (const "podman")
                 (const "docker")
                 (string)))


(defun sly-docker--running-containers ()
  (when-let ((default-directory tramp-compat-temporary-file-directory)
             (raw-list (shell-command-to-string
                        (concat sly-docker-container-runtime
                                " ps --format '{{.ID}}\t{{.Names}}'")))
             (lines (split-string raw-list "\n" 'omit))
             (pairs (mapcar
                     (lambda (line)
                       (when (string-match
                              (rx bol (group (1+ nonl))
                                  "\t" (group (1+ nonl)) eol)
                              line)
                         (cons (match-string 1 line) (match-string 2 line))))
                     lines)))
    pairs))


(defun sly-docker--docker-hostname-is-docker-container-p (hostname)
  (cl-find hostname (sly-docker--running-containers)
	   :test 'equal
	   :key 'car))


(defun sly-docker-create-filename-translator ()
  (list 'sly-docker--docker-hostname-is-docker-container-p
	'sly-docker--translate-local-filename
	'sly-docker--translate-container-filename))


(defun sly-docker--get-bind-mount-translations (container)
  "Select the bind mounts (ie. the paths inside the CONTAINER that correspond to paths on the host) attached to a container and create an association list of (host-path . container-path) pairs."

  (cl-flet ((bind-mount-p (mount)
                          (string-equal (gethash "Type" mount) "bind"))
            (get-mounts-for-container ()
                                      (gethash "Mounts" (first (json-read-from-string (shell-command-to-string (format "%s container inspect %s" docker-tramp-docker-executable container)))))))

    (let ((json-object-type 'hash-table)
          (json-array-type 'list)
          (json-key-type 'string))

      (mapcar (lambda (m) (cons (gethash "Source" m) (gethash "Destination" m)))
              ;; If it's not a bind mount, it's not from the host machine and it's a dead end for us
              (cl-remove-if-not #'bind-mount-p (get-mounts-for-container))))))


(defun sly-docker--translate-local-filename (filename)
  "Translates a FILENAME from inside a container to outside (host) by looking at the bind mounts associated with a container."

  ;; (message "sly-docker--translate-local-filename: translating %s" filename)
  (let* ((translation-data (cl-find-if (lambda (destination) (string-prefix-p destination filename))
                                       (sly-docker--get-bind-mount-translations (sly-machine-instance))
                                       :key 'car)))
    (if translation-data
        (let ((path-difference (cl-subseq filename (length (car translation-data)))))

          (concat (cdr translation-data) path-difference))
      filename)))


(defun sly-docker--translate-container-filename (filename)
  "Translates a FILENAME from inside a container to outside (host) by looking at the bind mounts associated with a container."

  ;; (message "sly-docker--translate-container-filename: translating %s" filename)
  (let* ((translation-data (cl-find-if (lambda (destination) (string-prefix-p destination filename))
                                       (sly-docker--get-bind-mount-translations (sly-machine-instance))
                                       :key 'cdr)))
    (if translation-data
        (let ((path-difference (cl-subseq filename (length (cdr translation-data)))))

          (concat (car translation-data) path-difference))
      (tramp-make-tramp-file-name
       sly-docker-container-runtime
       nil
       ""
       (sly-machine-instance)
       nil
       filename))))


;;;###autoload
(add-to-list 'sly-contribs 'sly-docker 'append)


(provide 'sly-docker)

;;; sly-docker.el ends here

(require 'docker-tramp)
(require 'sly)
(require 'sly-docker)
(require 'sly-tests "lib/sly-tests")
(require 'cl-lib)

(define-sly-ert-test sly-docker-basic-test ()
  (with-temp-buffer
    (lisp-mode)
    (should (member 'sly-docker sly-contribs))
    (should sly-filename-translations)
    (should (= (length sly-filename-translations) 1))))

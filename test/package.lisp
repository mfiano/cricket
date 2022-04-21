(in-package #:cl-user)

(defpackage #:cricket.test
  (:local-nicknames
   (#:c #:cricket)
   (#:png #:pngload)
   (#:u #:mfiano-utils))
  (:use #:cl
        #:prove))

(in-package #:cricket.test)

(setf *enable-colors* nil)

;; HACK: Prove is using ASDF in a deprecated way causing an annoying but
;; harmless warning message in the middle of tests, so wrap test calling to hide
;; the annoyance for now.
(defun run-tests (object)
  (handler-bind ((asdf/operate:recursive-operate #'muffle-warning))
    (run object)))

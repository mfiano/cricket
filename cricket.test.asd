(asdf:defsystem #:cricket.test
  :description "Tests for cricket."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/cricket"
  :depends-on
  (#:cricket
   #:mfiano-utils
   #:pngload
   #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (op c) (uiop:symbol-call '#:cricket.test '#:run-tests c))
  :components
  ((:file "package")
   (:file "common")
   (:test-file "test")))

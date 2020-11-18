

;;;; simple-parser.asd

(asdf:defsystem #:simple-parser
  :description "Very simple parser framework (but quite good)"
  :author "VIP"
  :license "vip"
  :depends-on (#:cl-ppcre #:anaphors)
  :serial t
  :components ((:file "package")
               (:file "simple-parser")
               ))


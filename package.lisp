
(in-package :cl-user)

(defpackage :simple-parser
  (:use :cl :cl-ppcre :anaphors)
  ;; !!! Need to export lots of the things
  (:export #:execute-parser
           
           #:sp-eof
           #:sp-next-char
           #:sp-read-char
           #:sp-skip-char
           #:sp-next-char-is
           #:sp-scan
           #:sp-starts-with
           #:sp-starts-with-string
           #:sp-starts-with-symbol
           #:sp-skip-whitespace
           #:sp-trail
           #:sp-already-parsed
           #:sp-try
           #:sp-error

           ;; I would like, perhaps, not to export these, but maybe it's ok
           #:*parser-input-string*
           #:*parser-input-position*
           
           ))




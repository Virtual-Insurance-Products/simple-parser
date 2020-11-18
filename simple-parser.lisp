

(in-package :simple-parser)



;; This is a really simple parser framework...

(defparameter *parser-input-string* nil)
(defparameter *parser-input-position* 0)

(defun execute-parser (parser input-string &key allow-trailing-junk)
  (let ((*parser-input-string* input-string)
        (*parser-input-position* 0))
    (let ((result (funcall parser)))
      (unless allow-trailing-junk
        (awhen (sp-trail)
          (error "Trailing junk: ~A" it)))
      result)))


;; arguably it would be better to just use a stream. Oh well...

(defun sp-eof ()
  (>= *parser-input-position* (length *parser-input-string*)))

(defun sp-next-char ()
  (unless (sp-eof)
    (elt *parser-input-string* *parser-input-position*)))

(defun sp-read-char ()
  (awhen (sp-next-char)
    (sp-skip-char)
    it))

;; should this error if EOF?
(defun sp-skip-char ()
  (incf *parser-input-position*))


(defun sp-next-char-is (char)
  (awhen (sp-next-char)
    (eql it char)))

;; normally the pattern should be tied to the start...
;; this will try to match a pattern and return the matched string if succesful, skipping over it. Otherwise it will return nil and not move the scanner...
(defun sp-scan (pattern &key ignore-case)
  (when (and (stringp pattern)
             (not (eql #\^ (elt pattern 0))))
    (error "sp-scan regex must be tied to start of input stream with ^"))
  (awhen (scan-to-strings (create-scanner pattern :case-insensitive-mode ignore-case)
                          (subseq *parser-input-string* *parser-input-position*))
    (progn
      (incf *parser-input-position* (length it))
      it)))


(defun sp-starts-with (string)
  (awhen (and (>= (length *parser-input-string*)
                  (+ *parser-input-position*
                     (length string)))
              (equal (subseq *parser-input-string* *parser-input-position*
                             (+ *parser-input-position*
                                (length string)))
                     string))
    (incf *parser-input-position* (length string))
    string))


;; This should be about as efficient as possible and can be case insensitiv
;; it will, of course, be sensitive to spacing differences, which could be annoying
(defun sp-starts-with-string (string &key (ignore-case t))
  (when (>= (length *parser-input-string*)
            (+ *parser-input-position* (length string)))
    (when (if ignore-case
              (string-equal *parser-input-string*
                            string
                            :start1 *parser-input-position*
                            :end1 (+ *parser-input-position*
                                     (length string)))
              (string= *parser-input-string*
                       string
                       :start1 *parser-input-position*
                       :end1 (+ *parser-input-position*
                                (length string))))
      (incf *parser-input-position* (length string)))))

(defun sp-starts-with-symbol (symbol)
  (and (sp-starts-with-string (symbol-name symbol))
       symbol))


(defun sp-skip-whitespace ()
  (loop while (member (sp-next-char)
                       (list #\Tab #\ (elt "
" 0)))
       do (sp-skip-char)))


;; that which is left unparsed
(defun sp-trail ()
  (unless (sp-eof)
    (subseq *parser-input-string* *parser-input-position*)))

(defun sp-already-parsed ()
  (subseq *parser-input-string* 0 *parser-input-position*))

;; This saves the position, tries the thunk and restores the position unless the whole thing succeeds
;; it gives us *OPTIONAL* infinite lookahead
(defun sp-try (thunk)
  (let ((saved-position *parser-input-position*))
    (or (funcall thunk)
        (progn
          (setf *parser-input-position* saved-position)
          nil))))


;; handy function for reporting errors
(defun sp-error (format &rest args)
  (apply #'error
         `(,(concatenate 'string format " at input character ~A~%=======================~%~A<<<HERE>>>~A")
            ,@args
            ,(+ 1 *parser-input-position*)
            ,(let ((parsed (sp-already-parsed)))
                  (if (> (length parsed) 20)
                      (subseq parsed (- (length parsed) 20))
                      parsed))
            ,(or (sp-trail) ""))))


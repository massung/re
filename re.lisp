;;;; Lua-style Pattern Matching for LispWorks
;;;;
;;;; Copyright (c) 2012 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defpackage :re
  (:use :cl :lw :parsergen)
  (:export
   #:re
   #:re-match

   ;; interface
   #:compile-re
   #:match-re
   #:find-re
   #:split-re
   #:replace-re

   ;; macros
   #:with-re-match
   #:with-re

   ;; match readers
   #:match-string
   #:match-groups
   #:match-pos-start
   #:match-pos-end))

(in-package :re)

(defclass re ()
  ((pattern     :initarg :pattern     :reader re-pattern)
   (match-start :initarg :match-start :reader re-match-start-p)
   (expression  :initarg :expression  :reader re-expression))
  (:documentation "Regular expression."))

(defclass re-match ()
  ((match     :initarg :match     :reader match-string)
   (groups    :initarg :groups    :reader match-groups)
   (start-pos :initarg :start-pos :reader match-pos-start)
   (end-pos   :initarg :end-pos   :reader match-pos-end))
  (:documentation "Matched token."))

(defmethod print-object ((re re) s)
  "Output a regular expression to a stream."
  (print-unreadable-object (re s :type t)
    (format s "~s" (re-pattern re))))

(defmethod print-object ((match re-match) s)
  "Output a regular expression match to a stream."
  (print-unreadable-object (match s :type t)
    (format s "~s" (match-string match))))

(defmethod make-load-form ((re re) &optional env)
  "Tell the system how to save and load a regular expression to a FASL."
  (with-slots (pattern case-fold multi-line)
      re
    `(compile-re ,pattern :case-fold ,case-fold :multi-line ,multi-line)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((dispatch-re (s c n)
           (declare (ignorable c n))
           (let ((re (with-output-to-string (re)
                       (loop :for c := (read-char s t nil t) :do
                         (case c
                           (#\/ (return))
                           (#\\ (let ((c (read-char s t nil t)))
                                  (princ c re)))
                           (otherwise
                            (princ c re)))))))
             (compile-re re))))
    (set-dispatch-macro-character #\# #\/ #'dispatch-re)))

(defmacro with-re-match ((match match-expr &key no-match) &body body)
  "Intern match symbols to execute a body."
  (let (($$ (intern "$$" *package*))
        ($1 (intern "$1" *package*))
        ($2 (intern "$2" *package*))
        ($3 (intern "$3" *package*))
        ($4 (intern "$4" *package*))
        ($5 (intern "$5" *package*))
        ($6 (intern "$6" *package*))
        ($7 (intern "$7" *package*))
        ($8 (intern "$8" *package*))
        ($9 (intern "$9" *package*))
        ($_ (intern "$_" *package*)))
    `(let ((,match ,match-expr))
       (if (null ,match)
           ,no-match
         (destructuring-bind (,$$ &optional ,$1 ,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9 &rest ,$_)
             (cons (match-string ,match) (match-groups ,match))
           (declare (ignorable ,$$ ,$1 ,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9 ,$_))
           (progn ,@body))))))

(defvar *match-stream*)
(defvar *match-groups*)

(defun capture-groups (s)
  "Return a list of strings that were captured by groups."
  (flet ((capture (groups group)
           (destructuring-bind (start end)
               group
             (push (subseq s start end) groups))))
    (reduce #'capture *match-groups* :initial-value nil)))

(defun satisfy (pred)
  "Match a character that satisfies a predicate."
  (let ((c (read-char *match-stream* nil nil)))
    (if (and c (funcall pred c))
        t
      (when c
        (unread-char c *match-stream*)))))

(defun unsatisfy (pred)
  "Match a character that doesn't satisfy a predicate."
  (not (satisfy #'(lambda (c) (funcall pred c)))))

(defun space-p (c)
  "T if c is a whitespace character."
  (find c '(#\space #\tab) :test #'char=))

(defun newline-p (c)
  "T if c is a newline character."
  (find c '(#\return #\linefeed #\newline) :test #'char=))

(defun punctuation-p (c)
  "T if c is a punctuation character."
  (find c "`~!@#$%^&*()-+=[]{}\|;:',./<>?\"" :test #'char=))

(defun hex-char-p (c)
  "T if c is a hexadecimal character."
  (digit-char-p c 16))

(defun stream-start ()
  "Match only the beginning of the input stream. Assert so FIND-RE can bail quickly!"
  (assert (zerop (file-position *match-stream*))))

(defun eof ()
  "Match the end of the input stream."
  (null (peek-char nil *match-stream* nil nil)))

(defun match (char)
  "Match a specific character."
  (compile nil `(lambda (c) (char= c ,char))))

(defun any-from (start end)
  "Generate a sequence of characters in a range."
  (compile nil `(lambda (c) (char<= ,start c ,end))))

(defun one-of (tests)
  "Match the next character with any test."
  (compile nil `(lambda (c)
                  (dolist (test ',tests)
                    (when (funcall test c)
                      (return t))))))

(defun none-of (tests)
  "Match the next character wtih none of the tests."
  (compile nil `(lambda (c)
                  (dolist (test ',tests t)
                    (when (funcall test c)
                      (return nil))))))

(defun many (expr)
  "Match an expression zero or more times."
  (let ((parse-state (gensym "parse-state")))
    `(,parse-state (when ,expr (go ,parse-state)))))

(defun many1 (expr)
  "Match an expression one or more times."
  `((if ,expr (tagbody ,@(many expr)) (return))))

(defun boundary (start end)
  "Match zero or more characters between two boundaries."
  (let ((parse-state (gensym "parse-state")))
    `(if (let ((c (peek-char nil *match-stream* nil nil)))
           (and c (char= c ,start)))
         (tagbody
          ,parse-state
          (let ((c (read-char *match-stream* nil nil)))
            (case c
              (,end t)
              (nil (return nil))
              (otherwise
               (go ,parse-state)))))
       (return nil))))

(defun group (body)
  "Execute a sub-pattern and group the matched characters."
  (let ((group (gensym "group")))
    `(let ((,group (list (file-position *match-stream*))))
       (push ,group *match-groups*)
       (tagbody ,@body)
       (rplacd ,group (list (file-position *match-stream*))))))

(defparser re-parser
  ((start re) $1)

  ;; pattern combinator
  ((re pattern re) `(,@$1 ,@$2))
  ((re end) `(,$1))

  ;; end of the pattern
  ((end :eof) `(return (eof)))
  ((end) `(return t))
  
  ;; grouped or simple patterns
  ((pattern group) $1)
  ((pattern simple) $1)

  ;; patterns allowed in group
  ((sub-pattern pattern sub-pattern) `(,@$1 ,@$2))
  ((sub-pattern :end-group) `())

  ;; captured patterns
  ((group :group sub-pattern) `(,(group $2)))

  ;; non-modifiable patterns
  ((simple :boundary)
   `(,(apply #'boundary $1)))

  ;; optional and repeating patterns
  ((simple expr :maybe) (list $1))
  ((simple expr :many) (many $1))
  ((simple expr :many1) (many1 $1))
  ((simple expr :thru) (many $1))
  ((simple expr)
   `((unless ,$1 (return nil))))

  ;; named character sets
  ((expr :satisfy) `(satisfy ,$1))
  ((expr :unsatisfy) `(unsatisfy ,$1))

  ;; single characters
  ((expr :char) `(satisfy ,(match $1)))
  ((expr :tick) `(satisfy ,(match #\^)))
  ((expr :any) `(satisfy #'identity))

  ;; character sets
  ((expr :set tests) `(satisfy ,(one-of $2)))
  ((expr :set :tick tests) `(satisfy ,(none-of $3)))

  ;; unknown pattern token
  ((expr :error)
   (error "Illegal pattern"))

  ;; satisfying predicates
  ((tests test tests) `(,$1 ,@$2))
  ((tests :end-set) `())

  ;; single predicates
  ((test :char :thru :char) (any-from $1 $3))
  ((test :any) (match #\.))
  ((test :group) (match #\())
  ((test :end-group) (match #\)))
  ((test :maybe) (match #\?))
  ((test :many) (match #\*))
  ((test :many1) (match #\+))
  ((test :char) (match $1))
  ((test :satisfy) $1))

(defun compile-re (pattern)
  "Create a regular expression pattern match."
  (with-input-from-string (s pattern)
    (flet ((next-token ()
             (let ((c (read-char s nil nil)))
               (when c
                 (case c
                   (#\%
                    (let ((c (read-char s)))
                      (case c
                        (#\s (values :satisfy #'space-p))
                        (#\S (values :unsatisfy #'space-p))
                        (#\n (values :satisfy #'newline-p))
                        (#\N (values :unsatisfy #'newline-p))
                        (#\a (values :satisfy #'both-case-p))
                        (#\A (values :unsatisfy #'both-case-p))
                        (#\l (values :satisfy #'lower-case-p))
                        (#\L (values :unsatisfy #'lower-case-p))
                        (#\u (values :satisfy #'lower-case-p))
                        (#\U (values :unsatisfy #'lower-case-p))
                        (#\p (values :satisfy #'punctuation-p))
                        (#\P (values :unsatisfy #'punctuation-p))
                        (#\w (values :satisfy #'alphanumericp))
                        (#\W (values :unsatisfy #'alphanumericp))
                        (#\d (values :satisfy #'digit-char-p))
                        (#\D (values :unsatisfy #'digit-char-p))
                        (#\x (values :satisfy #'hex-char-p))
                        (#\X (values :unsatisfy #'hex-char-p))
                        (#\z (values :char #\null))
                        (#\b (let ((b1 (read-char s))
                                   (b2 (read-char s)))
                               (values :boundary (list b1 b2))))
                        (otherwise
                         (values :char c)))))
                   (#\$ (if (null (peek-char nil s nil nil))
                            :eof
                          (values :char c)))
                   (#\. :any)
                   (#\^ :tick)
                   (#\( :group)
                   (#\) :end-group)
                   (#\[ :set)
                   (#\] :end-set)
                   (#\? :maybe)
                   (#\* :many)
                   (#\+ :many1)
                   (#\- :thru)
                   (otherwise
                    (values :char c)))))))
      (let ((match-start-p (char= (peek-char nil s nil #\null) #\^)))
        (when match-start-p
          (read-char s))
        (let ((expr (re-parser #'next-token)))
          (make-instance 're
                         :pattern pattern
                         :match-start match-start-p
                         :expression (compile nil `(lambda () (prog () ,@expr)))))))))

(defmacro with-re ((re pattern) &body body)
  "Compile pattern if it's not a RE object and execute body."
  (let ((p (gensym)))
    `(let ((,p ,pattern))
       (let ((,re (if (eq (type-of ,p) 're)
                      ,p
                    (compile-re ,p))))
         (progn ,@body)))))

(defun match-re (pattern s &key (start 0) end)
  "Test a pattern re against a string."
  (with-re (re pattern)
    (let ((*match-stream* (make-string-input-stream s start end))
          (*match-groups*))
      (when (and (or (zerop start)
                     (null (re-match-start-p re)))
                 (funcall (re-expression re)))
        (let ((end (file-position *match-stream*)))
          (make-instance 're-match
                         :start-pos start
                         :end-pos end
                         :groups (capture-groups s)
                         :match (subseq s start end)))))))

(defun find-re (pattern s &key (start 0) (end (length s)) all)
  "Find a regexp pattern match somewhere in a string."
  (with-re (re pattern)
    (if (not all)
        (loop :for i :from start :below end
              :while (or (zerop i) (not (re-match-start-p re)))
              :do (let ((match (match-re re s :start i :end end)))
                    (when match
                      (return match))))
      (loop :with i := start
            :for match := (find-re re s :start i :end end)
            :while match
            :collect (prog1
                         match
                       (setf i (match-pos-end match)))))))

(defun split-re (pattern s &key (start 0) (end (length s)) all coalesce-seps)
  "Split a string into one or more strings by pattern match."
  (with-re (re pattern)
    (let ((ms (find-re re s :start start :end end :all all)))
      (if (null ms)
          s
        (if (not all)
            (values (subseq s start (match-pos-start ms))
                    (subseq s (match-pos-end ms) end))
          (loop :with pos := start
                :for m :in ms
                :for split := (subseq s pos (match-pos-start m))
                :do (setf pos (match-pos-end m))
                :when (or (null coalesce-seps) (plusp (length split)))
                :collect split))))))

(defun replace-re (pattern with s &key (start 0) (end (length s)) all)
  "Replace patterns found within a string with a new value."
  (with-re (re pattern)
    (let ((matches (find-re re s :start start :end end :all all)))
      (with-output-to-string (rep nil :element-type 'character)
        (loop :with pos := 0
              :for match :in (when matches (if all matches (list matches)))
              :finally (princ (subseq s pos) rep)
              :do (progn
                    (princ (subseq s pos (match-pos-start match)) rep)
                    (princ (if (functionp with) (funcall with match) with) rep)
                    (setf pos (match-pos-end match))))))))
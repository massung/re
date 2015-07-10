;;;; Regular Expression Pattern Matching for Common Lisp
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

(defpackage :re
  (:use :cl)
  (:export
   #:re
   #:re-match

   ;; macros
   #:with-re
   #:with-re-match

   ;; interface
   #:compile-re
   #:match-re
   #:find-re
   #:split-re
   #:replace-re

   ;; match readers
   #:match-string
   #:match-groups
   #:match-pos-start
   #:match-pos-end))

(in-package :re)

;;; ----------------------------------------------------

(defclass re ()
  ((pattern   :initarg :pattern    :reader re-pattern)
   (expr      :initarg :expression :reader re-expression))
  (:documentation "Regular expression."))

;;; ----------------------------------------------------

(defclass re-match ()
  ((match     :initarg :match      :reader match-string)
   (groups    :initarg :groups     :reader match-groups)
   (start-pos :initarg :start-pos  :reader match-pos-start)
   (end-pos   :initarg :end-pos    :reader match-pos-end))
  (:documentation "Matched pattern."))

;;; ----------------------------------------------------

(defmethod print-object ((re re) s)
  "Output a regular expression to a stream."
  (print-unreadable-object (re s :type t)
    (format s "~s" (re-pattern re))))

;;; ----------------------------------------------------

(defmethod print-object ((match re-match) s)
  "Output a regular expression match to a stream."
  (print-unreadable-object (match s :type t)
    (format s "~s" (match-string match))))

;;; ----------------------------------------------------

(defmethod make-load-form ((re re) &optional env)
  "Tell the system how to save and load a regular expression to a FASL."
  `(compile-re ,(re-pattern re)))

;;; ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((dispatch-re (s c n)
           (declare (ignorable c n))
           (let ((re (with-output-to-string (re)
                       (loop for c = (read-char s t nil t) :do
                         (case c
                           (#\/ (return))
                           (#\\ (let ((c (read-char s t nil t)))
                                  (princ c re)))
                           (otherwise
                            (princ c re)))))))
             (compile-re re))))
    (set-dispatch-macro-character #\# #\/ #'dispatch-re)))

;;; ----------------------------------------------------

(defun tab-p (c)
  "T if c is a tab character."
  (char= c #\tab))

;;; ----------------------------------------------------

(defun space-p (c)
  "T if c is a whitespace character."
  (or (char= c #\tab)
      (char= c #\space)))

;;; ----------------------------------------------------

(defun newline-p (c)
  "T if c is a newline character."
  (or (char= c #\return)
      (char= c #\linefeed)))

;;; ----------------------------------------------------

(defun word-char-p (c)
  "T if is alphanumeric or an underscore."
  (or (alphanumericp c) (char= c #\_)))

;;; ----------------------------------------------------

(defun punctuation-p (c)
  "T if c is a punctuation character."
  (find c "`~!@#$%^&*()-+=[]{}\|;:',./<>?\"" :test #'char=))

;;; ----------------------------------------------------

(defun hex-char-p (c)
  "T if c is a hexadecimal character."
  (digit-char-p c 16))

;;; ----------------------------------------------------

(defun take-char (stream &optional c)
  "Optionally read the next character if it matches."
  (let ((v (peek-char nil stream nil)))
    (if (null v)
        (error "Unexpected end of re pattern")
      (when (or (null c) (eql v c))
        (read-char stream)))))

;;; ----------------------------------------------------

(defun escape (stream)
  "Return the test and predicate for an escaped character."
  (let ((c (take-char stream)))
    (case c

      ;; user-defined predicate
      (#\: (let ((sym (with-output-to-string (s)
                        (do ((c (take-char stream)
                                (take-char stream)))
                            ((eql c #\:))
                          (write-char c s)))))
             (list :one-of (symbol-function (read-from-string sym)))))

      ;; boundary test
      (#\b (let ((b1 (take-char stream))
                 (b2 (take-char stream)))
             (list :bounds `((:char ,b1)) `((:char ,b2)))))
      
      ;; named inclusive sets
      (#\s (list :one-of #'space-p))
      (#\t (list :one-of #'tab-p))
      (#\n (list :one-of #'newline-p))
      (#\a (list :one-of #'alpha-char-p))
      (#\l (list :one-of #'lower-case-p))
      (#\u (list :one-of #'upper-case-p))
      (#\d (list :one-of #'digit-char-p))
      (#\w (list :one-of #'word-char-p))
      (#\x (list :one-of #'hex-char-p))
      (#\p (list :one-of #'punctuation-p))
      
      ;; named exclusive sets
      (#\S (list :none-of #'space-p))
      (#\T (list :none-of #'tab-p))
      (#\N (list :none-of #'newline-p))
      (#\A (list :none-of #'alpha-char-p))
      (#\L (list :none-of #'lower-case-p))
      (#\U (list :none-of #'upper-case-p))
      (#\D (list :none-of #'digit-char-p))
      (#\W (list :none-of #'word-char-p))
      (#\X (list :none-of #'hex-char-p))
      (#\P (list :none-of #'punctuation-p))
      
      ;; just an escaped character
      (otherwise (list :char c)))))

;;; ----------------------------------------------------

(defun charset (stream)
  "Parse a character set."
  (let (ps)
    (do ((c (take-char stream)
            (take-char stream)))
        ((eql c #\])
         (compile nil `(lambda ($_)
                         (declare (optimize (safety 0) (speed 3) (debug 0)) (character $_))
                         (or ,@ps))))
      (let ((p (if (eql c #\%)
                   (destructuring-bind (test predicate &rest extra)
                       (escape stream)
                     (declare (ignore extra))
                     (case test
                       
                       ;; special case
                       (:bounds (error "Cannot have %b in a character set"))
                       
                       ;; inclusive and exclusive predicates
                       (:one-of  `(funcall ,predicate $_))
                       (:none-of `(not (funcall ,predicate $_)))
                       
                       ;; simple character, no ranging allowed
                       (:char `(char= ,predicate $_))))
                 
                 ;; just a simple character, check for a character range
                 (let ((v (take-char stream #\-)))
                   (cond ((null v) `(char= $_ ,c))
                         
                         ;; if - is followed by end, unread and add a character predicate
                         ((eql (peek-char nil stream) #\])
                          (prog1 `(char= $_ ,c)
                            (unread-char v stream)))
                         
                         ;; a valid range of characters
                         (t `(char<= ,c $_ ,(read-char stream))))))))
             
        ;; add the predicate to the list (order doesn't matter)
        (push p ps)))))

;;; ----------------------------------------------------

(defun rep (rep token)
  "Repeat (or maybe) a token."
  (if (member (first token) '(nil :? :+ :* :-))
      (error "Illegal re pattern")
    (list rep (list token))))

;;; ----------------------------------------------------

(defun parse-re (stream &optional recursive-p)
  "Parse a regular expression pattern into a token form."
  (loop with stack = nil
        with tokens = nil
        
        ;; check every character
        for c = (peek-char nil stream nil)

        ;; stop at the end of the stream or at the end of a group's last branch
        until (or (null c) (and recursive-p (eql c #\))))

        ;; parse the next token
        for token = (case (read-char stream)
                      
                      ;; escape characters
                      (#\% (escape stream))
                      
                      ;; conditional pattern
                      (#\| (progn (push tokens stack)
                             (setf tokens nil)))
                      
                      ;; character sets
                      (#\[ (let ((exclusive-p (take-char stream #\^)))
                             (list (if exclusive-p :none-of :one-of) (charset stream))))
                      
                      ;; reserved tokens
                      (#\] (error "Unexpected ']' in re pattern"))
                      (#\) (error "Unexpected ')' in re pattern"))
                      
                      ;; push a new group onto the stack
                      (#\( (let ((no-capture-p (take-char stream #\?))

                                 ;; parse what's inside the group recursively
                                 (group (parse-re stream t)))

                             ;; perform a sanity check
                             (unless (eql (read-char stream nil nil) #\))
                               (error "Missing ')' in re pattern"))

                             ;; return a capture or just a list of tokens
                             (if no-capture-p
                                 (list :ignore group)
                               (list :capture (append '((:push)) group '((:pop)))))))
                      
                      ;; pattern boundaries
                      (#\^ (if (= (file-position stream) 1) '(:start) (list :char c)))
                      (#\$ (if (null (peek-char nil stream nil)) '(:end) (list :char c)))
                      
                      ;; satisfy any character
                      (#\. (list :any))
                      
                      ;; optional expressions
                      (#\? (rep :? (pop tokens)))
                      
                      ;; repeat expressions
                      (#\+ (rep :+ (pop tokens)))
                      (#\* (rep :* (pop tokens)))
                      (#\- (rep :- (pop tokens)))
                      
                      ;; simply match an exact character
                      (otherwise (list :char c)))

        ;; collect them all (as a stack)
        when token do (push token tokens)
        
        ;; return all the tokens parsed
        finally (return (if (null tokens)
                            (error "Empty~@[ '|' in~] re pattern" stack)

                          ;; traverse the stack and create all the choices
                          (loop for choice in stack
                                do (setf tokens (list (list :or choice tokens)))
                                finally (return (reverse (if recursive-p
                                                             tokens
                                                           (cons '(:match) tokens)))))))))

;;; ----------------------------------------------------

(defun compile-re (pattern)
  "Create a regular expression from a pattern string."
  (let ((bs ())
        (re (make-array 16 :adjustable t :fill-pointer 0)))
    (labels ((compile-branch (symbol)
               (push (cons symbol (length re)) bs))
             (compile-any ()
               (vector-push-extend '(:any) re))
             (compile-split (b1 b2)
               (vector-push-extend (list :split b1 b2) re))
             (compile-jump (b)
               (vector-push-extend (list :jump b) re))
             (compile-tokens (tokens)
               (dolist (token tokens)
                 (destructuring-bind (tok &optional x y)
                     token
                   (case tok
                     ((:match :start :end :any :char :one-of :none-of :push :pop)
                      (vector-push-extend token re))

                     ;; choose branch
                     (:or (let ((this (gensym))
                                (else (gensym))
                                (done (gensym)))
                            (compile-split this else)
                            (compile-branch this)
                            (compile-tokens x)
                            (compile-jump done)
                            (compile-branch else)
                            (compile-tokens y)
                            (compile-branch done)))

                     ;; Lua boundary
                     (:bounds (let ((try (gensym))
                                    (done (gensym))
                                    (again (gensym)))
                                (compile-tokens x)
                                (compile-branch again)
                                (compile-split done try)
                                (compile-branch try)
                                (compile-any)
                                (compile-jump again)
                                (compile-branch done)
                                (compile-tokens y)))
                                
                     ;; capture and ignore groups
                     ((:capture :ignore) (compile-tokens x))

                     ;; maybe match
                     (:? (let ((this (gensym))
                               (else (gensym)))
                           (compile-split this else)
                           (compile-branch this)
                           (compile-tokens x)
                           (compile-branch else)))

                     ;; one or more times
                     (:+ (let ((rep (gensym))
                               (done (gensym)))
                           (compile-branch rep)
                           (compile-tokens x)
                           (compile-split rep done)
                           (compile-branch done)))

                     ;; zero or more times (greedy)
                     (:* (let ((try (gensym))
                               (done (gensym))
                               (again (gensym)))
                           (compile-branch again)
                           (compile-split try done)
                           (compile-branch try)
                           (compile-tokens x)
                           (compile-jump again)
                           (compile-branch done)))

                     ;; zero or more times (lazy)
                     (:- (let ((try (gensym))
                               (done (gensym))
                               (again (gensym)))
                           (compile-branch again)
                           (compile-split done try)
                           (compile-branch try)
                           (compile-tokens x)
                           (compile-jump again)
                           (compile-branch done))))))))

      ;; parse the pattern and compile all the expressions in it
      (with-input-from-string (s pattern)
        (compile-tokens (parse-re s))

        ;; resolve the labels created in the compiled expression
        (loop for token across re do (symbol-macrolet ((b1 (second token)) (b2 (third token)))
                                       (case (first token)
                                         (:jump  (setf b1 (cdr (assoc b1 bs))))
                                         (:split (setf b1 (cdr (assoc b1 bs))
                                                       b2 (cdr (assoc b2 bs))))))))

      ;; create the regular expression object
      (make-instance 're :pattern pattern :expression re))))

;;; ----------------------------------------------------

(defstruct (re-thread (:constructor make-re-thread (pc sp groups stack))) pc sp groups stack)

;;; ----------------------------------------------------

(defun run (expression s &optional (pc 0) (start 0) (end (length s)) (offset 0))
  "Execute a regular expression program."
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (do ((threads (list (make-re-thread pc (+ start offset) nil nil))))
      ((null threads))

    ;; pop the next thread and run it until a match or failure
    (with-slots (pc sp groups stack)
        (pop threads)

      ;; step until the thread fails or matches
      (do ()
          ((not (destructuring-bind (op &optional x y)
                    (aref expression pc)
                  (incf pc)
                  (case op
                    
                    ;; start and end boundaries
                    (:start   (= sp start))
                    (:end     (= sp end))
                    
                    ;; match any character
                    (:any     (when (< sp end)
                                (incf sp)))
                    
                    ;; match an exact character
                    (:char    (when (and (< sp end) (char= (char s sp) x))
                                (incf sp)))
                    
                    ;; match a predicate function
                    (:one-of  (when (and (< sp end) (funcall x (char s sp)))
                                (incf sp)))
                    
                    ;; fail to match a predicate function
                    (:none-of (when (and (< sp end) (not (funcall x (char s sp))))
                                (incf sp)))
                    
                    ;; push a capture group
                    (:push    (let ((capture (list sp)))
                                (push capture stack)
                                (push capture groups)))
                    
                    ;; pop a capture group
                    (:pop     (rplacd (pop stack) (list sp)))
                    
                    ;; jump to an instruction
                    (:jump    (setf pc x))
                    
                    ;; fork a thread
                    (:split   (let ((branch (make-re-thread y sp groups stack)))
                                (push branch threads)
                                (setf pc x)))
                    
                    ;; successfully matched, create and return
                    (:match   (return-from run
                                (let ((cs (let (cs)
                                            (do ((g (pop groups)
                                                    (pop groups)))
                                                ((null g) cs)
                                              (push (subseq s (first g) (second g)) cs)))))
                                  (make-instance 're-match
                                                 :start-pos (+ start offset)
                                                 :end-pos sp
                                                 :groups cs
                                                 :match (subseq s (+ start offset) sp)))))))))))))

;;; ----------------------------------------------------

(defmacro with-re ((re pattern) &body body)
  "Compile pattern if it's not a RE object and execute body."
  (let ((p (gensym)))
    `(let ((,p ,pattern))
       (let ((,re (if (eq (type-of ,p) 're)
                      ,p
                    (compile-re ,p))))
         (progn ,@body)))))

;;; ----------------------------------------------------

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
        ($_ (intern "$_" *package*))
        ($* (intern "$*" *package*)))
    `(let ((,match ,match-expr))
       (if (null ,match)
           ,no-match
         (destructuring-bind (,$$ &optional ,$1 ,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9 &rest ,$_)
             (cons (match-string ,match) (match-groups ,match))
           (declare (ignorable ,$$ ,$1 ,$2 ,$3 ,$4 ,$5 ,$6 ,$7 ,$8 ,$9 ,$_))
           (let ((,$* (match-groups ,match)))
             (declare (ignorable ,$*))
             (progn ,@body)))))))

;;; ----------------------------------------------------

(defun match-re (pattern s &key (start 0) (end (length s)) (offset 0) exact)
  "Test a pattern re against a string."
  (with-re (re pattern)
    (let ((m (run (re-expression re) s 0 start end offset)))
      (when m
        (and (or (null exact) (= (match-pos-end m) end)) m)))))

;;; ----------------------------------------------------

(defun find-re (pattern s &key (start 0) (end (length s)) (offset 0) all)
  "Find a regexp pattern match somewhere in a string. Run from an offset."
  (with-re (re pattern)
    (flet ((next-match (offset)
             (loop for i from offset below end
                   for m = (run (re-expression re) s 0 start end i)
                   when m
                   return m)))
      (if (not all)
          (next-match start)
        (loop for m = (next-match offset)
              while m
              collect (prog1 m
                        (setf offset (- (match-pos-end m) start))))))))

;;; ----------------------------------------------------

(defun split-re (pattern s &key (start 0) (end (length s)) (offset 0) all coalesce-seps)
  "Split a string into one or more strings by pattern match."
  (with-re (re pattern)
    (let ((ms (find-re re s :start start :end end :offset offset :all all)))
      (if (null ms)
          s
        (if (not all)
            (values (subseq s start (match-pos-start ms))
                    (subseq s (match-pos-end ms) end))
          (loop with pos = start
                for m in ms
                for split = (subseq s pos (match-pos-start m))
                do (setf pos (match-pos-end m))
                when (or (null coalesce-seps) (plusp (length split)))
                collect split))))))

;;; ----------------------------------------------------

(defun replace-re (pattern with s &key (start 0) (end (length s)) (offset 0) all)
  "Replace patterns found within a string with a new value."
  (with-re (re pattern)
    (let ((matches (find-re re s :start start :end end :offset offset :all all)))
      (with-output-to-string (rep nil :element-type 'character)
        (loop with pos = 0
              for match in (when matches (if all matches (list matches)))
              finally (princ (subseq s pos) rep)
              do (progn
                   (princ (subseq s pos (match-pos-start match)) rep)
                   (princ (if (functionp with) (funcall with match) with) rep)
                   (setf pos (match-pos-end match))))))))

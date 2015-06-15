;;;; Regular Expression Pattern Matching for LispWorks
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

(defclass re ()
  ((pattern   :initarg :pattern    :reader re-pattern)
   (expr      :initarg :expression :reader re-expression))
  (:documentation "Regular expression."))

(defclass re-match ()
  ((match     :initarg :match      :reader match-string)
   (groups    :initarg :groups     :reader match-groups)
   (start-pos :initarg :start-pos  :reader match-pos-start)
   (end-pos   :initarg :end-pos    :reader match-pos-end))
  (:documentation "Matched pattern."))

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
  `(compile-re ,(re-pattern re)))

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

(defun tab-p (c)
  "T if c is a tab character."
  (char= c #\tab))

(defun space-p (c)
  "T if c is a whitespace character."
  (or (char= c #\tab)
      (char= c #\space)))

(defun newline-p (c)
  "T if c is a newline character."
  (or (char= c #\return)
      (char= c #\linefeed)))

(defun word-char-p (c)
  "T if is alphanumeric or an underscore."
  (or (alphanumericp c) (char= c #\_)))

(defun punctuation-p (c)
  "T if c is a punctuation character."
  (find c "`~!@#$%^&*()-+=[]{}\|;:',./<>?\"" :test #'char=))

(defun hex-char-p (c)
  "T if c is a hexadecimal character."
  (digit-char-p c 16))

(defparser re-parser
  ((start re) $1)

  ;; regular expression
  ((re choose)
   `(,@$1 (:match)))

  ;; choose between multiple expression lists
  ((choose exprs :or choose)
   `((:split 0 ,(1+ (length $1))) ,@$1 (:jump ,(length $3)) ,@$3))
  ((choose exprs)
   `(,@$1))

  ;; top-level expressions
  ((exprs expr exprs)
   `(,@$1 ,@$2))
  ((exprs))

  ;; simple expressions
  ((expr simple) $1)
  ((expr :error)
   (error "Illegal re pattern."))

  ;; zero or one time
  ((simple inst :?)
   `((:split 0 ,(length $1)) ,@$1))

  ;; one or more times (greedy)
  ((simple inst :+)
   `(,@$1 (:split ,(- (1+ (length $1))) 0)))

  ;; one or more times (lazy)
  ((simple inst :-)
   `(,@$1 (:split 0 ,(- (1+ (length $1))))))

  ;; zero or more times
  ((simple inst :*)
   (let ((n (length $1)))
     `((:split 0 ,(1+ n)) ,@$1 (:jump ,(- (+ n 2))))))

  ;; just the instruction
  ((simple inst) $1)

  ;; bytecode instruction
  ((inst :^)
   `((:start)))
  ((inst :$)
   `((:end)))
  ((inst :char)
   `((:char ,$1)))
  ((inst :any)
   `((:any ,$1)))
  ((inst :none)
   `((:none ,$1)))
  ((inst :satisfy)
   `((:satisfy ,$1)))
  ((inst :unsatisfy)
   `((:unsatisfy ,$1)))
  ((inst :inclusive-set)
   `((:satisfy ,$1)))
  ((inst :exclusive-set)
   `((:unsatisfy ,$1)))

  ;; bounded characters
  ((inst :boundary)
   (destructuring-bind (b1 b2)
       $1
     `((:char ,b1) (:split 2 0) (:satisfy identity) (:jump -3) (:char ,b2))))

  ;; group captures
  ((inst :push :? choose :pop) $3)
  ((inst :push choose :pop)
   `((:push) ,@$2 (:pop))))

(defparser set-parser
  ((start set) $1)

  ;; parse all the predicates
  ((set predicate set) `(,$1 ,@$2))
  ((set))

  ;; tests
  ((predicate :char :- :char) `(char<= ,$1 $_ ,$3))
  ((predicate :char) `(char= ,$1 $_))
  ((predicate :satisfy) `(funcall ,$1 $_))
  ((predicate :unsatisfy) `(not (funcall ,$1 $_)))

  ;; error in pattern
  ((predicate :error)
   (error "Illegal re pattern.")))

(defun escape (c)
  "Return the test and predicate for an escaped character."
  (case c
    (#\s (values :satisfy #'space-p))
    (#\S (values :unsatisfy #'space-p))
    (#\t (values :satisfy #'tab-p))
    (#\T (values :unsatisfy #'tab-p))
    (#\n (values :satisfy #'newline-p))
    (#\N (values :unsatisfy #'newline-p))
    (#\a (values :satisfy #'alpha-char-p))
    (#\A (values :unsatisfy #'alpha-char-p))
    (#\l (values :satisfy #'lower-case-p))
    (#\L (values :unsatisfy #'lower-case-p))
    (#\u (values :satisfy #'upper-case-p))
    (#\U (values :unsatisfy #'upper-case-p))
    (#\d (values :satisfy #'digit-char-p))
    (#\D (values :unsatisfy #'digit-char-p))
    (#\w (values :satisfy #'word-char-p))
    (#\W (values :unsatisfy #'word-char-p))
    (#\x (values :satisfy #'hex-char-p))
    (#\X (values :unsatisfy #'hex-char-p))
    (#\p (values :satisfy #'punctuation-p))
    (#\P (values :unsatisfy #'punctuation-p))
    
    ;; just an escaped character
    (otherwise (values :char c))))

(defun compile-set (s)
  "Create a single satisfy predicate for a character set."
  (let ((exclusive-p (equal (peek-char nil s) #\^)))

    ;; if an exclusive set, skip the first character
    (when exclusive-p (read-char s))

    ;; get the position of the first character
    (let ((pos (file-position s)))

      ;; parse all the caracters in the set
      (flet ((next-token ()
               (let ((c (read-char s)))
                 (case c
                   
                   ;; end of set
                   (#\] nil)

                   ;; escaped predicate or character
                   (#\% (escape (read-char s)))
                   
                   ;; range character (or just a character if at start or end)
                   (#\- (if (or (equal (file-position s) (1+ pos))
                                (equal (peek-char nil s) #\]))
                            (values :char c)
                          (values :- c)))

                   ;; just a character
                   (otherwise (values :char c))))))
        
        ;; parse and build a predicate function
        (values (if exclusive-p :exclusive-set :inclusive-set)
                (compile nil `(lambda ($_)
                                (or ,@(set-parser #'next-token)))))))))

(defun compile-re (pattern)
  "Create a regular expression from a pattern string."
  (let ((n (length pattern)))
    (with-input-from-string (s pattern)
      (flet ((next-token ()
               (when-let (c (read-char s nil nil))
                 (case c
                   
                   ;; escaped predicate or character
                   (#\% (let ((c (read-char s)))
                          (if (char= c #\b)
                              (let ((b1 (read-char s))
                                    (b2 (read-char s)))
                                (values :boundary (list b1 b2)))
                            (escape c))))
                   
                   ;; compile a character set
                   (#\[ (compile-set s))

                   ;; conditional
                   (#\| :or)

                   ;; push a group capture
                   (#\( :push)
                   (#\) :pop)
                 
                   ;; pattern boundaries
                   (#\^ (if (= (file-position s) 1) :^ (values :char c)))
                   (#\$ (if (= (file-position s) n) :$ (values :char c)))

                   ;; satisfy any character
                   (#\. (values :satisfy 'identity))
                   
                   ;; optional and repeating
                   (#\? :?)
                   (#\+ :+)
                   (#\- :-)
                   (#\* :*)
                   
                   ;; reserved characters that will be syntax errors
                   ((#\]) (error "Illegal re pattern."))
                 
                   ;; just a simple character
                   (otherwise (values :char c))))))
        
        ;; create an array of all the instructions parsed
        (loop with re = (make-array 0 :adjustable t :fill-pointer t)
              with labels = ()
              
              ;; loop over each instruction
              for i in (re-parser #'next-token)
              
              ;; pull labels into a table, push instructions
              do (if (symbolp i)
                     (push (list i (length re)) labels)
                   (vector-push-extend i re))
              
              ;; 2nd pass fix labels
              finally (return (make-instance 're :pattern pattern :expression re)))))))

(defstruct (re-thread (:constructor make-re-thread (pc sp groups stack))) pc sp groups stack)

(defun run (expression s &optional (pc 0) (start 0) (end (length s)) (offset 0))
  "Execute a regular expression program."
  (loop with threads = (list (make-re-thread pc (+ start offset) nil nil))
        while threads

        ;; pop the next thread and run it
        do (with-slots (pc sp groups stack)
               (pop threads)

             ;; step until the thread fails or matches
             (loop while (destructuring-bind (op &optional x y)
                             (aref expression pc)
                           (incf pc)
                           (case op
                             
                             ;; start and end boundaries
                             (:start     (= sp start))
                             (:end       (= sp end))
                             
                             ;; match an exact character
                             (:char      (when (and (< sp end) (char= (char s sp) x))
                                           (incf sp)))
                             
                             ;; match a predicate function
                             (:satisfy   (when (and (< sp end) (funcall x (char s sp)))
                                           (incf sp)))
                             
                             ;; fail to match a predicate function
                             (:unsatisfy (when (and (< sp end) (not (funcall x (char s sp))))
                                           (incf sp)))
                             
                             ;; push a capture group
                             (:push      (let ((capture (list sp)))
                                           (push capture stack)
                                           (push capture groups)))
                             
                             ;; pop a capture group
                             (:pop       (rplacd (pop stack) (list sp)))
                             
                             ;; jump to an instruction
                             (:jump      (incf pc x))
                             
                             ;; fork a thread
                             (:split     (let ((branch (make-re-thread (+ pc y) sp groups stack)))
                                           (push branch threads)
                                           (incf pc x)))
                             
                             ;; successfully matched, create and return
                             (:match     (return-from run
                                           (let ((cs (let (cs)
                                                       (do ((g (pop groups)
                                                               (pop groups)))
                                                           ((null g) cs)
                                                         (push (subseq s (first g) (second g)) cs)))))
                                             (make-instance 're-match
                                                            :start-pos (+ start offset)
                                                            :end-pos sp
                                                            :groups cs
                                                            :match (subseq s (+ start offset) sp)))))))))))

(defmacro with-re ((re pattern) &body body)
  "Compile pattern if it's not a RE object and execute body."
  (let ((p (gensym)))
    `(let ((,p ,pattern))
       (let ((,re (if (eq (type-of ,p) 're)
                      ,p
                    (compile-re ,p))))
         (progn ,@body)))))

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

(defun match-re (pattern s &key (start 0) (end (length s)) (offset 0) exact)
  "Test a pattern re against a string."
  (with-re (re pattern)
    (when-let (m (run (re-expression re) s 0 start end offset))
      (and (or (null exact) (= (match-pos-end m) end)) m))))

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

;;;; Regular Expressions for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
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
  (:use :cl :parse)
  (:export
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
   #:match-captures
   #:match-pos-start
   #:match-pos-end
   
   ;; capturing groups retrieval
   #:match-capture-at-index
   #:match-captures-by-name
   #:match-capture-by-name
   #:match-capture-by-name-at-index
   #:match-has-capture-of-name
   
   ;; convenience functions
   #:match-extract-data
   
   ;; capturing group definition
   #:re-capture
   #:re-capture-start-position
   #:re-capture-end-position
   #:re-capture-substring
   #:re-capture-name
   #:re-capture-has-name
   
   ;; regular expression configurations
   #:re-configuration
   #:re-configuration-permit-named-captures
   #:re-configuration-named-capture-marker
   #:re-configuration-named-capture-name-starter
   #:re-configuration-named-capture-name-ender
   #:re-configuration-permit-ranged-quantifiers
   #:*re-configuration*
   ))

(in-package :re)




(defclass re-configuration ()
  (
   ;; Options for NAMED CAPTURING GROUPS:
   (permit-named-captures
     :initarg       :permit-named-captures
     :initform      NIL
     :accessor      re-configuration-permit-named-captures
     :documentation "Recognize named captures at all?")
   (named-capture-marker
     :initarg       :named-capture-marker
     :initform      #\!
     :accessor      re-configuration-named-capture-marker
     :type          character
     :documentation "Character to introduce a named capture, that is,
                     distinguish it from an unnamed or uncaptured group.
                     Example: #\! in (!<...>).")
   (named-capture-name-starter
     :initarg       :named-capture-name-starter
     :initform      #\<
     :accessor      re-configuration-named-capture-name-starter
     :type          character
     :documentation "Character to designate the start of a named
                     capture's name portion.
                     Example: #\< in (!<NAME>).")
   (named-capture-name-ender
     :initarg       :named-capture-name-ender
     :initform      #\>
     :accessor      re-configuration-named-capture-name-ender
     :type          character
     :documentation "Character to designate the end of a named
                     capture's name portion.
                     Example: #\> in (!<NAME>).")
   
   ;; Options for RANGED QUANTIFIERS:
   (permit-ranged-quantifiers
     :initarg       :permit-ranged-quantifiers
     :initform      NIL
     :accessor      re-configuration-permit-ranged-quantifiers
     :documentation "Are ranged quantifiers of the type '{...}'
                     homologated?"))
  (:documentation "Bundles the options for the regular expression engine."))

;;; ----------------------------------------------------

(defparameter *re-configuration*
  (make-instance 're-configuration
    :permit-named-captures      T
    :named-capture-marker       #\!
    :named-capture-name-starter #\<
    :named-capture-name-ender   #\>
    :permit-ranged-quantifiers  T))

;;; ----------------------------------------------------

(defclass re-capture ()
  ((start-position
     :initarg       :start-position
     :initform      0
     :type          integer
     :reader        re-capture-start-position
     :documentation "The start position of this capturing group
                     in the input string.")
   (end-position
     :initarg       :end-position
     :initform      0
     :type          integer
     :reader        re-capture-end-position
     :documentation "The end position of this capturing group
                     in the input string.")
   (substring
     :initarg       :substring
     :initform      ""
     :type          string
     :reader        re-capture-substring
     :documentation "The substring demarcated by this capturing group.")
   (name
     :initarg       :name
     :initform      NIL
     :type          (or null string)
     :reader        re-capture-name
     :documentation "The optional name of the capturing group."))
  (:documentation "Represents a capturing group, either named
                   or unnamed."))

(defmethod print-object ((group re-capture) stream)
  (with-slots (start-position end-position substring name) group
    (format stream "#<re-capture start=~a, end=~a, substring=~s, name=~a>"
      start-position
      end-position
      substring
      name)))

(defun re-capture-has-name (capturing-group)
  "Checks whether the CAPTURING-GROUP has a name."
  (with-slots (name) capturing-group
    (not (null name))))

;;; ----------------------------------------------------

(defclass re ()
  ((pattern   :initarg :pattern    :reader re-pattern)
   (expr      :initarg :expression :reader re-expression))
  (:documentation "Regular expression."))

;;; ----------------------------------------------------

(defclass re-match ()
  ((match     :initarg :match      :reader match-string)
   (groups    :initarg :groups     :reader match-groups)
   ;; Vector of "re-capture" instances.
   (captures  :initarg :captures   :reader match-captures)
   ;; Hash-Table: group-name -> vector of group-indices.
   (name-map  :initarg :name-map   :reader match-name-map)
   (start-pos :initarg :start-pos  :reader match-pos-start)
   (end-pos   :initarg :end-pos    :reader match-pos-end))
  (:documentation "Matched pattern."))

;;; ----------------------------------------------------

;; Public.
(defun match-capture-at-index (match index &key (default NIL))
  "Returns the group of the MATCH at the given INDEX, or, if the
   INDEX is invalid, the DEFAULT value."
  (with-slots (captures) match
    (if (array-in-bounds-p captures index)
      (aref captures index)
      default)))

;;; ----------------------------------------------------

;; Private.
(defun get-captures-by-name (match group-name)
  "Returns a vector of re-capture instances representing those
   groups in the MATCH associated with the GROUP-NAME.
   The vector is empty if no such group could be detected."
  (with-slots (name-map captures) match
    (let ((group-indices (gethash group-name name-map)))
      (if group-indices
        (let ((captures-for-indices (make-array (length group-indices)
                                                :adjustable T
                                                :fill-pointer 0)))
          (loop
            for group-index      across group-indices
            for capture-at-index =      (aref captures group-index)
            do  (vector-push-extend capture-at-index captures-for-indices))
          captures-for-indices)
        (make-array 0)))))

;;; ----------------------------------------------------

;; Private.
(defun get-capture-by-name-at-index (match group-name index
                                     &optional (default-value NIL has-default-value))
  "Returns the INDEX-th ``re-capture'' with the GROUP-NAME in the MATCH.
   If either the GROUP-NAME is unknown or the INDEX is invalid, and a
   DEFAULT-VALUE is supplied, this substitute value will be returned,
   otherwise an error is triggered to signal a failure."
  (declare (ignorable default-value has-default-value))
  (let ((captures-by-name (get-captures-by-name match group-name)))
    (if (< index (length captures-by-name))
      (aref captures-by-name index)
      (if has-default-value
        default-value
        (error "Invalid index for capture: ~d." index)))))

;;; ----------------------------------------------------

;; Public.
(defun match-captures-by-name (match group-name)
  "Returns a vector containing all capturing groups of the MATCH
   designated by the GROUP-NAME."
  (get-captures-by-name match group-name))

;;; ----------------------------------------------------

;; Public.
(defun match-capture-by-name (match group-name
                              &key (selection :first)
                                   (default    NIL))
  "Returns the capturing group of the MATCH designated by the
   GROUP-NAME, if such exists, and the SELECTION.
   The SELECTION might appopriate one of these two values:
     - The symbol :first, to obtain the first group with such name.
     - The symbol :last,  to obtain the last  group with such name.
   If no capturing group with this label can be found: returns NIL.
   This function constitutes a convenient alernative to
   MATCH-CAPTURE-BY-NAME-AT-INDEX, aimed as simulating the
   condition of capturing group names being unique."
  (let* ((captures-by-name   (get-captures-by-name match group-name))
         (number-of-captures (length captures-by-name))
         (has-captures       (plusp  number-of-captures)))
    (if has-captures
      (case selection
        (:first    (aref captures-by-name 0))
        (:last     (aref captures-by-name (1- number-of-captures)))
        (otherwise (error "Invalid SELECTION: ~a." selection)))
      default)))

;;; ----------------------------------------------------

;; Public.
(defun match-capture-by-name-at-index (match group-name index)
  "Returns the INDEX-th capturing group of the MATCH designated by the
   GROUP-NAME, if such exists.
   If no capturing group with this label can be found: returns NIL.
   If the INDEX is invalid:                            returns NIL."
  (get-capture-by-name-at-index match group-name index))

;;; ----------------------------------------------------

;; Public.
(defun match-has-capture-of-name (match group-name)
  "Checks whether the MATCH encompasses at least one capture designated
   by the GROUP-NAME."
  (let ((captures-of-name (get-captures-by-name match group-name)))
    (and captures-of-name (plusp (length captures-of-name)))))

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

(define-parser re-parser
  "A regular expression is one or more expressions."
  (.let (ex (.many1 're-expr))
    (.opt ex (.let (otherwise (.do (.is :or) 're-parser))
               (.ret `((:or ,ex ,otherwise)))))))

;;; ----------------------------------------------------

(define-parser re-expr
  "A single character, set, or loop of expressions."
  (.let (e (.or 're-boundary
                're-bounds
                're-char
                're-set
                're-group))

    ;; check to see if there is a following iteration token
    (.opt e (.or (.do (.is :*)     (.ret (list :*     e)))
                 (.do (.is :-)     (.ret (list :-     e)))
                 (.do (.is :+)     (.ret (list :+     e)))
                 (.do (.is :?)     (.ret (list :?     e)))
                 
                 ;; The ranged quantifier additionally requires the
                 ;; portion enclosed between its markers "{" and "}"
                 ;; (here: stored in the variable "x").
                 (.let (x (.is :start-range))
                   (let ((range-string (read-range-from-stream x)))
                     (.do (.is :end-range)
                       (.ret (list :range e range-string)))))))))

;;; ----------------------------------------------------

(define-parser re-boundary
  "The start or end of a string."
  (.or (.do (.is :start) (.ret (list :start)))
       (.do (.is :end)   (.ret (list :end)))))

;;; ----------------------------------------------------

(define-parser re-bounds
  "Lua-style %b bounds."
  (.let (bs (.is :bounds))
    (.ret (cons :bounds bs))))

;;; ----------------------------------------------------

(define-parser re-char
  "Match any character, exact character, or predicate function."
  (.or (.do (.is :any) (.ret '(:any)))

       ;; predicates and exact characters
       (.let (p (.is :is))   (.ret (list :is   p)))
       (.let (c (.is :char)) (.ret (list :char c)))))

;;; ----------------------------------------------------

(define-parser re-set
  "Match from a set of characters."
  (.let* ((exclusive (.is :set))
          (predicates 're-set-chars))
    (flet ((any (c)
             (some #'(lambda (p) (funcall p c)) predicates)))
      (.ret (list (if exclusive :is-not :is) #'any)))))

;;; ----------------------------------------------------

(define-parser re-set-chars
  "Characters, character ranges, and named character sets."
  (.let (ps (.many1 (.or (.is :is)

                         ;; exact character
                         (.let (a 're-set-char)

                           ;; range of characters?
                           (.or (.let (z (.do (.is :-) 're-set-char))
                                  (.ret #'(lambda (c) (char<= a c z))))
                                (.ret #'(lambda (c) (char= c a))))))))

    ;; match the end of the set and return the predicates
    (.do (.is :end-set) (.ret ps))))

;;; ----------------------------------------------------

(define-parser re-set-char
  "Valid characters in a character set."
  (.or (.is :char)

       ;; special characters are aren't special in a set
       (.do (.is :any)         (.ret #\.))
       (.do (.is :or)          (.ret #\|))
       (.do (.is :*)           (.ret #\*))
       (.do (.is :-)           (.ret #\-))
       (.do (.is :+)           (.ret #\+))
       (.do (.is :?)           (.ret #\?))
       (.do (.is :group)       (.ret #\())
       (.do (.is :end-group)   (.ret #\)))
       (.do (.is :start-range) (.ret #\{))
       (.do (.is :end-range)   (.ret #\}))))

;;; ----------------------------------------------------

(defun read-captured-group-name (stream)
  "Read the name of a captured group from an input STREAM.
   The STREAM is expected to point to the captured group start character
   (usually #\<) and contain the captured group end character
   (usually \#>).
     The function returns a string between those two
   demarcating characters, for example: The stream encompassing
   \"<year>...\" will return the string \"year\".
     As a side effect, the STREAM will be modified to point to the next
   character after the captured group start character."
  (let ((token-value (make-array 0 :element-type 'character
                                   :adjustable    t
                                   :fill-pointer  0)))
    ;; Read the named captured group start character (usually #\<)
    ;; before iterating.
    ;; => Otherwise: Problems if start and end characters are equal.
    ;; 
    ;; NOTE:
    ;;   Currently the storage and indagation of the first stream
    ;;   character is otiose, as no alternative cases exist other than
    ;;   the start of a named captured group's name. But this might
    ;;   change in the future, for instance, to provide further
    ;;   configurations. In consectary, a checking is performed.
    (with-slots (named-capture-name-starter named-capture-name-ender)
                *re-configuration*
      (let ((name-section-id (read-char stream)))
        (unless (char-equal name-section-id named-capture-name-starter)
          (error "Expected the named captured group name start character ~s, but found ~s."
            named-capture-name-starter
            name-section-id)))
      (loop
        for   next-char = (read-char stream)
        while next-char
        ;; The next character will demarcate the end of the
        ;; captured group name (usually #\>)?
        ;; => Read it to clean the stream, but do not store it.
        do    (if (char-equal next-char named-capture-name-ender)
                (return)
                (vector-push-extend next-char token-value))))
    token-value))

;;; ----------------------------------------------------

(define-parser re-group
  "Match an optionally captured group."
  (.let (group-data (.is :group))
    (let ((group-type (first group-data)))
      (case group-type
        ;; Uncaptured group? => Remove the "?" designator from the stream.
        (:uncaptured (let ((stream (third group-data)))
                       (read-char stream)
                       (.let (xs 're-parser)
                         (.do (.is :end-group)
                           (.ret (list :ignore xs NIL))))))
        
        (:captured   (.let (xs 're-parser)
                       (.do (.is :end-group)
                         (.ret (list :capture xs NIL)))))
        
        ;; Named group? => Remove the "!" designator from the stream.
        (:named      (let ((stream (third group-data)))
                       (read-char stream)
                       (let ((group-name (read-captured-group-name stream)))
                         (.let (xs 're-parser)
                           (.do (.is :end-group)
                             (.ret (list :capture xs group-name)))))))
        
        (otherwise   (.fail "Invalid GROUP-TYPE: ~s." group-type))))))

;;; ----------------------------------------------------

;; Checks whether the next character in the STREAM equals the
;; CHARACTER-TO-MATCH-AGAINST. The STREAM will not be modified.
(defun next-character-equals (stream character-to-match-against)
  (eq (peek-char nil stream nil nil) character-to-match-against))

;;; ----------------------------------------------------

;; Reads a range as a string from the STREAM, expected in the pattern
;;   <any-characters>}
;; The returned string value is of the pattern
;;   MINIMUM,MAXIMUM
;; The STREAM will be modified by reading from it.
(defun read-range-from-stream (stream)
  (with-output-to-string (range-string)
    (loop
      while (not (next-character-equals stream #\}))
      for next-char = (read-char stream)
      do  (write-char next-char range-string)
          (when (next-character-equals stream #\})
            (return)))))

;;; ----------------------------------------------------

(defun is-not (pred)
  "Create a predicate that tests the inverse."
  #'(lambda (c) (not (funcall pred c))))

;;; ----------------------------------------------------

(defun escape (stream)
  "Return the test and predicate for an escaped character."
  (let ((c (read-char stream)))
    (case c

      ;; user-defined predicate
      (#\: (let ((sym (with-output-to-string (s)
                        (do ((c (read-char stream)
                                (read-char stream)))
                            ((eql c #\:))
                          (write-char c s)))))
             (values :is (read-from-string sym))))

      ;; boundary test
      (#\b (let ((b1 (read-char stream))
                 (b2 (read-char stream)))
             (values :bounds (list b1 b2))))

      ;; named inclusive sets
      (#\s (values :is #'space-p))
      (#\t (values :is #'tab-p))
      (#\n (values :is #'newline-p))
      (#\a (values :is #'alpha-char-p))
      (#\l (values :is #'lower-case-p))
      (#\u (values :is #'upper-case-p))
      (#\d (values :is #'digit-char-p))
      (#\w (values :is #'word-char-p))
      (#\x (values :is #'hex-char-p))
      (#\p (values :is #'punctuation-p))

      ;; named exclusive sets
      (#\S (values :is (is-not #'space-p)))
      (#\T (values :is (is-not #'tab-p)))
      (#\N (values :is (is-not #'newline-p)))
      (#\A (values :is (is-not #'alpha-char-p)))
      (#\L (values :is (is-not #'lower-case-p)))
      (#\U (values :is (is-not #'upper-case-p)))
      (#\D (values :is (is-not #'digit-char-p)))
      (#\W (values :is (is-not #'word-char-p)))
      (#\X (values :is (is-not #'hex-char-p)))
      (#\P (values :is (is-not #'punctuation-p)))

      ;; just a character
      (otherwise (values :char c)))))

;;; ----------------------------------------------------

(defun parse-re (pattern)
  "Parse a regular expression pattern."
  (with-input-from-string (stream pattern)
    (flet ((token-reader ()
             (let ((c (read-char stream nil nil)))
               (when c
                 (case c

                   ;; any character
                   (#\. :any)

                   ;; escaped characters
                   (#\% (escape stream))

                   ;; iterators
                   (#\* :*)
                   (#\+ :+)
                   (#\? :?)

                   ;; lazy iterator, or end of set
                   (#\- (if (eql (peek-char nil stream nil nil) #\])
                            (values :char #\-)
                          (values :-)))

                   ;; conditional
                   (#\| :or)

                   ;; groups
                   ;; Possible return values are:
                   ;;   captured   group: (:group (list :captured))
                   ;;   uncaptured group: (:group (list :uncaptured #\?))
                   ;;   named      group: (:group (list :named      "?<NAMESTRING>"))
                   (#\( (with-slots (permit-named-captures named-capture-marker)
                                    *re-configuration*
                          (cond
                            ;; Uncaptured group?
                            ((next-character-equals stream #\?)
                             (values :group (list :uncaptured  #\( stream)))
                            ;; Potential named captured group?
                            ((next-character-equals stream named-capture-marker)
                             (if permit-named-captures
                               (values :group (list :named    #\( stream))
                               (values :group (list :captured #\( stream))))
                            ;; Unnamed captured group?
                            (t
                             (values :group (list :captured    #\( stream))))))
                   
                   ;; ranged iterator
                   (#\{ (with-slots (permit-ranged-quantifiers)
                                    *re-configuration*
                          (if permit-ranged-quantifiers
                            (values :start-range stream)
                            (values :char        #\{))))
                   
                   ;; end of ranged iterator
                   (#\} (with-slots (permit-ranged-quantifiers)
                                    *re-configuration*
                          (if permit-ranged-quantifiers
                            (values :end-range)
                            (values :char      #\}))))
                   
                   ;; sets
                   (#\[ (if (eql (peek-char nil stream nil nil) #\^)
                            (values :set (read-char stream))
                          (values :set ())))

                   ;; group and set terminals
                   (#\) :end-group)
                   (#\] :end-set)

                   ;; start boundary
                   (#\^ (if (eql (file-position stream) 1)
                            :start
                          (values :char c)))

                   ;; end boundary
                   (#\$ (if (null (peek-char nil stream nil nil))
                            :end
                          (values :char c)))

                   ;; default to just an exact character match
                   (otherwise (values :char c)))))))

      ;; parse all the tokens in the regular expression
      (parse 're-parser #'token-reader))))

;;; ----------------------------------------------------

(defun is-bound-negative (bound)
  "Checks whether the number BOUND is negative.
   Returns:
     T   - if the BOUND is not NIL and is a negative number
     NIL - if the BOUND is NIL or if it is a positive number."
  (and bound (minusp bound)))

;;; ----------------------------------------------------

(defun check-range-minimum (minimum)
  (when (is-bound-negative minimum)
    (error "The range minimum must be >= 0: ~a is negative." minimum)))

;;; ----------------------------------------------------

(defun check-range-maximum (maximum)
  (when (is-bound-negative maximum)
    (error "The range maximum must be >= 0: ~a is negative." maximum)))

;;; ----------------------------------------------------

(defun check-range-bounds (range)
  "Checks whether the minimum and maximum of the RANGE are valid,
   that is, if both bounds are non-NIL, the minimum must be less than
   or equal to the maximum.
   Upon breach of this condition, an error is triggered."
  (destructuring-bind (minimum maximum) range
    (when (and minimum maximum)
      (when (> minimum maximum)
        (error "The range minimum must be less than or ~
                equal to the range maximum: ~a > ~a."
                minimum maximum)))))

;;; ----------------------------------------------------

(defun parse-range-minimum (range-string)
  "Parses a RANGE-STRING's minimum, returning two values:
     (1) the lower bound of the range, either an integer number or NIL
     (2) the rest of the RANGE-STRING following the minimum portion."
  (let* ((end-position-of-minimum (or (position #\, range-string)
                                      (length       range-string)))
         (minimum-as-string (subseq range-string 0 end-position-of-minimum))
         (rest-of-range     (subseq range-string end-position-of-minimum)))
    (setf minimum-as-string (string-trim "" minimum-as-string))
    (if (plusp (length minimum-as-string))
      (values (parse-integer minimum-as-string) rest-of-range)
      (values NIL                               rest-of-range))))

;;; ----------------------------------------------------

(defun parse-range-maximum (range-string minimum)
  "Parses the RANGE-STRING's maximum, returning two values:
     (1) the upper bound of the range, either an integer number or NIL
     (2) the rest of the RANGE-STRING following the maximum portion."
  (let ((maximum NIL))
    (setf range-string (string-trim " " range-string))
    (cond
      ;; Empty string without comma? => No maximum. => Reuse MINIMUM.
      ((zerop (length range-string))
        (setf maximum minimum))
      ;; Not an empty string?
      (T
        ;; Remove preceding comma (",").
        (setf range-string (string-trim " ," range-string))
        ;; Parse the maximum. It may be become NIL.
        (setf maximum (parse-integer range-string :junk-allowed T))))
    (values maximum range-string)))

;;; ----------------------------------------------------

(defun parse-range (range-string)
  "Parses the RANGE-STRING of the pattern
     [minimum, maximum]
   returning a list of two values:
     (minimum maximum)
   either of these items may be NIL."
  (let ((minimum NIL)
        (maximum NIL)
        (range   NIL))
    (setf range-string (string-trim "[]" range-string))
    ;; Extract the MINIMUM.
    (multiple-value-bind (extracted-minimum curtailed-range-string)
                         (parse-range-minimum range-string)
      (setf minimum      extracted-minimum)
      (setf range-string curtailed-range-string)
      (check-range-minimum minimum))
    ;; Extract the MAXIMUM.
    (multiple-value-bind (extracted-maximum curtailed-range-string)
                         (parse-range-maximum range-string minimum)
      (declare (ignore curtailed-range-string))
      (setf maximum      extracted-maximum)
      (check-range-maximum maximum))
    ;; Generate and check the RANGE.
    (setf range (list minimum maximum))
    (check-range-bounds range)
    range))

;;; ----------------------------------------------------

(defun are-iterations-in-range (range iterations)
  "Checks whether the ITERATIONS are in the RANGE, that is,
   the numeric value of ITERATIONS is greater than or equal to the
   RANGE minimum and less than or equal to the RANGE maximum.
   Any of the two bounds being NIL is construed as a ``infinity''."
  (let ((is-in-lower-bound NIL)
        (is-in-upper-bound NIL))
    (destructuring-bind (minimum maximum) range
      (if minimum
        (setf is-in-lower-bound (>= iterations minimum))
        (setf is-in-lower-bound T))
      (if maximum
        (setf is-in-upper-bound (<= iterations maximum))
        (setf is-in-upper-bound T)))
    (and is-in-lower-bound is-in-upper-bound)))

(defun get-range-check-function (range-string)
  (let ((range (parse-range range-string)))
    #'(lambda (counter)
        (are-iterations-in-range range counter))))

;;; ----------------------------------------------------

;; Parses a string of the arrangement
;;   "a_1, ..., a_i, ..., a_n"
;; with a_i: a positive integer number.
;; 
;; Notes:
;;   - Whitespaces are ignored.
;;   - The string must not be empty.
;;   - Only integer numbers >= 0 are valid.
;;   - Besides integer numbers >=0 (with optional positive sign "+")
;;     and whitespaces, only commas are tolerated.
;; 
(defun parse-number-set (string)
  (setf string (string-trim "=" string))
  (labels ((parse-integer-in-subsequence (start-index end-index)
             (let ((number (parse-integer
                             (subseq string start-index
                               (if end-index
                                 end-index
                                 (length string))))))
              (if (>= number 0)
                number
                (error "Only integer numbers >= 0 are valid repetitions: ~s < 0."
                        number)))))
    (loop
      for     start-index = 0 then (if end-index (1+ end-index) NIL)
      for     end-index   = (if start-index
                              (position #\, string :start start-index)
                              NIL)
      while   start-index
      collect (parse-integer-in-subsequence start-index end-index))))

(defun get-set-check-function (set-string)
  (let ((number-set (parse-number-set set-string)))
    #'(lambda (counter)
        (member counter number-set :test #'=))))

;;; ----------------------------------------------------

;; Parses a function name in the pattern:
;;   %:function-name:
;; 
;; The "function-name" is converted into upper-case letters.
;; The respective function is expected to be of arity one:
;;   lambda(counter) => generalized-boolean
;; 
(defun parse-user-function (string)
  ;; (subseq string ...): Remove the "%:" portion from the start
  ;;                      and    the ":"  portion from the end.
  ;;                      => Yield: raw "function-name".
  (let* ((function-name   (subseq string    2 (1- (length string))))
         (function-symbol (read-from-string function-name))
         (function-object (fdefinition      function-symbol)))
    function-object))

(defun get-user-check-function (string)
  (parse-user-function string))

;;; ----------------------------------------------------

(defun get-iteration-check-function (range-string)
  "Parses the RANGE-STRING and returns a unary function of signature
     lambda(counter) => generalized-boolean
   which is expected to return ``T'', iff the number of iterations
   COUNTER is valid."
  (let ((identifier (char range-string 0)))
    (cond
      ((char-equal identifier #\[) (get-range-check-function range-string))
      ((char-equal identifier #\=) (get-set-check-function   range-string))
      ((char-equal identifier #\%) (get-user-check-function  range-string))
      (t                           (error "Invalid range identifier: ~s." identifier)))))

;;; ----------------------------------------------------

(defun compile-re (pattern)
  "Create a regular expression from a pattern string."
  (let ((re (make-array 8 :adjustable t :fill-pointer 0))
        (bs (make-array 4 :adjustable t :fill-pointer 0)))
    (labels ((compile-op (op &rest args)
               (vector-push-extend (cons op args) re))

             ;; branch labels
             (make-label ()
               (vector-push-extend nil bs))
             (resolve-label (label)
               (setf (aref bs label) (fill-pointer re)))

             ;; compile a list of tokens recursively
             (compile-tokens (xs)
               (loop
                  for (op x y) in xs

                  ;; compile each token
                  do (case op

                       ;; if not x then y
                       (:or (let ((this (make-label))
                                  (else (make-label))
                                  (done (make-label)))
                              (compile-op :split this else)
                              (resolve-label this)
                              (compile-tokens x)
                              (compile-op :jump done)
                              (resolve-label else)
                              (compile-tokens y)
                              (resolve-label done)))

                       ;; zero or more (greedy)
                       (:* (let ((try (make-label))
                                 (done (make-label))
                                 (again (make-label)))
                             (resolve-label again)
                             (compile-op :split try done)
                             (resolve-label try)
                             (compile-tokens (list x))
                             (compile-op :jump again)
                             (resolve-label done)))

                       ;; zero or more (lazy)
                       (:- (let ((try (make-label))
                                 (done (make-label))
                                 (again (make-label)))
                             (resolve-label again)
                             (compile-op :split done try)
                             (resolve-label try)
                             (compile-tokens (list x))
                             (compile-op :jump again)
                             (resolve-label done)))

                       ;; one or more matches
                       (:+ (let ((rep (make-label))
                                 (done (make-label)))
                             (resolve-label rep)
                             (compile-tokens (list x))
                             (compile-op :split rep done)
                             (resolve-label done)))

                       ;; maybe match
                       (:? (let ((this (make-label))
                                 (else (make-label)))
                             (compile-op :split this else)
                             (resolve-label this)
                             (compile-tokens (list x))
                             (resolve-label else)))
                       
                       ;; ranged iterator
                       (:range (let ((try   (make-label))
                                     (done  (make-label))
                                     (again (make-label)))
                                 (let ((iteration-check-function (get-iteration-check-function y)))
                                   (compile-op     :start-counter iteration-check-function)
                                   
                                   (resolve-label  again)
                                   (compile-op     :split try done)
                                   
                                   (resolve-label  try)
                                   (compile-tokens (list x))
                                   (compile-op     :increase-counter)
                                   (compile-op     :jump again)
                                   
                                   (resolve-label  done)
                                   (compile-op     :check-counter))))
                       
                       ;; Lua boundary
                       (:bounds (let ((try (make-label))
                                      (done (make-label))
                                      (again (make-label)))
                                  (compile-op :char x)
                                  (resolve-label again)
                                  (compile-op :split done try)
                                  (resolve-label try)
                                  (compile-op :any)
                                  (compile-op :jump again)
                                  (resolve-label done)
                                  (compile-op :char y)))

                       ;; ignore groups just match tokens
                       (:ignore (compile-tokens x))

                       ;; capture groups push, match, and pop
                       (:capture (progn
                                   (compile-op :push)
                                   
                                   ;; Named group? => "y" is non NIL.
                                   (when y
                                     (compile-op :name-group y))
                                   
                                   (compile-tokens x)
                                   (compile-op :pop)))

                       ;; all other tokens compile to themselves
                       (otherwise (compile-op op x))))))

      ;; compile the parsed tokens
      (compile-tokens (parse-re pattern))

      ;; resolve all labels in split and jump instuctions
      (dotimes (i (length re))
        (symbol-macrolet ((b1 (second (aref re i)))
                          (b2 (third (aref re i))))
          (case (first (aref re i))
            (:jump  (setf b1 (aref bs b1)))
            (:split (setf b1 (aref bs b1)
                          b2 (aref bs b2))))))

      ;; finally, append the match instruction
      (compile-op :match)

      ;; return the regular expression
      (make-instance 're :pattern pattern :expression re))))

;;; ----------------------------------------------------

;; Repetition counter and inclusion check function for
;; ranged quantifiers of the type
;;   {[min,max]}
;;   {=set-of-numbers}
;;   {%:checking-function:}
;; 
(defstruct (re-counter
  (:constructor   make-counter (iterations check-function))
  (:conc-name     counter-))
  (iterations     0)         ;; Increased on each iteration.
  (check-function NIL))      ;; Checks the ITERATIONS for a match.

;;; ----------------------------------------------------

(defun counter-increase (counter)
  "Increased the re-counter COUNTER's number of iterations by one.
   Returns the modified COUNTER."
  (with-slots (iterations) counter
    (incf iterations))
  counter)

;;; ----------------------------------------------------

(defun counter-check (counter)
  "Checks whether the COUNTER's number of iterations is a valid match
   according to the COUNTER's checking function.
   Returns:
     T   - if the COUNTER's current value matches.
     NIL - if the COUNTER's current value does not match."
  (with-slots (iterations check-function) counter
    (funcall check-function iterations)))

;;; ----------------------------------------------------

(defmethod print-object ((counter re-counter) stream)
  (format stream "#<re-counter ~a>" (counter-iterations counter)))

;;; ----------------------------------------------------

(defstruct (re-thread (:constructor make-re-thread (pc sp groups stack counters)))
  pc        ; program counter in compiled re instruction vector
  sp        ; string pointer
  groups    ; pushed capture groups (subseq)
  stack     ; pushed capture groups (sp)
  counters) ; list of ranged quantifiers counter, each item an re-counter instance.

;;; ----------------------------------------------------

;; Input:  GROUPS - List of findings as triple
;;                    (start-index end-index group-name),
;;                  ordered in reverse discovery sequence.
;; Output: Vector of re-capture instances.
(defun captures-from-groups (input-string groups)
  "Creates and returns a vector of ``re-capture'' objects from the
   INPUT-STRING and the GROUPS.
   The GROUPS are expected as a list of triples, each such comprised of:
     (start-index-in-INPUT end-index-in-INPUT group-name)
   The group name may be NIL, in that case the captured group is
   unnamed."
  (let ((captures (make-array (length groups) :adjustable T :fill-pointer 0)))
    (loop
      for (start-index end-index group-name) in (reverse groups)
      for substring = (subseq input-string start-index end-index)
      do  (let ((capture (make-instance 're-capture
                           :start-position start-index
                           :end-position   end-index
                           :substring      substring
                           :name           group-name)))
           (vector-push-extend capture captures)))
    captures))

;; Input:  CAPTURES - Vector of re-capture instances.
;; Output: Hash-table mapping
;;           group-name -> (vector indices-of-groups-with-this-group-name)
(defun name-map-from-captures (captures)
  "Creates and returns a hash table mapping of the captured group names
   to the indices of the respective ``re-capture'' objects in the
   CAPTURES.
   CAPTURES is a vector of ``re-capture'' instances.
   Example: An entry in the resulting hash table
              'animal' -> (vector 4 7)
            means that the ``re-capture'' instances in the CAPTURES
            vector at the positions four and seven are associated with
            the captured group name 'animal'."
  (let ((name-map (make-hash-table :test #'equal)))
    (loop
      for capture     across captures
      for group-name  =    (re-capture-name capture)
      for group-index from 0
      do  (let ((group-indices-for-name (gethash group-name name-map)))
            (unless group-indices-for-name
              (setf group-indices-for-name
                (make-array 0 :adjustable T :fill-pointer 0)))
            (vector-push-extend group-index group-indices-for-name)
            (setf (gethash group-name name-map) group-indices-for-name)))
    name-map))

(defun match (s thread start offset)
  "Create a re-match from a thread that matched."
  (with-slots (sp groups) thread
    (let* ((captures (captures-from-groups   s groups))
           (name-map (name-map-from-captures captures)))
      (let ((cs (let (cs)
                  (do ((g (pop groups)
                          (pop groups)))
                      ((null g) cs)
                    (push (subseq s (first g) (second g)) cs)))))
        (make-instance 're-match
                       :start-pos (+ start offset)
                       :end-pos   sp
                       :groups    cs
                       :captures  captures
                       :name-map  name-map
                       :match     (subseq s (+ start offset) sp))))))

;;; ----------------------------------------------------

(defun run (re s start end &aux (pc 0) (offset 0))
  "Execute a regular expression program."
  (declare (optimize (safety 0) (debug 0) (speed 3)))
  (loop
     with threads = (list (make-re-thread pc (+ start offset) nil nil nil))

     ;; get the next thread off the list
     for thread = (pop threads)

     ;; once all the threads have been exhausted, the match fails
     until (null thread)

     ;; evaluate the next thread, checking for a match
     do (with-slots (pc sp groups stack counters)
            thread
          (loop
             for (op x y) = (aref re pc)

             ;; get the current character
             for c = (when (< sp end) (char s sp))

             ;; advance the program counter
             do (incf pc)

             ;; loop until the thread fails an instruction
             while (case op

                     ;; start and end boundaries
                     (:start   (= sp 0))
                     (:end     (= sp (length s)))

                     ;; match any character
                     (:any     (when (< sp end)
                                 (incf sp)))

                     ;; match an exact character
                     (:char    (when (eql c x)
                                 (incf sp)))

                     ;; match a predicate function
                     (:is      (when (and c (funcall x c))
                                 (incf sp)))

                     ;; fail to match a predicate function
                     (:is-not  (when (and c (not (funcall x c)))
                                 (incf sp)))

                     ;; push a capture group
                     (:push    (let ((capture (list sp)))
                                 (push capture stack)
                                 (push capture groups)))
                     
                     ;; Named group?
                     ;; => Extend current capture group by group name.
                     (:name-group
                               (progn
                                 (rplacd (first groups) (list x))
                                 (rplacd (first stack)  (list x))))
                     
                     ;; pop a capture group
                     (:pop     (let* ((group      (pop    stack))
                                      (group-name (second group)))
                                 (rplacd group (list sp group-name))))

                     ;; jump to an instruction
                     (:jump    (setf pc x))

                     ;; fork a thread
                     (:split   (let ((b (make-re-thread y sp groups stack counters)))
                                 (push b threads)
                                 (setf pc x)))
                     
                     ;; Start counting the number of iterations for
                     ;; a ranged iterator.
                     ;; "x" contains the range quantifier check function.
                     (:start-counter
                       (let ((new-counter (make-counter 0 x)))
                         (push new-counter counters)))
                     
                     ;; Increase the number of iterations.
                     (:increase-counter
                       (counter-increase (first counters)))
                     
                     ;; Check whether the number of iterations matches
                     ;; the range quantifier's bounds.
                     (:check-counter
                       (let ((is-in-range (counter-check (first counters))))
                         (when is-in-range
                           (pop counters))
                         is-in-range))
                     
                     ;; successfully matched, create and return
                     (:match   (return-from run
                                 (match s thread start offset))))))))

;;; ----------------------------------------------------

(defmacro with-re ((re pattern) &body body)
  "Compile pattern if it's not a RE object and execute body."
  (let ((p (gensym)))
    `(let ((,p ,pattern))
       (let ((,re (if (subtypep (type-of ,p) 're)
                      ,p
                    (compile-re ,p))))
         (progn ,@body)))))

;;; ----------------------------------------------------

#|
;; Convert
;;   (vector re-capture-1 ... re-capture-N)
;; into
;;   hash-table: group-name-i -> substr-i
;;               or
;;               group-name-i -> (substr-i-1 ... substr-i-M)
;; 
(defun match-captures-to-map (captures)
  (let ((name-group-table (make-hash-table :test #'equal)))
    (labels (;; Convert the hash-table scalar value for the key
             ;; GROUP-NAME into a one-element list: (list value),
             ;; for further item appendage.
             (entry-to-list (group-name)
               (setf (gethash group-name name-group-table)
                 (list (gethash group-name name-group-table)))
               name-group-table)
             (set-substring (group-name substring)
               (setf (gethash group-name name-group-table)
                     substring))
             ;; Interpret the hash-table value for the GROUP-NAME as
             ;; a list, and append the SUBSTRING to the list tail.
             (add-substring (group-name substring)
               (setf (gethash group-name name-group-table)
                     (append (gethash group-name name-group-table)
                             (list substring)))
               name-group-table))
      (loop for match across captures do
        (let ((substring  (re-capture-substring match))
              (group-name (re-capture-name      match)))
          (cond
            ((null group-name)  (add-substring "" substring))
            (t                  (let ((matches-for-name (gethash group-name name-group-table)))
                                  (if (null matches-for-name)
                                    (set-substring group-name substring)
                                    (progn
                                      (unless (listp matches-for-name)
                                        (entry-to-list group-name))
                                      (add-substring group-name substring)))))))))
    name-group-table))
|#

;;; ----------------------------------------------------

(defun retrieve-set-of-captures (match set)
  (cond
    ((and (symbolp set) (eq set :all-groups))
     (match-captures         match))
    ((and (symbolp set) (eq set :unnamed-groups))
     (map 'vector
       #'(lambda (capture)
           (not (re-capture-has-name capture)))
       (match-captures match)))
    ((and (symbolp set) (eq set :named-groups))
     (map 'vector
       #'(lambda (capture)
           (re-capture-has-name capture))
       (match-captures match)))
    ((stringp set)
     (match-captures-by-name match set))
    (T
     (error "Invalid SET: ~a."     set))))

;;; ----------------------------------------------------

(defun extract-components (captures component)
  (case component
    (:capture        captures)
    (:substring      (map 'vector #'re-capture-substring      captures))
    (:start-position (map 'vector #'re-capture-start-position captures))
    (:end-position   (map 'vector #'re-capture-end-position   captures))
    (:start-and-end-position
                     (map 'vector
                       #'(lambda (capture)
                           (list (re-capture-start-position capture)
                                 (re-capture-end-position   capture)))
                       captures))
    (:name           (map 'vector #'re-capture-name           captures))
    (otherwise       (error "Invalid COMPONENT: ~a." component))))

;;; ----------------------------------------------------

(defun select-captures (captures selection default-value)
  (cond
    ((symbolp  selection)
      (case selection
        (:all   captures)
        (:first (if (plusp (length captures))
                  (aref captures 0)
                  default-value))
        (:last  (if (plusp (length captures))
                  (aref captures (1- (length captures)))
                  default-value))))
    ((integerp selection)
      (if (array-in-bounds-p captures selection)
        (aref captures selection)
        default-value))
    (T (error "Invalid SELECTION: ~a." selection))))

;;; ----------------------------------------------------

(defun match-extract-data
  (match
   &key ;; Valid: {:all-groups, :named-groups, :unnamed-groups,
        ;          group-name-string}.
        (set       :all-groups)
        ;; Valid: {:all, :first, :last, integer-index}.
        (selection :all)
        ;; Valid: {:capture, :substring, :start-position, :end-position,
        ;;         :start-and-end-position, :name}.
        (component :capture)
        ;; Substitute value for lacuna of result.
        (default    NIL))
  "Extracts the desiderated captured groups' data from the MATCH.
   Input:
     SET       - Designates which subset of ``re-capture'' objects
                 to retrieve from the MATCH.
                 Valid options comprise:
                   :all-groups     - Returns a vector containing all
                                     groups of the MATCH.
                                     This is the default.
                   :named-groups   - Returns a vector containing only
                                     named groups. Unnamed ones will be
                                     discarded.
                   :unnamed-groups - Returns a vector containing only
                                     unnamed groups. Named ones will be
                                     discarded.
                   group-name      - A string designating the name of
                                     the named group(s) to retrieve.
                                     Regardless of the quantity of these ---
                                     even if zero or one group with such
                                     denomination is found --- a vector
                                     of ``re-captures'' is returned.
     SELECTION - As the selected SET yields a vector of captures,
                 this parameter returns either the whole vector or a
                 specified item thereof.
                 Valid options are:
                   :all   - The complete SET vector. This is the default.
                   :first - The first group in the SET vector.
                   :last  - The last group in the SET vector.
                   index  - An non-negative integer designating the
                            index of the group in the SET vector to
                            retrieve.
     COMPONENT - The information to extract from each captured group.
                 May be one of these:
                   :capture        - The whole ``re-capture'' object.
                                     This is the default.
                   :substring      - The substring captured by the group.
                   :start-position - The start position in the input
                                     where the group begins.
                   :end-position   - The end position in the input
                                     where the group ends.
                   :start-and-end-position -
                                     A list of two items in
                                     this order:
                                       (start-position end-position).
                   :name           - The name of the group or, if it
                                     is unnamed, the NIL value.
     DEFAULT   - A substitute value to return if the specified group
                 cannot be retrieved, for instance because of a
                 nonexistent group name."
  (let ((captures (retrieve-set-of-captures match set)))
    (setf captures (extract-components captures component))
    (select-captures captures selection default)))

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
        ($* (intern "$*" *package*))
        
        ($-> (intern "$->" *package*)))
    `(let ((,match ,match-expr))
       (if (null ,match)
           ,no-match
         (let ((,$$  (match-string   ,match))
               (,$*  (match-groups   ,match))
               (,$<> (match-captures ,match)))
           (declare (ignorable ,$$ ,$*))
           
           (labels ((,$-> (set &optional (selection :all) (component :capture) (default NIL))
                          (match-extract-data ,match
                            :set       set
                            :selection selection
                            :component component
                            :default   default)))
             
             (symbol-macrolet ((,$1 (first    ,$*))
                               (,$2 (second   ,$*))
                               (,$3 (third    ,$*))
                               (,$4 (fourth   ,$*))
                               (,$5 (fifth    ,$*))
                               (,$6 (sixth    ,$*))
                               (,$7 (seventh  ,$*))
                               (,$8 (eighth   ,$*))
                               (,$9 (ninth    ,$*))
                               (,$_ (nthcdr 9 ,$*)))
               (progn ,@body))))))))

;;; ----------------------------------------------------

(defun match-re (pattern s &key exact (start 0) (end (length s)))
  "Test a pattern re against a string."
  (with-re (re pattern)
    (let ((m (run (re-expression re) s start end)))
      (if (not exact)
          m
        (when m
          (and (= (match-pos-end m) end) m))))))

;;; ----------------------------------------------------

(defun find-re (pattern s &key all (start 0) (end (length s)))
  "Find a regexp pattern match somewhere in a string."
  (with-re (re pattern)
    (let ((i start))
      (flet ((next-match ()
               (loop
                  until (>= i end)

                  ;; is there a match at this offset?
                  for m = (run (re-expression re) s i end)

                  ;; return the found match or advance the offset
                  do (if m
                         (return (prog1 m
                                   (setf i (match-pos-end m))))
                       (incf i)))))
        (if all
            (loop for m = (next-match) while m collect m)
          (next-match))))))

;;; ----------------------------------------------------

(defun split-re (pattern s &key all coalesce-seps (start 0) (end (length s)))
  "Split a string into one or more strings by pattern match."
  (with-re (re pattern)
    (let* ((seqs (list nil)) (tail seqs))
      (do ((m (find-re re s :start start :end end)
              (find-re re s :start start :end end)))
          ((null m))

        ;; only split if not all, coalescing, or there's something there
        (when (or (not coalesce-seps) (> (match-pos-start m) start))
          (let ((split (subseq s start (match-pos-start m))))
            (setf tail (cdr (rplacd tail (list split))))))

        ;; update the search position after the split
        (setf start (match-pos-end m))

        ;; stop after a single match?
        (unless (or all (eq tail seqs))
          (return)))

      ;; add everything that's left
      (when (< start end)
        (rplacd tail (list (subseq s start end))))

      ;; return the list or two values
      (if all
          (rest seqs)
        (values-list (rest seqs))))))

;;; ----------------------------------------------------

(defun replace-re (pattern with s &key all (start 0) (end (length s)))
  "Replace patterns found within a string with a new value."
  (with-re (re pattern)
    (with-output-to-string (rep nil :element-type 'character)
      (do ((m (find-re re s :start start :end end)
              (find-re re s :start start :end end)))
          ((null m))

        ;; write out everything up to the match
        (when (< start (match-pos-start m))
          (write-string s rep :start start :end (match-pos-start m)))

        ;; replace the match with a value
        (princ (if (functionp with) (funcall with m) with) rep)

        ;; update the search position after the match
        (setf start (match-pos-end m))

        ;; stop after a single replace?
        (unless all (return)))

      ;; add everything that's left
      (when (< start end)
        (write-string s rep :start start :end end)))))

;;; ----------------------------------------------------

(defmethod make-load-form ((re re) &optional env)
  "Tell the system how to save and load a regular expression to a FASL."
  (declare (ignore env))
  `(compile-re ,(re-pattern re)))

;;; ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((dispatch-re (s c n)
           (declare (ignorable c n))
           (let ((delim (read-char s)))
             (compile-re (with-output-to-string (re)
                           (do ((c (read-char s t nil t)
                                   (read-char s t nil t)))
                               ((char= c delim))
                             (if (char= c #\\)
                                 (princ (read-char s t nil t) re)
                               (princ c re))))))))
    (set-dispatch-macro-character #\# #\r #'dispatch-re)))

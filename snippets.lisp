;;;; Functions and macros that I've written and might be useful for more than
;;;; one program
(defpackage snippets
  (:use :cl))

(in-package snippets)

;; Sequence and functional programming utilities

(defun curry (function &rest args)
  "Partially apply function to args and return new function"
  (lambda (&rest more-args)
    (apply function (append args more-args))))

(defun chunk (n seq)
  (loop :for i :from 0 :until (null (nthcdr (+ i (1- n)) seq))
     :collecting
       (loop :for j :from i :to (+ i (1- n)) :collecting (nth j seq))))

(defun flatten* (l)
  "Like alexandria:flatten but don't prune 'nil from result"
  (if (atom l)
      (list l)
      (append (flatten* (first l)) (when (cdr l) (flatten* (rest l))))))

(defun apply-successive (seq target func)
  "Successively apply function until first non-nil result, returning it or 
nil if no results"
  (loop
     for item in seq
     for result = (funcall func item target)
     thereis result))

;;; Threading macros, like in R

(defmacro ---> (value &body body)
  "Call: (---> 2
               square
               exp
         ==> 54.59815
   Xpnd: (exp (square 2))"
  (reduce #'list body :from-end t :initial-value value))

(defmacro -_-> (value &body body)
  "Call: (-_-> 3
               (+ '_ 1)
               (* 4 '_))
         ==> 16
   Xpnd: (* 4 (+ 3 1))"
  (reduce (lambda (f fs) (substitute f '_ fs))
          body
          :initial-value value))

;;; Setting class attributes based on lists of accessor names. I found
;;; this useful when loading a CSV where the columns containing the relevant
;;; data might have been in different orders

(defun dynamic-setf (object alist)
  "Dynamically set slots of object based on alist of accessor
   name string and data"
  (dolist (pair alist)
    (funcall (fdefinition (list 'setf (find-symbol (str:upcase (car pair))
                                                   "KEYWORD")))
             (cdr pair)
             object)))

(defun dynamic-make-instance (object alist)
  (let ((instance (make-instance object)))
    (dynamic-setf instance alist)
    instance))

;;; String utilities

(defun replace-multiple (olds news string)
  "Replace multiple substrings with one call instead of chaining
calls to str:replace"
  (let ((result string))
    (loop
       :for old :in olds
       :for new :in news
       :do (setf result (str:replace-all old new result)))
    result))

(defun strip-text (string trim-texts new-text)
  "Successively remove substrings in trim-texts from string"
  (reduce (lambda (s trim) (replace-all trim new-text s))
          trim-texts
          :initial-value string))

(defun split-multiple (seps string)
  "Split sucessively on multiple separators"
  (reduce (lambda (result sep) (flatten (mapcar (lambda (s) (split sep s))
                                   result)))
          seps
          :initial-value (list string)))

(defun min-edit-distance (source target &key costs)
  "Levenshtein distance via Jurafsky and Martin. Turns out this is useful!"
  (let ((dist-matrix (make-array (list (length source) (length target)))))
    ;; Initialisation
    (destructuring-bind (n m) (mapcar '1- (array-dimensions dist-matrix))
      (loop
         for i from 1 to n
         do (setf (aref dist-matrix i 0)
                  (1+ (aref dist-matrix (- i 1) 0))))
      (loop
         for i from 1 to m
         do  (setf (aref dist-matrix 0 i)
                   (1+ (aref dist-matrix 0 (- i 1)))))
      ;; Recurrence relation
      (loop
         for i from 1 to n
         do (loop
               for j from 1 to m
               do (setf (aref dist-matrix i j)
                        (min (1+ (aref dist-matrix (1- i) j))
                             (1+ (aref dist-matrix (1- i) (1- j)))
                             (1+ (aref dist-matrix i (1- j)))))))
      (aref dist-matrix n m))))

;;TODO: turn the following function into a generic macro

(defun match-period (period year)
  "Parse the period string given in the spreadsheet for the pattern and
decide whether the given year is within the period.

Periods can be in the following formats: X0s, X0s/Y0s, YEAR.
This regex/function dispatch setup is quite nifty, maybe I should make
it into a macro for future use?"
  (labels ((same-decade (decade year)
             (equal (char (write-to-string year) 2)
                    (char decade 0)))
           (year-in-range (year decade1 decade2)
             (or (same-decade decade1 year)
                 (same-decade decade2 year)))) ; OR doesn't eval other forms if 1st is t
    
    (let ((despatch `(("\\d{2}(?=s)" ,(lambda (m)
                                        (year-in-range year (first m) (second m))))
                      ("\\d{4}"      ,(lambda (m)
                                        (equal year (parse-integer (first m))))))))
      (remove-if #'null
                 (loop
                    :for (regex func) :in despatch
                    :for match := (ppcre:all-matches-as-strings regex period)
                    :collect (when match (funcall func match)))))))

(defmacro regex-dispatch (string &body)
  "Search string for given matches and call associated function
Call:
(regex-dispatch 'hearty as my g s13g h13l'
                ('\\d{2}' (lambda (match) (+ match 13)))
                ('g' (lambda (match) (format t 'chur my g')))
Expansion:
")

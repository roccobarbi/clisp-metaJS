;;;; This program is aimed at creating a very high level language that writes
;;;; complex and formally correct Javascript with minimal code.
;;;;
;;;; The output of a typical program will be a list of strings and lists, which
;;;; can be parsed recursively, adding a level of indentation at each new list.
;;;; Each string is a line of code.
;;;;
;;;; E.g. (namespace "webtrekk_dl" (#'private-var "ciao" "mondo") (#'public-var "a" "b"))

(defvar *namespace* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro conc (type out var &body body)
  `(setf ,out (concatenate ,type ,var ,@body)))

(defmacro concstring (out var &body body)
  `(conc 'string ,out ,var ,@body))

(defmacro conclist (out var &body body)
  `(conc 'list ,out ,var ,@body))

(defun public-var (name value)
  (let ((output ""))
    (concstring output *namespace* "." name " = " value ";")
    output))

(defun private-var (name value)
  (let ((output ""))
    (concstring output "var " name " = " value ";")
    output))

(defmacro namespace (ns &rest contents)
  `(let ((*namespace* (concatenate 'string "window." ,ns)) (output nil))
     (conclist output output (list "(function(ns){"))
     (conclist output output
       (let ((*namespace* "ns"))
	 (list
	  (list ,@(loop for e in contents collect `(funcall ,@e))))))
     (let ((line ""))
       (concstring line line "}(" *namespace* " = " *namespace* " || {}));")
       (conclist output output (list line)))
     output))


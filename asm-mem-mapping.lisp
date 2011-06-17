#!/usr/local/bin/clisp
;; -*- mode: lisp -*-
(unless (= (length ext:*args*) 2)
  (error "usage: mem-mapping asm-file.s bin-file"))

(let ((*standard-output* nil)
      (*error-output* nil))
  (load (merge-pathnames ".clisprc.lisp" (user-homedir-pathname)))
  (require :cl-ppcre))
(defpackage :mm (:use :cl :cl-ppcre))
(in-package :mm)

(defmacro lambda-registers (registers regexp &body body)
  "Create a function over the register matches using `register-groups-bind'."
  (let ((string (gensym)))
    `(lambda (,string)
       (register-groups-bind ,registers (,regexp ,string)
         ,@body))))

(defun shell (&rest rst)
  (let ((in (ext:run-shell-command
             (apply #'format (cons nil rst)) :output :stream)))
    (loop for line = (read-line in nil :eof)
       until (eq line :eof) collect line)))

(defvar asm-path (first ext:*args*))
(defvar bin-path (second ext:*args*))
(defvar asm-lines
  (with-open-file (in asm-path)
    (loop for line = (read-line in nil :eof)
       until (eq line :eof) collect line)))

(defun functions (lines)
  (remove nil (mapcar (lambda-registers (name) "^([^\\.\\s][\\S]+):" name) lines)))

(defun gdb-disassemble (function)
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s" function bin-path))

(defun addrs (function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda-registers (addr offset) "[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*"
       (parse-integer addr :radix 16))
     (gdb-disassemble function))))

(defun lines (function &aux on)
  "Return the line numbers of the lines (in order) of FUNCTION."
  (loop for line in asm-lines as counter from 0
     ;; possibly collect the line number
     when on collect counter
     ;; turn collection on or off
     do (register-groups-bind (line-function) ("^([^\\.][\\S]+):" line)
          (setf on (string= function line-function)))))

(defun addr-zip (function)
  "Combine the addresses and lines, skipping any lines which map to .L labels."
  (mapcar #'cons
          (addrs function)
          (remove-if (lambda (n) (scan "^[\\s]*\\." (nth n asm-lines)))
                     (lines function))))

(let ((mapping (sort (apply #'append (mapcar #'addr-zip (functions asm-lines)))
                     #'< :key #'car)))
  (loop for (addr . line) in mapping
     do (format t "~&~x ~d" addr line)))

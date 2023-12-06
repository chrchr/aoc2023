(defpackage eof
  (:use :cl)
  (:export #:eof-p))

(in-package eof)

(defun eof-p (stream)
  (eq 'eof (peek-char nil stream nil 'eof)))

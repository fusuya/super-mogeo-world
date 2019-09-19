;;win
(ql:quickload :ftw)

(defpackage moge
  (:use #:cl #:ftw #:cffi))

(in-package moge)

(load "stage.lisp" :external-format :utf-8)
(load "def-win.lisp" :external-format :utf-8)
(load "moge-win.lisp" :external-format :utf-8)


;; (setf sb-ext:*invoke-debugger-hook*
;;       (lambda (condition hook)
;;         (declare (ignore condition hook))
;; 	(gamekit:stop)))

;; (load "mogeo-gl.lisp")


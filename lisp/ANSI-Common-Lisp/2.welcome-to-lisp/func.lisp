#!/usr/bin/clisp

(defun our-third (x)
  (car (cdr (cdr x))))

(format t "~d~%" (our-third '(1 2 3 4)))


(defun our-member (obj lst)
  (if (null lst)
    nil
    (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst)))))

(format t "~A~%" (our-member 'b '(a b c)))
(format t "~A~%" (our-member 'z '(a b c)))


(defun show-squares (start end)
  (do ((i start (+ i 1)))
    ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))
(format t "~A~%" (show-squares 1 5))

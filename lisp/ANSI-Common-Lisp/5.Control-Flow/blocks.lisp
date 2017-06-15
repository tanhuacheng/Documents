#!/usr/bin/clisp

(progn
  (format t "a")
  (format t "b")
  (+ 11 12))

(block head
       (format t "Here we go.")
       (return-from head 'idea)
       (format t "We'll never see this"))

(block nil
       (return 27))

(dolist (x '(a b c d e))
  (format t "~A " x)
  (if (eql x 'c)
      (return 'done)))

(defun foo ()
  (return-from foo 27))
(foo)

(defun read-integer (str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
        (if i
            (setf accum (+ (* accum 10) i))
            (return-from read-integer nil))))
    accum))

(tagbody
  (setf x 0)
  top
  (setf x (+ x 1))
  (format t "~A " x)
  (if (< x 10) (go top)))

(let ((x 7)
      (y 2))
  (+ x y))

((lambda (x) (+ x 1)) 3)

(defun our-member (obj lst)
  (cond ((atom lst) nil)
        ((eql obj (car lst)) lst)
        (t (our-member obj (cdr lst)))))

(multiple-value-bind (x y z) (values 1 2)
  (list x y z))
(multiple-value-bind (s m h) (get-decoded-time)
  (format t "~A:~A:~A" h m s))
(multiple-value-call #'+ (values 1 2 3))
(multiple-value-list (values 1 2 3))

(setf (symbol-function 'add)
      #'(lambda (x) (+ x 2)))

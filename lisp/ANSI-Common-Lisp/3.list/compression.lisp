#!/usr/bin/clisp

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql elt next)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(setf x '(1 1 1 0 1 0 0 0 1 1 0 0 1 0))
(format t "~A:~%~A~%" x (compress x))


(defun uncompress (x)
  (if (null x)
      nil
      (let ((elt (car x))
            (rest (uncompress (cdr x))))
        (if (consp elt)
            (append (apply #'list-of elt) rest)
            (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(setf x '((3 1) 0 1 (2 0)))
(format t "~A:~%~A~%" x (uncompress x))

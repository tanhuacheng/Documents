#!/usr/bin/clisp

(format t "hello, world!~%")


; 没什么特别的, 和其它语言差不多
(defun sum (n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))

(format t "~d~%" (sum 100))


; 输入一个数 n, 返回把 n 与传入参数相加的函数
(defun addn (n)
  #'(lambda (x)
      (+ x n)))

(let ((add10 (addn 10)))
  (format t "~d~%" (funcall add10 5)))
(let ((add10 (addn 10)))
  (format t "~d~%" (funcall add10 6)))

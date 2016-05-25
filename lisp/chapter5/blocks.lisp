(progn
  (format t "a~%")
  (format t "b~%")
  (+ 11 22))


(block nil
       (format t "block nil~%")
       (block head
              (format t "Here we go.~%")
              (format t "inside nil~%")
              (return-from head 'idea)
              (format t "We'll never see this.~%"))
       (format t "block nil end~%")
       (return 'ret-nil)
       (format t "block nil return~%"))

(block nil
       (return 27))

;许多接受一个表达式主体的CL操作符，皆隐含在一个叫做nil的区块里。比如：
(dolist (x '(a b c d e))
  (format t "~A" x)
  (if (eql x 'c)
    (return 'done)))

;使用defun定义的函数体，都隐含在一个与函数同名的区块里，所以可以:
(defun foo ()
  (return-from foo 27))
(foo)

(defun our-read-integer (str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
        (if i
          (setf accum (+ (* accum 10) i))
          (return-from our-read-integer nil))))
    accum))

(tagbody
  (setf x 0)
  top
  (setf x (+ x 1))
  (format t "~A " x)
  (if (< x 10)
    (go top)))


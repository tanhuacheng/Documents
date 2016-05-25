(defun our-copy-tree (tr)
  (if (atom tr)
    tr
    (cons (our-copy-tree (car tr))
          (our-copy-tree (cdr tr)))))

(setf x '(and (integerp x) (zerop (mod x 2))))
(subst 'y 'x x)
x

(defun our-subst (new old tree)
  (if (eql tree old)
    new
    (if (atom tree)
      tree
      (cons (our-subst new old (car tree))
            (our-subst new old (cdr tree))))))

(setf x (our-subst 'y 'x x))

(defun mirrorp (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))
(mirrorp '(a b b a))
(equal '(a b b a) (reverse '(a b b a)))

(setf x '(1 3 5 2 3 8 6 7 9 2 3))
(sort x #'>)
(some #'oddp x)

(setf x '(1 2 3 4))
(setf y x)
x
y
(pop x)
(push 1 x)
(setf (car x) 8)
(setf (cadr x) 8)
(setf x '((+ . "add") (- . "subtract")))
(assoc '+ x)





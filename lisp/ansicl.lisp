(cons 'a '(b c d))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
    ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-rec (start end)
  (if (> start end)
    'done
    (progn
      (format t "~A ~A~%" start (* start start))
      (show-squares (+ start 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(lambda (x y)
  (+ x y))

(funcall #'(lambda (x) (+ x 100)) 1)

;映射函数(Mapping Functions)
(mapcar #'(lambda (x y) (+ x y))
        '(1 2 3) '(4 5 6))
;(5 7 9)

(mapcar #'list
        '(a b c)
        '(1 2 3 4))
;((A 1) (B 2) (C 3))

(maplist #'(lambda (x) x)
         '(a b c))
;((A B C) (B C) (C))

;树(Trees)
(defun our-copy-tree (tr)
  (if (atom tr)
    tr
    (cons (our-copy-tree (car tr))
          (our-copy-tree (cdr tr)))))

(defun our-subst (new old tree)
  (if (eql tree old)
    new
    (if (atom tree)
      tree
      (cons
        (our-subst new old (car tree))
        (our-subst new old (cdr tree))))))

    
;理解递归(Understanding Recursion)
(defun len (lst)
  (if (null lst)
    0
    (+ 1 (len (cdr lst)))))

(defun our-member (obj lst)
  (if (null lst)
    nil
    (if (eql obj (car lst))
      lst
      (our-member obj (cdr lst)))))

;集合(Sets)

(+ 1 2)



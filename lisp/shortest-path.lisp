(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (format t "~a~%" queue)
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
          (reverse path)
          (bfs end
               (append (cdr queue)
                       (new-paths path node net))
               net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(setf min '((a b c) (b c) (c d)))
(shortest-path 'a 'd min)
(new-paths '(a) 'a min)
(cdr '((a)))
(setf net '((a b c) (b g) (c d e) (d h) (e f) (f h) (g h)))
(shortest-path 'a 'h net)


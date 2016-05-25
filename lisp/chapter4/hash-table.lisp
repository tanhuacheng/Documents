(setf ht (make-hash-table))
(gethash 'color ht)
(setf (gethash 'color ht) 'red)
(setf (gethash 'color ht) nil)

(setf bugs (make-hash-table))
(push "Doesn't take keyword arguments."
      (gethash #'member bugs))
bugs
(push "tan" (gethash 'name bugs))
(remhash 'name bugs)

(make-hash-table :size 5)

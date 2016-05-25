(setf *l* (list 'a 'b 'c))
(loop for cons on *l*
      do (format t "~a" (car cons))
      when (cdr cons)
      do (format t ", "))
(format t "~{~a~^, ~}" *l*)

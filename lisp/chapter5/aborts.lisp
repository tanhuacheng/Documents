(defun super ()
  (catch 'abort
         (sub)
         (format t "We'll never see this.")))

(defun sub ()
  (throw 'abort 99))

(super)

(progn
  (error "Oops!")
  (format t "After the error."))
  Error: Oops!
  Options: :abort, :backtrace


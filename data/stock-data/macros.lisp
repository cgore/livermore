(defmacro while (conditional &rest rest)
  `(loop (when (not ,conditional)
           (return))
    ,@rest))

(defmacro until (conditional &rest rest)
  `(loop (when ,conditional
           (return))
    ,@rest))

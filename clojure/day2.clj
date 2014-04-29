;; Unless macro with a body
;;(defmacro unless [test body]
;;    (list 'if (list 'not test) body))

;; Unless macro with body / else call
(defmacro unless [test body else]
    (list 'if (list 'not test) body else))

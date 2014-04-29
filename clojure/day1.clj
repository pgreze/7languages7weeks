(defn big
    "Returns if a string st is longer than n characters"
    [st n]
    (< n (count st)))
;; Tests:
;; user=> (big "hello" 3)
;; true
;; user=> (big "hello" 5)
;; false
;; user=> (big "hello" 6)
;; false

(defn collection-type
    [col]
    (if (list? col) :list
        (if (vector? col) :vector
            (if (map? col) :map :unknown))))
;; Tests:
;; user=> (collection-type [])
;; :vector
;; user=> (collection-type ())
;; :list
;; user=> (collection-type {})
;; :map
;; user=> (collection-type #{})
;; :unknown
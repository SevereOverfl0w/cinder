(let [a      10
      ;; comment
      foobar 20]
  ;; still here
  (+ a foobar))

;; Keep me

^{:a    20} {:a      20
             :foobar 10}


{:a      20} [  {:b   20
                 :c   30}]

(let   ) (let   
           [])

(let )
(let   )
(let
  )

(comment
  ;; This should stay

  {:a 1
   ;; Don't promote the next brace
   }

  ;; This should also stay
  )

#_(let                )

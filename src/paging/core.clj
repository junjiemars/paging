(ns paging.core
  (require [clojure.set :as set])
  (:gen-class))


(defn unique-rand-int [n r]
  (let [s (set (take n (repeatedly #(rand-int r))))]
    (concat s (set/difference (set (take n (range r)))
                              s))))

(defn paging
  "c, requested numbers which less than or equal 10
   n, requested numbers may be insected with c, S(c)<=S(n)
   p, pointer to paging position"
  [c n p])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [c []]
    (println "#Rc(K)=EMPTY")
    (pprint c))
  (let [c (set (repeatedly 10 #(rand-int 32)))
        n (set (repeatedly 10 #(rand-int 32)))]
    (println "#Rc(K)=N that (N <= 10)")
    (pprint c)
    (pprint n)
    (pprint (set/difference n c))
    (pprint (set/intersection n c))))

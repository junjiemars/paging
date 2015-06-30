(ns paging.core
  (require [clojure.set :as set])
  (:gen-class))


(defn unique-rand-int [n r]
  (let [s (set (take n (repeatedly #(rand-int r))))]
    (set (take n (concat s (set/difference
                            (set (take n (range r)))
                            s))))))

(defn unique-rand-set [n r x]
  (let [s (unique-rand-int r r)]
    (set (take n (set/difference s x)))))

(defn paging
  "c, requested numbers which less than or equal 10
   n, requested numbers may be insected with c, S(c)<=S(n)
   p, pointer to paging position
   s, page size, default is 10"
  [c n p s]
  (let [x 1]
    (if (empty? c)
      (let [p0 {:c c
                :n {:i 0 :v n}
                :r {:i 0 :v n}}]
        (pprint p0)
        p0)
      (let [c-rc (count c)
            c-rn (count n)]
        (if (empty? p);the fist page
          (let [d0 (take (- s c-rc) (set/difference n c))
                p0 {:c c
                    :n {:i 0 :v [n]}
                    :r {:i 0 :v d0}}]
            (pprint p0)
            p0)
          (let [ps (:r p)
                pn (:n p)
                p0 {:c c
                    :n {:i 0 :v (conj (:v (:n p)) n)}
                    :r {:i 0 :v #{}}}]
            (println ps)
            (println pn)
            (pprint p0)
            p0))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [c []
        n []
        p []]
    (println "#0Rc(K)=EMPTY")
    (paging c n p 10))
  (let [c (set (repeatedly 10 #(rand-int 32)))
        n (set (unique-rand-int 10 100))
        p0 []]
    (println "0#Rc(K)=N that (N <= 10)")
    (let [n1 (unique-rand-set 10 100 n)
          p1 (paging c n p0 10)]
      (println "1#Rc(K)=N Page#1")
      (let [p2 (paging c n1 p1 10)]
        (println "<END>")))))

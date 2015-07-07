(ns paging.core
  (require [clojure.set :as set])
  (:gen-class))


(defn unique-rand-int
  "Returns n numbers of rand integers based on r, 
  or be excluded by x."
  ([n r]
   (let [s (set (take n (repeatedly #(rand-int r))))]
     (set (take n (concat s (set/difference
                             (set (take n (range r)))
                             s))))))
  ([n r x]
   (let [s (unique-rand-int r r)
         x1 (set (reduce concat x))]
     (set (take n (set/difference s x1))))))

(defn unique-rand-pages
  "Returns [coll <news>], the <news> is uniqued against coll."
  [n r t f coll]
  (loop [i 0
         c coll
         t0 []]
    (if (< i t)
      (let [t1 (f (unique-rand-int n r c))]
        (recur (inc i) (conj c t1) (conj t0 t1)))
      (vector coll t0))))

(comment
            p1 (loop [j 0
                  v0 []]
             (cond
              (< j s)
              (recur s (vec
                        (concat (vec c) (take (- s cc) d))))

              (< i0 cd)
              (vector v0 (vec (take s (subvec (vec d) i0))))
              
               :else v0))
)

(defn pageup
  ([r p]
   (let [s (:s (:s r))
         l (:l (:s r))
         t (:t (:s r))
         c (:c (:c r))
         cc (count c)

         ri (:i (:r r))
         rl (:l (:r r))
         rc (:c (:r r))

         ni (:i (:n r))]
     (cond
      (< ri rl) (assoc-in p [:r :i] ri)

      (= 0 rc) (let [n0 (unique-rand-int s l)
                     d0 (set/difference n0 c)
                     cd (count d0)
                     i0 (- s cc)
                     v (concat (vec c) (take i0 d0))
                     p0 r
                     p1 (assoc-in p0 [:r]
                                  {:i 0 :l 1 :c (count v)
                                   :v (vec v)})
                     p2 (assoc-in p1 [:c :d] d0)
                     p3 (assoc-in p2 [:n]
                                  {:i 0 :l 1 :t t
                                   :c (count n0)
                                   :v n0})]
                 (pageup p3 p3 i0 d0))
      :else p
      )))

  ([r p i d]
   (let [d0 (vec d)]
     (update-in p [:r :v] vector (subvec d0 i))))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [s 3
        l 100
        t 12]
    (let [p0 (pageup 
                     {:s {:s s :l l :t t};paging constants
                      :c {:c (unique-rand-int
                              (rand-int (inc s)) l)
                          :d nil}
                      :n {:i 0 :l 0 :c 0}
                      :r {:i 0 :l 0 :c 0}}
                     {})
          p1 (pageup 
                     (update-in p0 [:r :i] + 1)
                     p0)
          ]
      (println "<END>")
      (pprint p0)
      (pprint p1))))


(ns paging.core
  (require [clojure.set :as set])
  (use [clojure.tools.trace])
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

         ni (:i (:n r))
         nc (:c (:n r))
         nv (:v (:n p))]
     (cond
      (< ri (dec rl)) (let [p0 (assoc-in p [:r :i] ri)
                            r0 (:v (:r p))
                            p1 (assoc-in p0 [:p] (nth r0 ri))]
                        p1)

      (= 0 rc)
      (let [n0 (unique-rand-int s l)
            d0 (set/difference n0 c)
            cd (count d0)
            i0 (- s cc)
            v (concat (vec c) (take i0 d0))
            p0 r
            p1 (assoc-in p0 [:r]
                         {:i 0 :l 1 :c (count v)
                          :v (if (empty? c)
                               (vector (vec v)) (vec v))})
            p2 (assoc-in p1 [:c :d] d0)
            p3 (assoc-in p2 [:n]
                         {:i 0 :l 1 :t t
                          :c (count n0)
                          :v (vector n0)})]
        (cond
         (and (> i0 0) (< i0 s)) 
         (pageup p3 p3 i0 d0)

         (= s cc) (pageup p3 p3 0 d0)

         :else p3)
        )

      (> ri (dec rl))
      (let [l0 (inc (- ri (dec rl)))
            n0 (unique-rand-int (* 2 s l0) l nv)
            d0 (set/difference n0 c)
            cd (count d0)
            v (peek (:v (:r p)))
            vc (count v)
            i0 (- s vc)]
        (cond
         (= s vc)
         (let [v0 (concat v (take i0 d0))]
           (printf "##v0:%s\n" (seq v0))
           p)
         
         :else
         (do (printf "#l0:%d n0:%s d0:%s v:%s\n"
                     l0 n0 d0 v)
             p)
         ))
      
      :else p
      )))

  ([r p i d]
   (let [r0 (:r p)
         s (:s (:s p))
         i0 (+ i (min s (- (count d) i)))
         d0 (subvec (vec d) i i0)
         p1 (assoc-in p [:r] {:i (:i r0)
                              :l (inc (:l r0))
                              :c (+ (:c r0) (count d0))
                              :v (vector (:v r0) d0)})]
     (if (< i0 (dec (count d)))
       (pageup p1 p1 i0 d)
       p1))
   )
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
                      :r {:i 0 :l 0 :c 0}
                      :p nil}
                     {})
          p1 (pageup 
                     (update-in p0 [:r :i] + 1)
                     p0)

          p2 (pageup (update-in p1 [:r :i] + 1) p1)]
      (println "<END>")
      (let [i (:i (:r p0))
            v (:v (:r p0))]
        (pprint p0)
        (pprint (nth v i)))
      (let [i (:i (:r p1))
            v (:v (:r p1))]
        (pprint p1)
        (pprint (nth v i)))
      (let [i (:i (:r p2))
            v (:v (:r p2))]
        (pprint p2)
        (pprint (nth v i))))))


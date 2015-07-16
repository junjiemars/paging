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
         rv (:v (:r r))

         ni (:i (:n r))
         nl (:l (:n r))
         nc (:c (:n r))
         nv (:v (:n p))]

     (cond
      (or (< ri (dec rl))
          (and (= ri (dec rl)) (= rc (* s rl))))
      (let [p0 (assoc-in p [:r :i] ri)
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
                          :v (vector (vec v))})
            
            p2 (assoc-in p1 [:c :d] d0)
            p3 (assoc-in p2 [:n]
                         {:i 0 :l 1
                          :c (count n0)
                          :v (vector n0)})]
        (cond
         (and (> i0 0) (< i0 s)) 
         (do
           (pageup p3 p3 i0 d0))

         (= s cc)
         (do
           (pageup p3 p3 0 d0))

         :else p3)
        )

      (>= ri (dec rl))
      (let [l0 (inc (- ri (dec rl)))
            s2 (* s l0)
            n0 (unique-rand-int s2 l nv)
            d0 (set/difference n0 c)
            cd (count d0)
            v (peek rv)
            vc (count v)
            i0 (- s vc)]
        (cond
         (< vc s)
         (let [v0 (concat v (take i0 d0))
               v1 (conj (pop rv) (vec v0))
               p0 (assoc-in p [:r]
                            {:i ri
                             :c (- (+ rc s) vc)
                             :l (dec (inc rl))
                             :v v1})
               p1 (assoc-in p0 [:c :d] d0)
               p2 (assoc-in p1 [:n]
                            {:i (+ ni l0)
                             :l (+ nl l0)
                             :c (+ nc s2)
                             :v (vec
                                 (concat nv
                                         (map set
                                              (partition s n0))))})]
           (pageup p2 p2 i0 d0))

         (= vc s)
         (let [v0 (vec (take s d0))
               v1 (conj rv v0)
               p0 (assoc-in p [:c :d] d0)
               p1 (assoc-in p0 [:r]
                            {:i rl
                             :c (+ rc s)
                             :l (inc rl)
                             :v v1})
               p2 (assoc-in p1 [:n]
                            {:i (+ ni l0)
                             :l (+ nl l0)
                             :c (+ nc s2)
                             :v (vec
                                 (concat nv
                                         (map set
                                              (partition s n0))))})]
           (pageup p2 p2 s d0))
         
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
                              :v (conj (:v r0) d0)})]
     
     (if (<= i0 (dec (count d)))
       (pageup p1 p1 i0 d)
       p1))
   )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [s 3
        l 100 ;removed in Pro env
        t 12]
    (let [p0 (pageup 
                     {:s {:s s :l l :t t}
                      :c {:c (unique-rand-int
                              (rand-int (inc s)) l)
                          :d nil}
                      :n {:i 0 :l 0 :c 0}
                      :r {:i 0 :l 0 :c 0}
                      :p nil}
                     {}) 

          p1 (pageup (update-in p0 [:r :i] + 1) p0)

          p2 (pageup (update-in p1 [:r :i] + 2) p1)

          ]
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
        (pprint (nth v i)))

)))

          (comment
            p1 (pageup 
                (update-in p0 [:r :i] + 1)
                p0)

            p2 (pageup (update-in p1 [:r :i] + 1) p1))

      (comment
        (let [i (:i (:r p1))
              v (:v (:r p1))]
          (pprint p1)
          (pprint (nth v i)))
        (let [i (:i (:r p2))
              v (:v (:r p2))]
          (pprint p2)
          (pprint (nth v i))))

(comment
           (= vc s)
         (let [v0 (vec (take i0 d0))
               p0 (assoc-in p [:c :d] d0)
               p1 (assoc-in p0 [:r]
                            {:i ri
                             :c (+ rc s)
                             :l (inc rl)
                             :v (concat rv v0)})]
           (pageup p1 p1 (+ i0 s) d0))
)

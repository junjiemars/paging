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


(defn paging0 [s c n i p]
  (let [cc (count c)
        cn (count n)
        d (set/difference n c)
        cd (count d)
        i0 (- s cc)
        p1 (loop [j 0
                  v0 []]
             (cond
              (< j s)
              (recur s (vec
                        (concat (vec c) (take (- s cc) d))))

              (< i0 cd)
              (vector v0 (vec (take s (subvec (vec d) i0))))
               :else v0))]
    p1))

(defn pageup
  "Return paging struct.
   s: page size
   l: limit
   t: total elements
   r: requested struct
   p: posted struct"
  [s l t r p]
  (let [r-ri (:i (:r r))
        r-rv (:v (:r r))
        r-rl (:l (:r r))
        r-ni (:i (:n r))
        ]
    (cond
     (< r-ri r-rl)
     (do
       (assoc-in p [:r :i] r-ri))
     
     (= 0 (+ r-ri r-rl))
     (let [c (:c r)
           p0 (assoc-in p [:c] c)
           n (unique-rand-int s l)
           d (set/difference n (:c r))
           n0 (assoc-in p0 [:n] {:i 0 :l 1 :v (vector n)})
           r0 (assoc-in n0 [:r] {:i 0 :l 1})
           v (concat c (take (- s (count c)) d))
           rv0 (assoc-in r0 [:r :v] (vec v))]
       (cond (= (count c) (count d))
             (let [p1 (assoc-in rv0
                                [:r :v]
                                (vector (:v (:r rv0))
                                        (vec d)))]
               (update-in p1 [:r :l] + 1))

         :else rv0))

     :else
     (let [p0 p]
       (println "abc")
       p0))
    ))

(defn paging
  "c, requested numbers which less than or equal 10
   n, requested numbers may be insected with c, S(c)<=S(n)
   p, pointer to paging position
   s, page size, default is 10
   t, total numbers"
  [c n p s]
  (let [x 1]
    (if (empty? c)
      (let [p0 {:c c
                :n {:i 0 :v n}
                :d []
                :r {:i 0 :v n}}]
        (pprint p0)
        p0)
      (let [cc (count c)
            cn (count n)]
        (if (empty? p);the fist page
          (let [d (take (- s cc) (set/difference n c))
                p0 {:c c
                    :n {:i 0 :v (vector n)}
                    :d d
                    :r {:i 0 :v
                        (vector
                         (vec (concat c d))
                         (vec (set/difference n c d)))}}]
            (pprint p0)
            p0)
          (let [pri (:i (:r p))
                prv (:v (:r p))
                crv (count prv)
                pni (:i (:n p))
                pnv (:v (:n p))]
            (if (<= (+ 1 pri) crv)
              (let [v (vec (nth prv pri))]
                (if (< (count v) s)
                  (let [n0 (unique-rand-int s 100 (vector pnv))
                        d0 (take (- s (count v)) n0)
                        v0 (vec (concat v d0))
                        pnv0 (assoc-in p [:n :v (inc pni)] n0)
                        pni0 (assoc-in pnv0 [:n :i] (inc pni))
                        p0 (assoc-in pni0 [:r :v pri] v0)]
                    (pprint p0)
                    p0)
                  (do (pprint p) p)))
              (let [dr0 (- (inc pri) crv)
                    dn0 (- (inc pri) (inc pni))
                    n0 (unique-rand-pages s 100 dn0 set pnv)]
                (println "dr0:" dr0)
                (println "dn0:" dn0)
                (println "pni:" pni)
                (println "n0:" n0)
                (println "n0-vec:" (map vec n0))
                
                ))
            p))))))

(defn -test
  [& args]
  (let [s 3
        l 100
        t 12]
    (let [p0 (pageup s l t
                     {:c (unique-rand-int (rand-int (inc s)) l)
                      :n {:i 0 :l 0}
                      :r {:i 0 :l 0}}
                     {})
          p1 (pageup s l t
                     (update-in p0 [:r :i] + 1)
                     p0)
          ]
      (println "<END>")
      (pprint p0)
      (pprint p1))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [c []
        n []
        p []]
    (println "#0Rc(K)=EMPTY")
    (paging c n p 10))
  (let [c (set (repeatedly 10 #(rand-int 32)))
        n (unique-rand-int 10 100)
        p0 []]
    (println "0#Rc(K)=N that (N <= 10)")
    (let [n1 (unique-rand-int 10 100 (vector n))
          p1 (update-in (paging c n p0 10) [:r :i] + 1)]
      (println "1#Rc(K)=N Page#1")
      (let [p2 (update-in (paging c n1 p1 10) [:r :i] + 2)]
        (println "2#Rc(K)=N Page#1+2")
        (let [p3 (paging c n1 p2 10)]
          (println "<END>"))))))

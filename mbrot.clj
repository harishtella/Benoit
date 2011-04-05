; take a stab at making a mandelbrot drawer

(ns mbrot
  (:use [rosado.processing]
        [rosado.processing.applet]))

(def screen-dim 512)
(def mbrot-dim 256)
(def base-color '(0 0 0))
(def colors 
  (repeatedly 
    #(list (rand-int 256) (rand-int 256) (rand-int 256))))


; TODO app next z in parrallel
; dont comput un needed'zs 


(defn iterate-with-end [f x] 
  (let [next-x (f x)]
    (if (= next-x ())
      ;;; XXX fixed this bug
      (list x)
      (cons x (lazy-seq (iterate-with-end f next-x))))))
      

(defn gen-constants-board [divs]
  (let [step-size (/ 4 (dec divs))]
    (letfn [(gen-f [[x y]] 
                     (cond 
                       (= [x y] [2 -2]) ()
                       (= x 2) [-2 (- y step-size)] 
                       :else [(+ x step-size) y]))]
      (iterate-with-end gen-f [-2 2])))) 

(comment
(defn gen-constants-board [divs]
  (let [step-size (/ 4 (dec divs))]
    (letfn [(gen-f [[x y]] 
                   (lazy-seq
                     (cond 
                       (= [x y] [2 -2]) ()
                       (= x 2) (let [new-elem [-2 (- y step-size)]]
                                 (cons new-elem (gen-f new-elem)))
                       :else (let [new-elem [(+ x step-size) y]]
                               (cons new-elem (gen-f new-elem))))))]
      (cons [-2 2] (gen-f [-2 2]))))) 
  )

(defrecord elem [z c color escaped])

(defn gen-init [divisions]
  (map
    (fn [x ] (elem. [0 0] x base-color false))
    (gen-constants-board divisions)))

(defn out-of-set [[x y]]
  (> (+ (* x x) (* y y)) 4))

(defn square-imaginary [[x y]]
  [(- (* x x) (* y y)) (* 2 x y)])

(defn next-z [z c]
  (vec (map + (square-imaginary z) c)))

(defn advance-elem [elem-old color-now]
  (let [{:keys [z c color escaped]} elem-old]
    (if escaped
      elem-old
      (let [new-z (next-z z c)]
        (cond
          (out-of-set new-z) (elem. new-z c color-now true)
          :else (elem. new-z c color false))))))

(defn do-mbrot-iters [[last-iter colors]]
  (let [[color-now & colors-rest] colors]
    [(map advance-elem last-iter (repeat color-now)) 
     colors-rest] 
    ))

(defn mbrot-iters [divs]
  (map first (iterate do-mbrot-iters [(gen-init divs) colors]))) 

(defn add-coords-to-elem [dim-elem-count i elem]
  (let [row (int (/ i dim-elem-count))
        col (mod i dim-elem-count)]
    (assoc elem 
           :y (/ row dim-elem-count) 
           :x (/ col dim-elem-count))))

(defn with-coords [elems]
  (let [dim-elem-count (sqrt (count elems))]   
    (map-indexed (partial add-coords-to-elem dim-elem-count) elems)))

(defn with-screen-coords [screen-size elems]
  (map #(assoc %
               :x (* (:x %) screen-size)
               :y (* (:y %) screen-size))
       elems))
 
(defn draw-mbrot [elem-size elem] 
  (let [{:keys [z c x y color escaped]} elem]
    (apply fill-float color) 
    (with-translation [x y]
                      (begin-shape)
                      (vertex 0 0)
                      (vertex elem-size 0)
                      (vertex elem-size elem-size)
                      (vertex 0 elem-size)
                      (end-shape CLOSE))))

(def mbrot-drawer
  (let [mbrot-data (atom (mbrot-iters mbrot-dim))
        elem-size (* (/ 1 (sqrt (count (first @mbrot-data)))) screen-dim)]
    (fn [] 
      (doall
        (map (partial draw-mbrot elem-size) 
           (with-screen-coords screen-dim
                               (with-coords (first @mbrot-data)))))
      (swap! mbrot-data rest))))

(defn key-pressed [evt]
  (redraw))
(defn draw
  []
  (mbrot-drawer)
  )
(defn setup []
  "Runs once."
  (smooth)
  (background-float 225)
  ;(stroke-float 0)
  (no-stroke)
; (no-loop)
  )

(defapplet mb :title "mandelbrot"
  :setup setup :draw draw :size [screen-dim screen-dim]
  :key-pressed key-pressed)

(run mb)
;; (stop mb)
 

(defn pz [x]
  (doall (map println (partition 3 (map :z x)))))

(def mbrot-iter-printer 
  (let [x (mbrot-iters 3)]
    (letfn [(pp [r] 
                (fn bb [] 
                  (pz (first r))
                  (pp (rest r))))]
      (pp x))))

(comment
(doall (map 
  println 
  (partition 3 
  (map #(select-keys % '(:x :y :c)) 
  (with-screen-coords screen-dim (with-coords (first x)))))))

(defn mbrot-drawer [divs]
  (let [mbrot-iters (atom (mbrot-iters divs))] 
    (fn [] (let [cur-data (first @mbrot-iters)]
             (map draw-tiles with-coords cur-data) 
             (swap! mbrot-iters rest)))))


(def mbrot-iter-printer (mbrot-iter-printer))

(def x (mbrot-iters 3))

(pz (first x))

(pz (second x))
)

 

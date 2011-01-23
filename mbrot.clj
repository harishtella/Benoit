; take a stab at making a mandelbrot drawer

(ns mbrot
  (:use [rosado.processing]
        [rosado.processing.applet]))


(defn split [n x]
  (if x
    (letfn [(make-block [n x]
                        (if (> n 0) 
                          (cons (first x) (make-block (dec n) (next x)))
                          ()))
            (get-rest-after [n x]
                     (if (> n 0)
                       (get-rest-after (dec n) (next x))
                       x))]
      (let [new-block (make-block n x)
            rest-of-list (get-rest-after n x)]
        (cons new-block (split n rest-of-list))))))

(defn pz [x]
  (doall (map println (split 3 (map :z x)))))

(def screen-dim 800)

(def colors 
  (repeatedly 
    #(list (rand-int 256) (rand-int 256) (rand-int 256))))

(def base-color '(0 150 0))

; this works but its not lazy and its a mess
(defn gen-constants-board [divs]
  (let [step-size (/ 4 (dec divs))]
    (letfn [(gen-f [[x y]] 
                   (cond 
                     (= [x y] [2 -2]) ()
                     (= x 2) (cons [-2 (- y step-size)] (gen-f [-2 (- y step-size)]))
                     :else (cons [(+ x step-size) y] (gen-f [(+ x step-size) y]))))]
      (cons [-2 2] (gen-f [-2 2]))))) 

(comment
; causes null pointer error
(defn gen-constants-board [divisions] 
  (let [step-size (/ 4 (dec divisions))]
    (letfn [(gen-f [[x y]] 
                   (cond 
                     (= [x y] [2 -2]) ()
                     (= x 2) [-2 (- y step-size)]
                     :else [(+ x step-size) y]))]
      (iterate gen-f [-2 2]))))

; this wont work because let isn't recursive
(defn gen-constants-board [divs]
  (let [step-size (/ 4 (dec divs))]
    (letfn [(gen-f [[x y]] 
                   (cond 
                     (= [x y] [2 -2]) ()
                     (= x 2) [-2 (- y step-size)]
                     :else [(+ x step-size) y]))]
      (let [board-seq (lazy-seq
                        (cons [-2 2] (gen-f (first board-seq))))]
        board))))
)
   
(defn gen-init [divisions]
  (map
    (fn [x ] {:z [0 0]
               :c x
               :color base-color
               :escaped false})
    (gen-constants-board divisions)))

(defn out-of-set [[x y]]
  (> (+ (* x x) (* y y)) 4))

(defn square-imaginary [[x y]]
  [(- (* x x) (* y y)) (* 2 x y)])

(defn next-z [z c]
  (vec (map + (square-imaginary z) c)))

(defn advance-elem [elem-old color-now]
  (let [{:keys [z c color escaped]} elem-old
        new-z (next-z z c)]
    (cond
      escaped {:z new-z
                 :c c
                 :color color
                 :escaped escaped}
      (out-of-set new-z) {:z new-z
                          :c c
                          :color color-now
                          :escaped true}
      :else {:z new-z
             :c c
             :color color
             :escaped false})))

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
  
(def mbrot-iter-printer 
  (let [x (mbrot-iters 3)]
    (letfn [(pp [r] 
                (fn bb [] 
                  (pz (first r))
                  (pp (rest r))))]
      (pp x))))


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
  (let [mbrot-data (atom (mbrot-iters 80))
        elem-size (* (/ 1 (sqrt (count (first @mbrot-data)))) screen-dim)]
    (fn [] 
      (doall
        (map (partial draw-mbrot elem-size) 
           (with-screen-coords screen-dim
                               (with-coords (first @mbrot-data)))))
      (swap! mbrot-data rest))))




(comment
(doall (map 
  println 
  (split 3 
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
  (stroke-float 0)

  (no-loop))



(defapplet mb :title "mandelbrot"
  :setup setup :draw draw :size [screen-dim screen-dim]
  :key-pressed key-pressed)

(run mb)
;; (stop mb)
  

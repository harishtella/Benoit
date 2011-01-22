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
  (doall (map println (split 3 (map :c x)))))

(def screen-dim 800)

(def colors 
  (repeatedly 
    #(list (rand-int 256) (rand-int 256) (rand-int 256))))

(def base-color '(150 150 150))

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
  (+ (square-imaginary z) c))

(defn advance-elem [past-elem color-now]
  (let [{:keys [z c color escaped]} past-elem
        new-z (next-z z c)]
    (cond
      (escaped) {:z new-z
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


(defn do-mbrot-iters [[last-iter colors-list]]
  (let [color-now (first colors-list) ]
    [(map advance-elem last-iter (repeat color-now)) 
     (rest colors-list)]
    ))

(defn mbrot-iters [divs]
  (iterate do-mbrot-iters [(gen-init divs) colors]))  





(comment

(defn mbrot-drawer [divs]
  (let [mbrot-iters (atom (mbrot-iters divs))] 
    (fn [] (let [cur-data (first @mbrot-iters)]
             (map draw-tiles with-coords cur-data) 
             (swap! mbrot-iters rest)))))




(defn draw
  []
  (background-float 125)
  (stroke-float 10)
  (fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  (with-translation [(/ 200 2) (/ 200 2)]
    (with-rotation [QUARTER_PI]
      (begin-shape)
      (vertex -50  50)
      (vertex  50  50)
      (vertex  50 -50)
      (vertex -50 -50)
      (end-shape CLOSE)))
  (filter-kind INVERT))

(defn setup []
  "Runs once."
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 10))

(defapplet mb :title "mandelbrot"
  :setup setup :draw draw :size [screen-dim screen-dim])

(run mb)
;; (stop mb)
  
)

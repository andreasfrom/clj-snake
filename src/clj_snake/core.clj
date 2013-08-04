(ns clj-snake.core
  (:require [lanterna.screen :as s]
            [clojure.math.numeric-tower :as math])
  (:gen-class))
(set! *warn-on-reflection* true)

(def running (atom true))

(defrecord Snaker [tail dir dir-fn nav length score score-y color])
(defrecord Apple [fun bg fg])
(def ordinary-apple
  "Increments length and score by 1.
  Operates on a snake."
  (Apple. #(-> % (update-in [:length] inc) (update-in [:score] inc))
          :red
          :white))

(def point-apple
  "Increments length by 1 and score by 5!
  Operates on a snake."
  (Apple. #(-> % (update-in [:length] inc) (update-in [:score] (partial + 5)))
          :green
          :white))

(defn deplete!
  "Returns a vector of the results of running f until it returns nil."
  [f]
  (loop [c []] ; could be transient, but it shouldn't matter much
    (if-let [v (f)]
      (recur (conj c v))
      c)))

(defn in?
  "Returns a non-false value if c contains i.
  c must be a vector."
  [c i]
  (some #{i} c))

(defn cancels?
  "Returns true if the given vectors cancel each other out."
  [& vs]
  (every? zero? (apply (partial map +) vs)))

(defn user-dir
  "Returns a snake with the new direction.
  Takes a snake and a lanterna screen."
  [{old-dir :dir, nav :nav, :as snake} _ scr]
  (let [dirs (zipmap nav [[0 -1] [0 1] [-1 0] [1 0]])
        keyps (deplete! #(s/get-key scr))
        dir (get dirs (last (filter dirs keyps)) old-dir)
        dir (if (cancels? dir old-dir) old-dir dir) ; don't go backwards
        quit? (in? keyps :escape)]
    (when quit?
      (reset! running false))
    (assoc-in snake [:dir] dir)))

(defn sign [n]
  (cond
    (pos? n) 1
    (neg? n) -1
    :else 0))

(defn pos->dir
  [[a b] [x y]]
  (if (= b y)
    [(sign (- x a)) 0]
    [0 (sign (- y b))]))

(defn ai-dir
  "Very sophisticated..."
  [{old-dir :dir, tail :tail, :as snake} {pos :pos} _]
  (let [head (peek tail)
        dir (pos->dir head pos)
        dir (if (cancels? dir old-dir) old-dir dir)]
    (assoc-in snake [:dir] dir)))

(defn wrap
  "Returns n if between bottom and top, else bottom and top respectively."
  [n bottom top]
  (cond
   (> n top) bottom
   (< n bottom) top
   :else n))

(defn new-head
  "Appends to a snakes :tail based on its :dir.
  Wraps around the board."
  [{:keys [tail dir], :as snake} board-size]
  (let [head (map + (peek tail) dir)
        head (vec (map wrap head [0 0] board-size))]
    (update-in snake [:tail] conj head)))

(defn new-apple
  "Returns a new apple, not on the snake's tail.
  The apple is within :board-size.
  Takes a game."
  [{snakes :snakes, [x y] :board-size}]
  (let [pos
        (loop [pos (peek (:tail (first snakes)))] ;; start with an invalid value
          (if (not-empty (for [snake snakes
                               :while (in? (:tail snake) pos)]
                           true))
            (recur (vector (rand-int x) (rand-int y)))
            pos))
        typ (if (> (rand-int 10) 7)
              point-apple
              ordinary-apple)]
    {:pos pos :type typ}))

(defn pop-tail
  "Returns the new tail after popping the last bit off.
  Takes a snake."
  [{:keys [tail length] :as snake}]
  (let [tail-len (count tail)
        tail (if (> tail-len length)
               (subvec tail (- tail-len length))
               tail)]
    (assoc-in snake [:tail] tail)))

(defn hit-oneself
  "Slices the snakes tail and updates length and score if it hit itself.
  Takes a snake."
  [{tail :tail, :as snake}]
  (let [head  (peek tail)
        tail (butlast tail)
        hit (first (keep-indexed #(if (= head %2) %1) tail))] ;; return index of match
    (if hit
      (-> snake
          (update-in [:tail] subvec (inc hit))
          (update-in [:length] #(- % hit))
          (update-in [:score] #(- % hit)))
      snake)))

(defn hit-by-snake
  "Must not be called with itself."
  [{tail :tail, :as snake} {tail2 :tail}]
  (let [head (peek tail2)
        tail (butlast tail)
        hit (first (keep-indexed #(if (= head %2) %1) tail))] ;; return index of match
    (if hit
      (-> snake
          (update-in [:tail] subvec (inc hit))
          (update-in [:length] #(- % hit))
          )
      snake)))

(defn draw-snake
  "Draws a snake on a screen."
  [{:keys [tail score score-y color]} scr]
  (s/put-string scr 0 23 "ESC: Quit") ;; quit
  (s/put-string scr 0 score-y (str "Score: " score) {:fg color})
  (let [[head-x head-y] (peek tail)
        tail (butlast tail)]
    (doseq [[x y] tail]
      (s/put-string scr x y " " {:bg color})) ;; tail
    (s/put-string scr head-x head-y ":" {:bg color})))

(defn draw-apple
  [{[apple-x apple-y] :pos, {:keys [bg fg]} :type} scr]
  (s/put-string scr apple-x apple-y "@" {:bg bg :fg fg})) 

(defn now [] (System/currentTimeMillis))

(def ate-apple? (atom false))
(defn eat-apple
  "Apply apple's fun to snake if eaten."
  [{tail :tail, :as snake} {pos :pos, {fun :fun} :type}]
  (if (in? tail pos)
    (do (reset! ate-apple? true)
        (fun snake))
    snake))

;;(-main)
(defn -main
  []
  (let [scr (s/get-screen :swing)]
    (s/in-screen scr
                 (loop [game {:snakes [(Snaker. [[5 5]] [0 1] user-dir [:up :down :left :right] 10 0 0 :red)
                                       (Snaker. [[10 10]] [0 -1] ai-dir nil 10 0 1 :blue)]
                              :apple {:pos [20 7]
                                      :type ordinary-apple}
                              :board-size [79 23]}
                        last-frame (now)]
                   
                   (let [snakes (vec (map #(-> %1
                                               ((:dir-fn %1) (:apple game) scr)
                                               (new-head (:board-size game))
                                               (eat-apple (:apple game))
                                               (hit-oneself)
                                               (hit-by-snake %2)
                                               (pop-tail))
                                          (:snakes game) (reverse (:snakes game))))
                         game (assoc-in game [:snakes] snakes)
                         game (if @ate-apple?
                                (do (reset! ate-apple? false)
                                    (assoc-in game [:apple] (new-apple game)))
                                game)]

                     (s/clear scr)
                     (doseq [snake (:snakes game)]
                       (draw-snake snake scr))
                     (draw-apple (:apple game) scr)
                     (s/redraw scr)

                     (let [timeout (- 75 (- (now) last-frame))]
                       (when (pos? timeout)
                         (Thread/sleep timeout)))

                     (when @running
                       (recur game (now)))))))
  (reset! ate-apple? false)
  (reset! running true)) ;; dev

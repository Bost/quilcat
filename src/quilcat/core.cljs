(ns quilcat.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))


(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


(defn in?
  "true if coll contains elm"
  [coll elm] (some #(= elm %) coll))

;; (defn draw-line
;;   "Draws a horizontal line on the canvas at height h"
;;   [h]
;;   #_(q/bezier 0.17 0.67 0.83 0.67)
;;   #_(q/bezier 333 81 218 143 316 154 100 100)
;;   ;; (q/stroke 0 (- 255 h))
;;   ;; (q/line 10 h (- (q/width) 20) h)
;;   ;; (q/stroke 255 h)
;;   ;; (q/line 10 (+ h 4) (- (q/width) 20) (+ h 4))
;;   )

(def size {:x 800 :y 600})

(defn setup-sketch []
  (conj
   size
   {:elem1
    {:coord {:x 100 :y 100}
     :size {:width 50 :height 50}
     :r 20
     :moving true
     :active false}}
   {:elem2
    {:coord {:x 200 :y 200}
     :size {:width 50 :height 50}
     :r 20
     :moving true
     :active false}}
   {:elem3
    {:coord {:x 400 :y 400}
     :size {:width 50 :height 50}
     :r 20
     :moving true
     :active false}}))

(defn update-state [state] state)

;; TODO see https://github.com/kovasb/session
(defn draw-cat [x y]
  (let [oy 50 ox 25]
    (q/ellipse x y 120 250) ; surround
    ;; objs
    #_(q/ellipse x y 1 1)
    (q/ellipse (+ x ox) (- y oy) 10 10)
    (q/ellipse (- x (+ 8 ox)) y 10 10)
    (q/ellipse (+ x ox) (+ y oy) 10 10)))

(defn center [state kw]
  (/ (kw state) 2))

(defn draw-elem [state k]
  (let [x (get-in state [k :coord :x])
        y (get-in state [k :coord :y])
        xe (get-in state [k :size :width])
        ye (get-in state [k :size :height])
        black-stroke [0 0 0]
        red-stroke [255 0 0]
        white-stroke [255 255 255]]
    (apply q/stroke (if (get-in state [k :active]) black-stroke red-stroke))
    (q/rect x y xe ye)
    (apply q/stroke black-stroke)))

(defn draw-state [state]
  (q/background 255)

  (doseq [el (remove #(in? #{:x :y} %) (keys state))]
    (draw-elem state el))

  #_(let [offset 200
        x (- (center state :x) offset)
        y (center state :y)]
    (draw-cat x y)
    (draw-cat (+ (* 2 offset) x) y)))

(defn mouse-moved [state event]
  state
  #_(if (get-in [:elem1 :moving] state)
    (let [ex [:elem1 :coord :x]
          ey [:elem1 :coord :y]]
      (-> state
          (update-in ex (fn [] (:x event)))
          (update-in ey (fn [] (:y event)))))
    state))

(defn over? [elem event]
  (let [mx (:x event) my (:y event)
        ex (get-in elem [:coord :x])
        ey (get-in elem [:coord :y])
        size (:size elem)
        sx (get-in elem [:size :width])
        sy (get-in elem [:size :height])]
    (and (<= ex mx (+ ex sx))
         (<= ey my (+ ey sy)))))

(defn fn-toggle-active [elem state event]
  (if (over? elem event)
    (update-in elem [:active] (fn [] (not (get-in elem [:active]))))
    elem))

(defn map-values
  [m keys f & args]
  (reduce (fn [hm ks] (apply update-in hm [ks] f args)) m keys))

(defn mouse-clicked [state event]
  #_(println "mouse-clicked" "x" (:x event) "y" (:y event)
             "over?" (over? event (:elem1 state)))
  (let [elems (remove (fn [k] (in? #{:x :y} k)) (keys state))]
    (map-values state elems fn-toggle-active state event)))

(defn mouse-entered [state]
  #_(println "mouse-entered")
  state)

(q/defsketch example
  :host "quilcat"
  :size (vals size)
  :setup setup-sketch
  :draw draw-state
  :update update-state
  :mouse-moved mouse-moved
  :mouse-clicked mouse-clicked
  :mouse-entered mouse-entered
  :middleware [m/fun-mode])

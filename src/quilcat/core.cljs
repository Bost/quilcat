(ns quilcat.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))


(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

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
   {:elem
    {:coord {:x 100 :y 100}
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

(defn draw-state [state]
  (q/background 255)
  (let [x (get-in state [:elem :coord :x])
        y (get-in state [:elem :coord :y])
        xe (get-in state [:elem :size :width])
        ye (get-in state [:elem :size :height])
        black-stroke [0 0 0]
        red-stroke [255 0 0]
        white-stroke [255 255 255]
        ]
    #_(q/fill (apply q/color (if (get-in state [:elem :active])
                             [50 55 100]
                             [255 204 0])))
    #_(println "(get-in state [:elem :active])" (get-in state [:elem :active]))
    (apply q/stroke (if (get-in state [:elem :active]) black-stroke red-stroke))
    (q/rect x y xe ye)
    (apply q/stroke black-stroke))

  (let [offset 200
        x (- (center state :x) offset)
        y (center state :y)]
    (draw-cat x y)
    (draw-cat (+ (* 2 offset) x) y)))

(defn mouse-moved [state event]
  state
  #_(if (get-in [:elem :moving] state)
    (let [ex [:elem :coord :x]
          ey [:elem :coord :y]]
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

(defn active-elem [state event]
  (if (over? (:elem state) event)
    (not (get-in state [:elem :active]))))

(defn mouse-clicked [state event]
  #_(println "mouse-clicked" "x" (:x event) "y" (:y event)
             "over?" (over? event (:elem state)))
  (-> state
      ;; (update-in [:x] (fn [] (:x event)))
      ;; (update-in [:y] (fn [] (:y event)))
      (update-in [:elem :active] (fn [] (active-elem state event)))
      (update-in
       [:moving]
       (fn []
         (if (over? event (:elem state))
           (not (:moving state)))))))

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

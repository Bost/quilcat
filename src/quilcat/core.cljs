(ns quilcat.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [sablono.core :as sab]
            [clojure.set :as set])
  (:require-macros
   [devcards.core :refer [defcard deftest]]))

;; TODO take a look at deftest

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

(defn draw-rect [elem]
  (let [x  (get-in elem [:coord :x])
        y  (get-in elem [:coord :y])
        xe (get-in elem [:size :x])
        ye (get-in elem [:size :y])
        symbol (get-in elem [:symbol])]
    (q/rect x y xe ye)))

(defn draw-ellipse [elem]
  (let [x  (get-in elem [:coord :x])
        y  (get-in elem [:coord :y])
        xe (get-in elem [:size :x])
        ye (get-in elem [:size :y])
        symbol (get-in elem [:symbol])]
    (q/ellipse (+ x (/ xe 2)) (+ y (/ ye 2)) xe ye)))

(defn setup-sketch []
  (conj
   size
   {:active-elems #{}
    :arrows #{[:elem1 :elem2] [:elem2 :elem3] [:elem1 :elem3]}
    :elem1
    {:coord {:x 100 :y 100}
     :size {:x 50 :y 50}
     :drawfn draw-rect}}
   {:elem2
    {:coord {:x 400 :y 200}
     :size {:x 50 :y 50}
     :drawfn draw-rect}}
   {:elem3
    {:coord {:x 200 :y 400}
     :size {:x 50 :y 50}
     :drawfn draw-rect}}
   {:elem4
    {:coord {:x 0 :y 0}
     :size {:x 50 :y 50}
     :drawfn draw-ellipse}}))

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

(defn center [elem axis]
  (+ (get-in elem [:coord axis])
     (/ (get-in elem [:size axis]) 2)))

(def black-stroke [0 0 0])
(def red-stroke [255 0 0])
(def white-stroke [255 255 255])

(defn arrow [x1 y1 x2 y2]
  (q/line x1 y1 x2 y2)
  (q/push-matrix)
  (q/translate x2 y2)
  (q/rotate (q/atan2 (- x1 x2) (- y2 y1)))
  (let [arrow-size 4]
    (q/line 0 0 (- arrow-size) (- arrow-size))
    (q/line 0 0 (+ arrow-size) (- arrow-size)))
  (q/pop-matrix))

(defn elems [state]
  (remove (fn [e] (in? #{:x :y :active-elems :arrows} e)) (keys state)))

(defn draw-state [state]
  (q/background 255)

  (let [active-elems (get-in state [:active-elems])]
    (doseq [elem (elems state)]
      (apply q/stroke (if (in? active-elems elem) black-stroke red-stroke))
      (let [drawfn (get-in state [elem :drawfn])]
        (drawfn (get-in state [elem])))))
  (doseq [[src dst] (get-in state [:arrows])]
    (let [src (get-in state [src])
          dst (get-in state [dst])]
      (arrow (center src :x) (center src :y)
             (center dst :x) (center dst :y))))

  #_(apply q/stroke black-stroke)

  #_(let [offset 200
        x (- (center state :x) offset)
        y (center state :y)]
    (draw-cat x y)
    (draw-cat (+ (* 2 offset) x) y)))

(defn active-elems [state]
  (get-in state [:active-elems]))

(defn over? [elem event]
  (let [mx (:x event) my (:y event)
        ex (get-in elem [:coord :x])
        ey (get-in elem [:coord :y])
        size (:size elem)
        sx (get-in elem [:size :x])
        sy (get-in elem [:size :y])]
    (and (<= ex mx (+ ex sx))
         (<= ey my (+ ey sy)))))

(defn map-values
  [m keys f & args]
  (reduce (fn [hm ks] (apply update-in hm [ks] f args)) m keys))

(defn mouse-moved [state event]
  (map-values
   state (active-elems state)
   (fn [elem]
     (update-in
      elem [:coord]
      (fn []
        (let [{:keys [x y]} (get-in elem [:size])]
          {:x (- (:x event) (/ x 2))
           :y (- (:y event) (/ y 2))}))))))

;; TODO detect over lapping elems when on-clicked
(defn onclick [state event]
  (let [old-active (active-elems state)
        new-active (set (remove nil? (for [e (elems state)]
                                       (if (over? (e state) event)
                                         e))))]
    (update-in state [:active-elems]
               (fn []
                 #_new-active
                 (set/difference (set/union old-active new-active)
                                 (set/intersection old-active new-active))))))

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
  :mouse-clicked onclick
  :mouse-entered mouse-entered
  :middleware [m/fun-mode])

#_(defcard
  #_(setup-sketch)
  #_(draw-state (setup-sketch))
  #_(map-values {:x 0 :y 10 :z nil} [:x :y] + 20) ; initial data
  (map-values {:x 0 :y 10 :z nil} [:x :y] + 20))

#_(defcard devcard-X ;; optional symbol name
  #_"**Optional Mardown documentation**" ;; optional literal string doc
  ;; main obj
  #_(q/state-atom)
  (fn [data-atom owner]
    (sab/html
     [:div
      [:h3 "Example Counter w/Initial Data: " (:count @data-atom)]
      [:button
       {:onClick (fn [] (swap! data-atom update-in [:count] inc))}
       "inc"]]))
  ;; initial data
  {:count 10}
  ;; devcard options
  {
   ;; devcard-name is displayed only if :heading true :frame true
   :heading false :frame false :padding false
   :hidden false       ;; whether to diplay the card or not
   :inspect-data true  ;; whether to display the data in the card atom
   :watch-atom true    ;; whether to watch the atom and render on change
   :history false      ;; whether to record a change history of the atom
   :classname ""       ;; provide card with a custom classname
   })

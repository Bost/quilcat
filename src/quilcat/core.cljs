(ns quilcat.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            #_[sablono.core :as sab] ;; included via [devcards "0.2.1-7"]
            [clojure.set :as set])
  #_(:require-macros
   [devcards.core :refer [defcard deftest]]))

;; TODO take a look at deftest

(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn draw-name [elem]
  (let [{x :x y :y} (get-in elem [:center])
        name (get-in elem [:name])
        letter-height 5]
    (q/fill 0 0 0)
    (q/text name (- x (* 3 (count name))) (+ y letter-height))
    (q/no-fill)))

(defn draw-rect [elem]
  (let [{x :x y :y} (get-in elem [:center])
        {w :width h :height} (get-in elem [:size])]
    (draw-name elem)
    (q/rect (- x (/ w 2)) (- y (/ h 2)) w h)))

(defn draw-ellipse [elem]
  (let [{x :x y :y} (get-in elem [:center])
        {w :width h :height} (get-in elem [:size])]
    (draw-name elem)
    (q/ellipse x y h w)))

(def size {:x 800 :y 600})

;; TODO move setup-sketch to a clj / cljc file and use clojure.typed
(defn setup-sketch []
  (conj
   size
   {:active-elems #{}
    :arrows #{[:elem1 :elem2]
              ;; [:elem2 :elem3] [:elem1 :elem3] [:elem1 :elem4]
              }
    :elem1
    {:center {:x 200 :y 200}
     :size {:width 50 :height 50}
     :name "e1:t1"
     :drawfn draw-rect}}
   {:elem2
    {:center {:x 50 :y 250}
     :size {:width 50 :height 50}
     :name "e2:t2"
     :drawfn draw-rect}}
   #_{:elem3
    {:center {:x 200 :y 400}
     :size {:width 50 :height 50}
     :name "e3:t3"
     :drawfn draw-rect}}
   {:elem4
    {:center {:x 25 :y 25}
     :size {:width 50 :height 50}
     :name "e4:t4"
     :drawfn draw-ellipse}}))

(defn atan2-angle [sx sy dx dy]
  (q/atan2 (- sy dy) (- dx sx)))

(defn atan2-quadrant [angle]
  (let [p_pi1_4 (/ q/PI 4)
        p_pi3_4 (* 3 (/ q/PI 4))
        m_pi1_4 (* -1 p_pi1_4)
        m_pi3_4 (* -1 p_pi3_4)
        r (cond
            (< m_pi1_4 angle p_pi1_4) 1
            (< p_pi1_4 angle p_pi3_4) 2
            (or (> angle p_pi3_4)
                (< angle m_pi3_4)) 3
            (< m_pi3_4 angle m_pi1_4) 4
            )]
    #_(println "atan2-quadrant" "src" sx sy "dest" dx dy "angle" angle "r" r
               "p_pi1_4" p_pi1_4
               "p_pi3_4" p_pi3_4
               "m_pi1_4" m_pi1_4
               "m_pi3_4" m_pi3_4)
    r))

(defn in?
  "true if coll contains elm"
  [coll elm] (some #(= elm %) coll))

(defn arrow [x1 y1 x2 y2]
  (let [cx1 (+ x1 50)
        cy1 (+ y1 50)
        cx2 (- x2 50)
        cy2 (- y2 50)
        ]
    (q/bezier x1 y1 cx1 cy1 cx2 cy2 x2 y2))
  #_(q/line x1 y1 x2 y2)
  (q/push-matrix)
  (q/translate x2 y2)
  (q/rotate (q/atan2 (- x1 x2) (- y2 y1)))
  (let [arrow-size 4]
    (q/line 0 0 (- arrow-size) (- arrow-size))
    (q/line 0 0 (+ arrow-size) (- arrow-size)))
  (q/pop-matrix))

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

(defn rect? [elem]
  (in? #{draw-rect} (get-in elem [:drawfn])))

(defn center [elem axis]
  (get-in elem [:center axis]))

(def black-stroke [0 0 0])
(def red-stroke [255 0 0])
(def white-stroke [255 255 255])

(defn elems [state]
  (remove (fn [e] (in? #{:x :y :active-elems :arrows} e)) (keys state)))

;; t/Num t/Num t/Num t/Num -> t/Num
(defn quadrant [sx sy dx dy]
  (cond
    (and (< sx dx) (> sy dy)) 1
    (and (> sx dx) (> sy dy)) 2
    (and (> sx dx) (< sy dy)) 3
    (and (< sx dx) (< sy dy)) 4
    :else (do (println 'quadrant sx sy dx dy "s or d lie on the axis(es).")
              -1)))

(defn angle [sx sy dx dy]
  (println sx sy dx dy)
  #_(let [ang (q/atan2 (- dy sy) (- dx sx))]
    (println "angle" ang)))

(def sqrt (.-sqrt js/Math))

(defn hypotenuse [a b]
  (sqrt (+ (* a a) (* b b))))

(defn draw-state [state]
  (q/background 255)
  (let [active-elems (get-in state [:active-elems])]
    (doseq [elem (elems state)]
      (apply q/stroke (if (in? active-elems elem) black-stroke red-stroke))
      (let [drawfn (get-in state [elem :drawfn])]
        (drawfn (get-in state [elem])))))
  (apply q/stroke red-stroke)
  (doseq [[src dst] (get-in state [:arrows])]
    (let [{sx :x sy :y} (get-in state [src :center])
          {dx :x dy :y} (get-in state [dst :center])
          angle-src-dst (atan2-angle sx sy dx dy)

          quadrant-src-dst (atan2-quadrant angle-src-dst)
          r
          (let [{w :width h :height} (get-in state [src :size])
                w2 (/ w 2)
                h2 (/ h 2)]
            (cond
              (= 1 quadrant-src-dst) (hypotenuse w2 h2)
              (= 2 quadrant-src-dst) (hypotenuse w2 h2)
              (= 3 quadrant-src-dst) (hypotenuse w2 h2)
              (= 4 quadrant-src-dst) (hypotenuse w2 h2)
              (= -1 quadrant-src-dst) (hypotenuse w2 h2)
              :else (do
                      (println 'draw-state "Unknown quadrant: " quadrant-src-dst)
                      40)))

          src-angle (q/atan2 (- dx sx) (- dy sy))
          ssx (+ sx (* r (q/sin src-angle)))
          ssy (+ sy (* r (q/cos src-angle)))

          dst-angle (q/atan2 (- sx dx) (- sy dy))
          dsx (+ dx (* r (q/sin dst-angle)))
          dsy (+ dy (* r (q/cos dst-angle)))]
      (arrow ssx ssy dsx dsy)))

  #_(let [offset 200
        x (- (center state :x) offset)
        y (center state :y)]
    (draw-cat x y)
    (draw-cat (+ (* 2 offset) x) y)))

(defn active-elems [state]
  (get-in state [:active-elems]))

;; (t/ann over? [Elem -> t/Bool])
(defn over? [elem event]
  (let [{ex :x ey :y} event
        {cx :x cy :y} (get-in elem [:center])
        {w :width h :height} (get-in elem [:size])
        w2 (/ w 2)
        h2 (/ h 2)]
    (and (<= (- cx w2) ex (+ cx w2))
         (<= (- cy h2) ey (+ cy h2)))))

(defn map-values
  [m keys f & args]
  (reduce (fn [hm ks] (apply update-in hm [ks] f args)) m keys))

(defn mouse-moved [state event]
  (map-values
   state (active-elems state)
   (fn [elem]
     (update-in elem [:center] (fn [] {:x (:x event) :y (:y event)})))))

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

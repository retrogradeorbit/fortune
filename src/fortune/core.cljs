(ns ^:figwheel-always fortune.core
    (:require))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


(defn calculate-k
  "Given the turning point of a parabola specified as pos1 (x1, x2),
  and making it pass through second point pos2, return the
  coefficient k, such that the parabola could be described by
  y = k * (x - x1)^2 + y1"
  [[x1 y1] [x2 y2]]
  (/
   (- y2 y1)
   (*
    (- x2 x1)
    (- x2 x1))))

(defn parabola-y
  "Given a parabola [pos k], and a x position, x
  return the y position from the described parabola"
  [[[x1 y1] k] x]
  (+ y1 (* k (- x x1) (- x x1))))




;;
;; Beach head
;;

; The beach head can be described when we have l (the sweep line position)
; by a set of parabolas and points. p0, x0, p1, x1, p2, x2, p3.
;
;

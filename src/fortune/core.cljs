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

(defn parabola-intersection
  "Given two parabolas [pos1 k1] [pos2 k2]
  return the two solutions for x that satisfy both graphs
  using quadratic formula"
  [[[x1 y1] k1] [[x2 y2] k2]]
  (let [a (- k1 k2)
        b (- (* 2 k2 x2) (* 2 k1 x1))
        c (+ (* k1 x1 x1) (* (- k2) x2 x2) (- y2) y1)
        b2-4ac (- (* b b) (* 4 a c))]
    (if (= 0 a)
      (if (= b 0)
        ;; no solution
        #{}

        ;; one solution
        #{(/ (- c) b)})

      (if (= 0 b2-4ac)
        ;; one solution
        #{(/ (- b) (* 2 a))}

        ;; two solutions
        (let [sqrt-b2-4ac (Math/sqrt b2-4ac)]
          #{(/ (+ (- b) sqrt-b2-4ac) (* 2 a))
            (/ (- (- b) sqrt-b2-4ac) (* 2 a))})))))

(def a1 0)
(def a2 1)

(def b1 1)
(def b2 0)

(def c1 -1)
(def c2 0)

(defn circle-center
  "given three points that lie on the circumference
  of a circle, return the center location"
  [[a1 a2] [b1 b2] [c1 c2]]
  (let [
        ;; slopes of lines to center
        m1 (/ (- a1 b1) (- b2 a2))
        m2 (/ (- b1 c1) (- c2 b2))

        ;; points lines to center pass through (d and e)
        d1 (+ a1 (* 0.5 (- b1 a1)))
        d2 (+ a2 (* 0.5 (- b2 a2)))
        e1 (+ b1 (* 0.5 (- c1 b1)))
        e2 (+ b2 (* 0.5 (- c2 b2)))

        ;; the x position of the center
        x (/
           (- (* m1 d1) d2 (* m2 e1) (- e2))
           (- m1 m2))

        ;; the y position of the center
        y (- (* m1 x) (* m1 d1) (- d2))]
    [x y]))

;;
;; Beach head
;;

; The beach head can be described when we have l (the sweep line position)
; by a set of parabolas and points. p0, x0, p1, x1, p2, x2, p3.
;
;

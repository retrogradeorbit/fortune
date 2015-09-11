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

(defn circle-center
  "given three points that lie on the circumference of a circle,
  return the center location"
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
        x (if (= c2 b2)
            ;; m2 is infinite
            e1

            (if (= b2 a2)
              ;; m1 is infinite
              d1

              ;; m1 and m2 have finite slope
              (/
               (- (* m1 d1) d2 (* m2 e1) (- e2))
               (- m1 m2))))

        ;; the y position of the center
        y (if (= b2 a2)
            (- (* m2 x) (* m2 e1) (- e2))
            (- (* m1 x) (* m1 d1) (- d2)))]
    [x y]))

(defn compute-parabola-from-point-and-sweep
  "calculate the parabolic coefficients from the point and the
  position of the sweep line. returns [[x y] k] where x and y are the
  turning point and k is the coefficient"
  [[p q] l]
  (let [denom (-
               (* 2 q)
               (* 2 l))]
    [[
       ;; x
       p

       ;; y
       (/
        (-
         (* q q)
         (* l l))
        denom)
      ]

     ;; k
     (/ 1 denom)]))


(defn sweep-parabola-intersection [pos1 pos2 sweep]
  (parabola-intersection
   (compute-parabola-from-point-and-sweep pos1 sweep)
   (compute-parabola-from-point-and-sweep pos2 sweep)))
;;
;; Beach head
;;

; The beach head can be described when we have l (the sweep line position)
; by a set of parabolas and points. p0, x0, p1, x1, p2, x2, p3.
;
; p0, p1 are parabolas defined by points (and the line)
; x0, x1 are the x positions of the boundaries between the parabolas
;
; there is always an odd number of parts. All the even numbered indexes
; 0, 2, 4 etc are parabolas
; All the odd indexes 1, 3, 5 etc are x positions
;
; eg [ [10 4] 5 [12 6] 7 [10 4] ]

(defn add-to-beach-head [beach-head [x y]]
  (if (empty? beach-head)
    [[x y]]
    ;; binary search
    (let [num (count beach-head)
          half (dec (int (/ num 2)))
          [mx my] (nth beach-head half)]
      (if (< x mx)
        ;; on the left


        ;; on the right
        )
      ))
  )

(defn fortune [points ystart yend ydelta]
  (loop [beach-head []
         remaining (sort-by second points)]
    (let [[x y] (first remaining)]
      (recur (add-to-beach-head beach-head [x y])))))

(ns fortune.core-test
  (:require [cljs.test :refer-macros [deftest is]]
            [fortune.core :as core]))

(deftest calculate-k
  (is (= (core/calculate-k [0 0] [-1 1]) 1))
  (is (= (core/calculate-k [0 0] [1 1]) 1))
  (is (= (core/calculate-k [0 0] [1 -1]) -1))
  (is (= (core/calculate-k [0 0] [-1 -1]) -1))
  (is (= (core/calculate-k [1 1] [2 3]) 2)))

(deftest parabola-y
  (is (= (core/parabola-y [[0 0] 1] 0) 0))
  (is (= (core/parabola-y [[0 0] 1] 1) 1))
  (is (= (core/parabola-y [[0 0] 1] 2) 4))
  (is (= (core/parabola-y [[-10 -10] 1] -8) -6))
  (is (= (core/parabola-y [[0 0] 2] -1) 2))
  (is (= (core/parabola-y [[0 0] 3] -2) 12)))

(deftest parabola-intersection
  (is (= (core/parabola-intersection [[1 3] 1] [[0 0] 1]) #{2}))
  (is (= (core/parabola-intersection [[0 0] 1] [[0 0] 2]) #{0}))
  (is (= (core/parabola-intersection [[0 1] 1] [[0 0] 2]) #{-1 1}))
  (is (= (core/parabola-intersection [[0 1] 1] [[0 0] 1]) #{})))

(deftest circle-center
  (is (= (core/circle-center [0 1] [1 0] [-1 0]) [0 0]))
  (is (= (core/circle-center [1 0] [-1 0] [0 1]) [0 0]))
  (is (= (core/circle-center [1 1] [-1 1] [-1 -1]) [0 0]))
  (is (= (core/circle-center [5 5] [6 -2] [2 -4]) [2 1]))
  )

(deftest compute-parabola-from-point-and-sweep
  (is (= (core/compute-parabola-from-point-and-sweep [0 10] 9)
         [[0 9.5] 0.5]))
  (is (= (core/compute-parabola-from-point-and-sweep [0 10] 8)
         [[0 9.0] 0.25]))
  (is (= (core/compute-parabola-from-point-and-sweep [5 10] 6)
         [[5 8.0] 0.125])))

(deftest sweep-parabola-intersection
  (is (= (sort (core/sweep-parabola-intersection [0 1] [0 2] 0))
         (list (- (Math/sqrt 2)) (Math/sqrt 2))))
  (is (= (sort (core/sweep-parabola-intersection [5 2] [5 4] 0))
         (list (- 5 (* 2 (Math/sqrt 2))) (+ 5 (* 2 (Math/sqrt 2)))))))

(deftest compute-parabola
  (let [[x1 x2] (vec (core/sweep-parabola-intersection [5 5] [10 10] 4))]
    (is
     (core/almost
      (core/parabola-y (core/compute-parabola-from-point-and-sweep [5 5] 4) x1)
      (core/parabola-y (core/compute-parabola-from-point-and-sweep [10 10] 4) x1)))
    (is
     (core/almost
      (core/parabola-y (core/compute-parabola-from-point-and-sweep [5 5] 4) x2)
      (core/parabola-y (core/compute-parabola-from-point-and-sweep [10 10] 4) x2)))))

(deftest update-beach-intersections
  (let [beach [[10 10] 0.5999 [6 9] 6.0001 [10 10]]
        sweep 8
        [p0 x0 p1 x1 p2] (core/update-beach-intersections beach sweep)]
    (is (= p0 p2))
    (is (< x0 x1))
    (is (core/almost
         (core/parabola-y (core/compute-parabola-from-point-and-sweep p0 sweep) x0)
         (core/parabola-y (core/compute-parabola-from-point-and-sweep p1 sweep) x0)))
    (is (core/almost
         (core/parabola-y (core/compute-parabola-from-point-and-sweep p1 sweep) x1)
         (core/parabola-y (core/compute-parabola-from-point-and-sweep p2 sweep) x1)))))

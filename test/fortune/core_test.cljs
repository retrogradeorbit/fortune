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

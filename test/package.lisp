(defpackage aoc2023/test
  (:use :cl
   :fiveam)
  (:import-from :aoc2023 #:calibration-value
                #:evaluate-calibration-document
                #:sum-of-ids-of-valid-games
                #:sum-of-part-numbers
                #:card-values))

(in-package aoc2023/test)

(def-suite aoc2023
  :description "Test suite for Advent of Code 2023")

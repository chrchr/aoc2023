(in-package aoc2023/test)

(def-suite* day1 :in aoc2023)

(test calibration-value
  (is (= (calibration-value "1abc2") 12)))

(test calibration-value-no-digits
  (is (= (calibration-value "hello there are no digits today") 0)))

(test calibration-value-spelled-out
  (is (= (calibration-value "two1nine") 29)))

(test calibration-value-spelled-out-zero
  (is (= (calibration-value "two1ninezero") 29)))


(test evaluate-calibration-document
  (is
   (= (evaluate-calibration-document "test-calibration-document.txt")
      142)))

(test evaluate-spelled-out-calibration-document
  (is
   (= (evaluate-calibration-document "day1-spelled-out-calibration-document.txt")
      281)))

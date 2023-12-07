(in-package aoc2023/test)

(def-suite* day4 :in aoc2023/test)

(test card-values
  (is
   (= 13 (card-values "day4-sample-cards.txt"))))

(test they-use-different-numbers-of-cards
  (is
   (= 0 (aoc2023::card-value (make-string-input-stream "Card   1: 42 68 56  3 28 97  1 78 55 48 | 78 54  5 38 94 73 72 57 51 31 86 43  7 81  4 27 26 58 75 69 74 55  5 28 40")))))

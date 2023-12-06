(in-package aoc2023/test)

(def-suite* day3 :in aoc2023)

(test sum-of-part-numbers
  (is
   (= (aoc2023:sum-of-part-numbers "day3-sample-schematic.txt") 4361)))

(test sum-of-part-numbers-more
  (is
   (= (aoc2023:sum-of-part-numbers "day3-subset.txt") (+ 633 803 361 192 539 973 340 313))))


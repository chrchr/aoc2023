(in-package aoc2023/test)

(def-suite* day3 :in aoc2023)

(defun do-string (str)
  (aoc2023::sum-of-part-numbers-stream (make-string-input-stream str)))

(test sum-of-part-numbers
  (is
   (= (aoc2023:sum-of-part-numbers "day3-sample-schematic.txt") 4361)))

(test numbers-left
  (is
   (= (do-string "*1") 1)))

(test numbers-right
  (is
   (= (do-string "1*") 1)))

(test numbers-upper-left
  (is
   (= (do-string (format nil "*.~%.1")) 1)))

(test numbers-above
  (is
   (= (do-string (format nil "*~%1")) 1)))

(test numbers-upper-right
  (is
   (= (do-string (format nil ".*~%1.")) 1)))

(test numbers-below-left
  (is
   (= (do-string (format nil ".1~%*.")) 1)))


(test numbers-below
  (is
   (= (do-string (format nil "1~%*")) 1)))

(test numbers-below-right
  (is
   (= (do-string (format nil "1.~%.*")) 1)))

(test multiply-symbols
  (is
   (= (do-string (format nil "*1*~%***")) 1)))


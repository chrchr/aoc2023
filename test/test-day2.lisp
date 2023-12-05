(in-package aoc2023/test)

(def-suite* day2 :in aoc2023)

(test valid-game
  (let ((games (make-string-input-stream "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))
    (is
     (= 1 (aoc2023::validate-game 100 100 100 games)))))

(test invalid-game
  (let ((games (make-string-input-stream "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))
    (is
     (= 0 (aoc2023::validate-game 1 1 5 games)))))

(test sum-of-ids-of-valid-games
  (is
   (= (aoc2023:sum-of-ids-of-valid-games 12 13 14 "day2-sample-games.txt") 8)))


(test sum-of-minimum-cube-sets
  (is
   (= (aoc2023:sum-of-minimum-cube-sets "day2-sample-games.txt") 2286)))

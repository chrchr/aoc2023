;; --- Day 3: Gear Ratios ---

;; You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

;; It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

;; "Aaah!"

;; You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

;; The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

;; The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

;; Here is an example engine schematic:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

;; Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

;; --- Part Two ---

;; The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.

;; You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.

;; Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.

;; The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.

;; This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.

;; Consider the same engine schematic again:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces 467835.

;; What is the sum of all of the gear ratios in your engine schematic?

(in-package aoc2023)

(defun symbol-char-p (character)
  ;; Not really clear in the spec what a symbol is. It's not a number or a '.'!
  ;; (and (char/= character #\.)
  ;;      (char/= character #\Nul)
  ;;      (not (digit-char-p character))
  ;;      (char/= character #\Newline)))
  (gear-char-p character))

(defparameter +gear-char+ #\*)

(defun gear-char-p (character)
  (char= character +gear-char+))

(defun allocate-empty-string (&optional (initial-capacity 2048))
  (make-array initial-capacity :adjustable t :fill-pointer 0 :element-type 'character))

(defun take-number-near (line offset)
  (when (digit-char-p (char line offset))
      (take-number
         line 
         (1+ (or (position-if
                  (complement #'digit-char-p) line :from-end t :end offset)
                 -1))
         (or (position-if
              (complement #'digit-char-p) line :from-end nil :start offset)
             (length line)))))

(defun take-number (line beginning-of-number end-of-number)
  (prog1
      (parse-integer line :start beginning-of-number :end end-of-number)
    ;; Erase the number so that it doesn't get reported as valid a second time
    ;; due to a symbol appearing on the following line in the specification
    (fill line #\. :start beginning-of-number :end end-of-number)))

(defun find-numbers-adjacent-to-symbol (symbol-index lines line-number)
  (let ((line-length (length (aref lines line-number)))
        (numbers (list)))
    (loop for y from (max 0 (1- line-number))
            upto (min (1+ line-number) (1- (length lines))) do
              (loop for x from (max 0 (1- symbol-index))
                      upto (min (1+ symbol-index) (1- line-length)) do
                        (let ((number (take-number-near (aref lines y) x)))
                          (when (not (null number))
                            (push number numbers)))))
    ;; the elegance
    (if (= (length numbers) 2)
        (apply #'* numbers)
        0)))

(defun analyze-schematic-line (lines line-number)
  (let ((line (elt lines line-number))
        (index 0))
    (loop for symbol-index = (position-if #'symbol-char-p line :start index)
          until (or (null symbol-index) (>= index (length line)))
          do (setf index (1+ symbol-index))
          sum (find-numbers-adjacent-to-symbol symbol-index lines line-number))))

(defun sum-of-part-numbers-stream (stream)
  (let ((lines (make-array 64 :adjustable t :fill-pointer 0)))
    (loop for line = (read-line stream nil :eof)
          for line-number from 0
          until (eq line :eof)
          do (vector-push-extend line lines))
    (loop for line-number from 0 upto (1- (length lines))
          sum (analyze-schematic-line lines line-number))))
      
(defun blah-blah-blah-no-not-this-way (stream)
  ;; Okay now I will need three lines of input.
  (let ((previous-line (allocate-empty-string))
        (line (allocate-empty-string))
        (sum-of-part-numbers 0))

      (loop for c = (peek-char nil stream nil :eof) do
        (cond ((eq c :eof)
               (return sum-of-part-numbers))
              ((char= c #\Newline)
               (read-char stream)
               (let ((old-line previous-line))
                 (setf previous-line line)
                 (setf (fill-pointer old-line) 0)
                 (setf line old-line)))
              ((char= c #\.)
               (read-dots stream line))
              ((symbol-char-p c)
               (incf sum-of-part-numbers (read-symbol stream line previous-line)))
              ((digit-char-p c)
               (incf sum-of-part-numbers (read-part-number stream line previous-line)))))))
  
(defun sum-of-part-numbers (filename)
  (with-open-file (stream filename :direction :input :external-format :utf8)
    (sum-of-part-numbers-stream stream)))

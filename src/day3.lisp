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

(in-package aoc2023)

(defun symbol-char-p (character)
  ;; Not really clear in the spec what a symbol is. It's not a number or a '.'!
  (and (char/= character #\.)
       (char/= character #\Nul)
       (not (digit-char-p character))
       (char/= character #\Newline)))

(defun read-dots (stream line)
  (loop for c = (peek-char nil stream nil #\Nul) until (char/= c #\.) do
    (vector-push-extend (read-char stream) line)))

(defun allocate-empty-string (&optional (initial-capacity 2048))
  (make-array initial-capacity :adjustable t :fill-pointer 0 :element-type 'character))

(defun symbol-above-p (previous-line start end)
  (and (find-if #'symbol-char-p previous-line :start (max (1- start) 0) :end (min (1+ end) (length previous-line))) t))

(defun digit-above-p (previous-line offset)
  (digit-char-p (char previous-line offset)))

(defun take-number-near (line offset)
  (if (and (not (zerop (length line))) (digit-char-p (char line offset)))
      (let ((not-digit-char-p (lambda (c) (not (digit-char-p c)))))
        (take-number
         line 
         (1+ (or (position-if not-digit-char-p line :from-end t :end offset) -1))
         (or (position-if not-digit-char-p line :from-end nil :start offset) (length line))))
      0))

(defun take-number (line beginning-of-number end-of-number)
  (prog1
      (parse-integer line :start beginning-of-number :end end-of-number)
    ;; Erase the number so that it doesn't get reported as valid a second time
    ;; due to a symbol appearing on the following line in the specification
    (fill line #\. :start beginning-of-number :end end-of-number)))

(defun read-part-number (stream line previous-line)
  (let ((beginning-of-number-offset (length line)))
    (loop for c = (peek-char nil stream nil #\Nul)
          while (digit-char-p c)
          do (vector-push-extend (read-char stream) line))
    (if (or
         ;; The previous character is a symbol
         (and (not (zerop beginning-of-number-offset))
              (symbol-char-p (char line (1- beginning-of-number-offset))))
         ;; There's a symbol on the previous line
         (and (not (zerop (length previous-line))) (symbol-above-p previous-line beginning-of-number-offset (length line)))
         ;; The next character is a symbol
         (symbol-char-p (peek-char nil stream nil #\Nul)))
        (take-number line beginning-of-number-offset (length line))
        0)))

(defun read-symbol (stream line previous-line)
  (+
   (if (not (zerop (length line)))
       (take-number-near previous-line (1- (length line)))
       0)
   (loop for c = (peek-char nil stream nil #\Nul)
         while (symbol-char-p c)
         sum
         (prog1
             (take-number-near previous-line (length line))
           (vector-push-extend (read-char stream) line)))
     (if (< (length line) (length previous-line))
         (take-number-near previous-line (length line))
         0)))

(defun sum-of-part-numbers-stream (stream)
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

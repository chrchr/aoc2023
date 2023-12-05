(in-package aoc2023/test)

(def-suite* stream-search :in aoc2023)

(test stream-search-table
  (is (equalp (stream-search::stream-search-table "ABCDABD")
              #(-1 0 0 0 -1 0 2 0))))

(test stream-search
  (let ((w "ABCDABD")
        (s (make-string-input-stream "ABC ABCDAB ABCDABCDABDE")))
    (is (= (stream-search::stream-search w s) 15))))

(test stream-search-next-char-in-stream
  (let ((w "ABC")
        (s (make-string-input-stream "ABCDEF")))
    (stream-search:stream-search w s)
    (is (eq #\D (read-char s)))))

(test stream-search-end-of-stream
  (let ((w "ABC")
        (s (make-string-input-stream "     ABC")))
    (is (= 5 (stream-search:stream-search w s)))))

(test stream-search-not-found
  (is (not (stream-search::stream-search "not found"
                          (make-string-input-stream "now is the time for all good men to come to the aid of their country")))))


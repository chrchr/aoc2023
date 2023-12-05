(defpackage stream-search
  (:use :cl)
  (:export #:stream-search))

(in-package stream-search)

(defun stream-search-table (needle)
  (let ((table (make-sequence 'simple-vector (1+ (length needle))))
        (candidate-index 0))
    (setf (aref table 0) -1)
    (loop for table-position from 1 upto (1- (length needle))
          do (cond ((eq (char needle table-position) (char needle candidate-index))
                    (setf (aref table table-position) (aref table candidate-index)))
                   (t
                    (setf (aref table table-position) candidate-index)
                    (loop while (and (>= candidate-index 0)
                                     (not (eq (char needle table-position)
                                              (char needle candidate-index))))
                          do (setf candidate-index (aref table candidate-index)))))
             (incf candidate-index))
    (setf (aref table (length needle)) candidate-index)
    table))


(defun stream-search (needle stream)
  "An implementation of KMP search that operates on streams."
  (handler-case
      (let ((j 0) ;; position of the current character in the stream
            (k 0) ;; position of the current character in the needle
            (table (stream-search-table needle))
            (c (read-char stream)) ;; candidate character
            (needle-length (length needle)))
        (loop do (cond ((eq (char needle k) c)
                        (incf j)
                        (incf k)
                        (when (= k needle-length)
                          (return (- j k)))
                        (setf c (read-char stream)))
                       (t
                        (setf k (aref table k))
                        (when (< k 0)
                          (incf j)
                          (incf k)
                          (setf c (read-char stream)))))))
    (end-of-file nil)))


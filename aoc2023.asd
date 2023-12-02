(asdf:defsystem #:aoc2023
  :description "Advent of Code 2023"
  :author "Robert Church <chrchr@gmail.com>"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "day1"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:aoc2023/test))))

(asdf:defsystem #:aoc2023/test
  :depends-on (:aoc2023
               :fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-day1"))))
  :perform (asdf:test-op :after (op c)
                         (symbol-call :fiveam :run!
                                      (find-symbol* :aoc2023 :aoc2023/test))))
                       

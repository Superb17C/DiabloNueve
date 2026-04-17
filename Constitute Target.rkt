#lang racket

; cons-all: value (list (list value)) -> (list (list value))
; insert given "element" at beginning of every given list
(define (cons-all element lists)
  (if (empty? lists)
      empty
      (cons (cons element
                  (first lists))
            (cons-all element
                      (rest lists)))))

; constitute-target: non-negative-number (list positive-number) -> (list (list positive-number))
; find complete list of ways to exactly reach "target" number by summing elements from library of "addends"
(define (constitute-target target addends)
  (if (= target 0) ; there is exactly 1 way to make 0 from positive addends: by summing none of the addends
      (list empty)
      (if (or (< target 0) ; performance improvement: there are exactly 0 ways to make a negative number from positive addends
              (empty? addends)) ; there are exactly 0 ways to make a positive number from no addends
          empty
          (append (cons-all (first addends) ; append's 1st argument = all ways to make target using 1st addend
                            (constitute-target (- target (first addends))
                                               (rest addends)))
                  (constitute-target target ; append's 2nd argument = all ways to make target without using 1st addend
                                     (rest addends))))))

; main program
(constitute-target 9111 (list 2100 3100 4100 5100 6100 7100 8100 0100 1100 1100 1100
                              2010 3010 4010 5010 6010 7010 8010 0010 1010 1010 1010
                              2001 3001 4001 5001 6001 7001 8001 0001 1001 1001 1001))
#lang racket

  ; type Words = (Listof String)
  ; type Lines = (Listof Words)

  ; Path-String -> (Listof String)
  (define (kwic-read filename)
    (with-input-from-file filename
      (λ ()
        (for/list ([line (in-lines)])
          line))))

  ; (Listof String) -> Lines
  (define (kwic-split lines)
    (map string-split lines))

  ; Move first element to last position
  ; Words -> Words
  (define (circular-shift words)
    (append (rest words) (list (first words))))

  ; Words -> (Listof Words)
  (define (all-circular-shifts words)
    (for/fold ([all-shifts (list words)])
              ([i (in-range 1 (length words))])
      (cons (circular-shift (first all-shifts))
            all-shifts)))

  ; Lines -> Lines
  (define (alphabetize all-shifts)
    (sort all-shifts shift<?))

  ; Lexicographic order on lists of words
  ; Words Words -> Boolean
  (define (shift<? shift1 shift2)
    (match* (shift1 shift2) ; destruct multiple values
     [('() _) ; first list empty, don't care about second
      #t]
     [(_ '()) ; first list non-empty, second empty
      #f]
     [((cons s1 shift1-rest) (cons s2 shift2-rest))
      (or (string<? s1 s2)
          (and (string=? s1 s2)
               (shift<? shift1-rest shift2-rest)))]))

  ; Lines -> Void
  (define (kwic-display all-sorted-shifts)
    (define (display-words words)
      (display (first words))
      (for ([word (in-list (cdr words))])
        (display " ")
        (display word))
      (newline))
    ; for-each is like map, but always returns (void)
    (for-each display-words all-sorted-shifts))

  ; Lines -> (Listof Lines)
  (define (all-circular-shifts* lines)
    (map all-circular-shifts lines))

  ; Path-String -> Void
  (define (kwic-index file-name)
    (define all-lines (kwic-split (kwic-read file-name)))
    (define all-shifts (append* (all-circular-shifts* all-lines)))
    (kwic-display (alphabetize all-shifts)))

  ; End-to-end test
  ; -> Void
  (define (run-test)
    (define test-file "test.txt")
    ; Make a file and test
    (unless (file-exists? test-file)
      (with-output-to-file test-file
        (λ ()
          (displayln "imagine if this")
          (displayln "took 2 weeks to write"))))
    (kwic-index test-file))

  (run-test)




    (module+ test
      ; Open a submodule named 'test'
      (require rackunit)
      (check-equal?
        (circular-shift '("A" "B" "C"))
        '("B" "C" "A")))



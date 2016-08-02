#lang typed/racket

  (module+ test
    (require typed/rackunit)

    (define-syntax-rule (check-equal?* [i o] ...)
      (begin
        (check-equal? i o)
        ...)))

  (define-type Words (Listof String))
  (define-type Lines (Listof Words))

  (: kwik-read : Path-String -> (Listof String))
  (define (kwik-read filename)
    (with-input-from-file filename
      (λ ()
        (for/list ([line (in-lines)])
                  : (Listof String)
          line))))

  (module+ test
    (let ([tmpfile (make-temporary-file)])
      (with-output-to-file tmpfile #:exists 'replace
        (λ ()
          (displayln "The Nellie,")
          (displayln "a cruising yawl,")
          (displayln "swung to her anchor without a flutter of sails,")
          (displayln "and was at rest.")))
      (define actual (kwik-read tmpfile))
      (define expect (file->lines tmpfile))
      (delete-file tmpfile)
      (check-equal? actual expect)))

  (: kwik-split : (Listof String) -> Lines)
  (define (kwik-split lines)
    (map #{string-split :: (String -> Words)} lines))

  (module+ test
    (check-equal?*
      [(kwik-split '())
       '()]
      [(kwik-split '("hello    world"))
       '(("hello" "world"))]))

  ; Move first element to last position
  (: circular-shift : Words -> Words)
  (define (circular-shift words)
    (append (rest words) (list (first words))))

  (module+ test
    (check-equal?*
      [(circular-shift '("A" "B" "C"))
       '("B" "C" "A")]))

  (: all-circular-shifts : Words -> (Listof Words))
  (define (all-circular-shifts words)
    (for/fold ([all-shifts (list words)])
              ([i (in-range 1 (length words))])
              : (Listof Words)
      (cons (circular-shift (first all-shifts))
            all-shifts)))

  (module+ test
    (check-equal?*
      [(all-circular-shifts '("A" "B" "C"))
       '(("C" "A" "B") ("B" "C" "A") ("A" "B" "C"))]))

  (: alphabetize : Lines -> Lines)
  (define (alphabetize all-shifts)
    (sort all-shifts shift<?))

  (module+ test
    (check-equal?*
      [(alphabetize '(("A" "B" "C") ("B" "C") ("A")))
       '(("A") ("A" "B" "C") ("B" "C"))]))

  ; Lexicographic order on equal-length lists of words
  (: shift<? : Words Words -> Boolean)
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

  (module+ test
    (check-equal?*
      [(shift<? '() '())
       #t]
      [(shift<? '("A" "B") '("A" "C"))
       #t]))

  (: kwik-display : Lines -> Void)
  (define (kwik-display all-sorted-shifts)
    (: display-words : Words -> Void)
    (define (display-words words)
      (display (first words))
      (for ([word (in-list (cdr words))])
        (display " ")
        (display word))
      (newline))
    ; for-each is like map, but always returns (void)
    (for-each display-words all-sorted-shifts))

  (module+ test
    (parameterize ([current-output-port (open-output-string)])
      (kwik-display '(("A") ("B" "C")))
      (check-equal?
        (get-output-string (current-output-port))
        "A\nB C\n")))

  (: all-circular-shifts* : Lines -> (Listof Lines))
  (define (all-circular-shifts* lines)
    (map all-circular-shifts lines))

  (module+ test
    (check-equal?
      (all-circular-shifts* '(("A" "B" "C") ("D")))
      '((("C" "A" "B") ("B" "C" "A") ("A" "B" "C")) (("D")))))

  (: kwik-index : Path-String -> Void)
  (define (kwik-index file-name)
    (define all-lines (kwik-split (kwik-read file-name)))
    (define all-shifts (append* (all-circular-shifts* all-lines)))
    (kwik-display (alphabetize all-shifts)))

  (module+ test
    (parameterize ([current-output-port (open-output-string)])
      (define tmpfile (make-temporary-file))
      (with-output-to-file tmpfile #:exists 'replace
        (λ ()
          (displayln "imagine if this")
          (displayln "took 2 weeks to write")))
      (kwik-index tmpfile)
      (delete-file tmpfile)
      (check-equal?
        (get-output-string (current-output-port))
        (string-join '(
          "2 weeks to write took"
          "if this imagine"
          "imagine if this"
          "this imagine if"
          "to write took 2 weeks"
          "took 2 weeks to write"
          "weeks to write took 2"
          "write took 2 weeks to\n") "\n"))))

  (module+ main
    (require racket/cmdline)
    (: *output-to* (Parameterof Any))
    (define *output-to* (make-parameter #f))
    (command-line
      #:program "kwik index"
      #:once-each
      [("-o" "--output")
       output-to
       "Write output to file"
       (*output-to* output-to)]
      #:args (file-name)
      (define output-to (*output-to*))
      (define out-port
        (if (string? output-to)
          (open-output-file output-to #:exists 'replace)
          (current-output-port)))
      (parameterize ([current-output-port out-port])
        (kwik-index (cast file-name Path-String)))
      (when (string? output-to)
        (close-output-port out-port))))

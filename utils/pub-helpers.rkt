#lang racket

(provide
 ;; (Union String ByteString) -> (Listof publication)
 ;;
 ;; Given the path portion of a URI for an author's XML file on DBLP some author as a string or
 ;; byte-string (e.g. "/pers/xx/s/Shivers:Olin.xml"), returns a list of publication structs for all
 ;; the publications on that DBLP page we are able to read (currently supports article, book, and
 ;; inproceedings types).
 scrape-dblp

 ;; (Listof publication) output-port -> void
 ;;
 ;; Writes the given publications to the given port in a format that can easily be copied and pasted
 ;; into publications.rkt
 write-pubs)

;; ---------------------------------------------------------------------------------------------------

(require xml
         net/http-client
         "../publication-data.rkt")

(define (write-pubs pubs port)
  (for ([pub pubs])
  (match-define (publication title authors venue year link) pub)
  (fprintf port
"(publication ~s
             ~s
             ~s
             ~s
             ~s)\n"
           title authors venue year link)))

;; ---------------------------------------------------------------------------------------------------
;; DBLP

(define supported-record-types `(article book inproceedings))

(define (scrape-dblp uri)
  (define-values (status headers xml-stream) (http-sendrecv "dblp.uni-trier.de" uri))
  (define pub-records
    (map (compose1 first element-content)
         (get-children-of-name (document-element (read-xml xml-stream)) 'r)))
  (for/list ([record pub-records] #:when (member (record-type record) supported-record-types))
    (dblp-record->pub record)))

(define (dblp-record->pub record)
  (define authors
    (map (compose1 pcdata-string first element-content) (get-children-of-name record 'author)))
  (define author-text
    (match (length authors)
      [1 (first authors)]
      [2 (string-append (first authors) " and " (second authors))]
      [_ (string-join authors ", " #:before-last ", and ")]))
  (define venue
    (match (element-name record)
      ['article (element-text (get-child-of-name record 'journal))]
      ['book (element-text (get-child-of-name record 'publisher))]
      ['inproceedings
       (let ([booktitle (element-text (get-child-of-name record 'booktitle))])
         (hash-ref booktitle-venue-map booktitle booktitle))]
      [other-type (error 'record->pub "Received unsupported record type ~s" other-type)]))
  (define url
    (match (get-children-of-name record 'ee)
      [(list url) (element-text url)]
      [_ #f]))
  (publication (element-text (get-child-of-name record 'title))
               author-text
               venue
               (string->number (element-text (get-child-of-name record 'year)))
               url))

(define booktitle-venue-map
  (hash
   "OOPSLA" "Object-Oriented Programming Systems, Languages, and Applications (OOPSLA)"
   "PLDI" "Programming Language Design and Implementation (PLDI)"
   "ICFP" "International Conference on Functional Programming (ICFP)"
   "POPL" "Principles of Programming Languages (POPL)"
   "ESOP" "European Symposium on Programming (ESOP)"
   "JFP" "Journal of Functional Programming (JFP)"
   "ECOOP" "European Conference on Object-Oriented Programming (ECOOP)"
   "SNAPL" "Summit on Advances in Programming Langugages (SNAPL)"
   "CC" "International Conference on Compiler Construction (CC)"
   "SIGCSE" "SIGCSE"
   "TOPLAS" "Transactions on Programming Languages and Systems (TOPLAS)"
   "IFL" "Implementation and Application of Functional Languages (IFL)"
   "PADL" "Practical Aspects of Declarative Languages (PADL)"
   "TFP" "Trends in Functional Programming (TFP)"
   "DLS" "Dynamic Languages Symposium (DLS)"
   "TLDI" "Types in Language Design and Implementation (TLDI)"
   "Scheme" "Scheme and Functional Programming Workshop"
   "PPDP" "Principles and Practice of Declarative Programming (PPDP)"
   "ACL2" "ACL2 Workshop"
   "FDPE" "Functional and Declarative Programming in Education (FDPE)"
   "GPCE" "Generative Programming: Concepts & Experience (GPCE)"
   "LMCS" "Logical Methods in Computer Science (LMCS)"
   "MSCS" "Mathematical Structures in Computer Science (MSCS)"
   "LICS" "Logic in Computer Science (LICS)"
   "DBPL" "Database Programming Languages (DBPL)"
   "SIGCSE" "ACM Technical Symposium on Computer Science Education (SIGCSE)"))

(define (record-type r) (element-name r))

;; ---------------------------------------------------------------------------------------------------
;; XML Helpers

;; TODO: replace these helpers with some kind of XPath library (e.g. the sxml or xepr-path packages)

(define (get-children-of-name the-element name)
   (filter
    (lambda (e) (and (element? e) (eq? (element-name e) name)))
    (element-content the-element)))


(define (get-child-of-name the-element name)
  (maybe-get-child-of-name the-element
                           name
                           (lambda ()
                             (error 'get-child-of-name
                                    "Child ~s not found in ~s"
                                    name
                                    the-element))))

(define (maybe-get-child-of-name the-element name default)
  (match (get-children-of-name the-element name)
    [(list) (if (procedure? default) (default) default)]
    [(list child) child]
    [_ (error 'get-child-of-name
              "multiple children found in ~s, looking for ~s"
              the-element
              name)]))

(define (element-text e)
  (pcdata-string (first (element-content e))))

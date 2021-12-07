#lang racket/gui
(require csc151)
(require 2htdp/image) 
(require csc151/rex)
(require racket/set)

;;; Project Idea
;;; Ask the user to input how many sections they want to divide the book into
;;; And how many sentences they want to see in each section's summary 
;;; Summarize each section of the book
;;; The user can ask for a specific number of keywords in the text and an image will be displayed

;;; Instructions for Using GUI:
;;; 1. Run this Racket file, and a GUI should pop up 
;;; 2. The GUI will ask for some information about the text file (use "test-sentences.txt" as an example)
;;; 3. The GUI will also ask for an output file name (name it anything with a ".txt" ending)
;;; 4. Use the slider to choose the number of sections, sentences, and keywords
;;; 5. The file where the summary and keywords visualizer will appear at the bottom after you click the button "Submit"
;;; HOPE YOU ENJOY OUR PROJECT!!! 

;; project.rkt
;
;; Mini Project 8
;
;; CSC-151 Fall 2021
;; Mini Project 8
;; Author: Quang Le, Jeronimo Camargo, Jack Gillespie
;; Date: 2021-12-05
;; Acknowledgements:
;; Professor Sam Rebelsky
;; Linh Tang
;; Mini Project 3, 5
;; Course Syllabus
;; Racket Documentation 

;; +-------+-------------------------------------------------------------
;; | Files | 
;; +-------+

;;; Test File
(define test-sentence "text-sentences.txt")

;;; A list of stop words
(define stop-words
  (file->words "stop-words.txt"))

;; +-------------+------------------------------------------------------
;; | Procedure 1 | REMOVE STOP WORDS FROM SENTENCES AND FILE 
;; +-------------+------------------------------------------------------

;;; (split-word str) -> list?
;;; str : string?, a text written in one string
;;; Return a list of words found in the text 
(define split-word
  (lambda (str)
    (map string-downcase
         (rex-split-string (rex-any-of (rex-repeat (rex-string " "))
                                       (rex-repeat (rex-string "\n"))
                                       (rex-repeat (rex-string "\r"))
                                       (rex-repeat (rex-string "\t"))
                                       (rex-concat (rex-repeat (rex-char-set ".,;:!?_\"<>|/^%$#"))
                                                   (rex-repeat-0 (rex-string " ")))) str))))

;;; (file->sentence fname) -> list?
;;; fname : string?, name of file
;;; Return a list of sentence from a file 
(define file->sentence 
  (lambda (fname)
    (string-split (regexp-replace* #px"(\\?|\\!|\\.)"
                                   (string-normalize-spaces (file->string fname))
                                   "\\1\\1")
                  #px"(\\?|\\!|\\.)[\\s]")))

;;; (clean-words-in-sentence) -> list?
;;; sentence : string?
;;; Return a list of non-stop-words from the sentence 
(define clean-words-in-sentence
  (let ([non-stop-word? (lambda (str)
                          (eq? (index-of stop-words str) #f))])
    (o (section filter non-stop-word? <>) split-word)))

;;; (clean-words-in-sentence) -> listof list?
;;; fname : string?, name of file
;;; Return a list of lists of non-stop-words inside sentences inside 'fname'
(define clean-words-in-file
  (lambda (fname)
    (let [(sentences (file->sentence fname))]
      (flatten (map clean-words-in-sentence sentences)))))

;; +-------------+-------------------------------------------------------------
;; | Procedure 2 | FINDING THE COSINE SIMILIARITY VALUE BETWEEN TWO SENTENCES
;; +-------------+-------------------------------------------------------------

; Two test sentences for procedures 
(define test-sentence1 "Welcome to CSC-151-01, Grinnell College’s introductory computer science course.")
(define test-sentence2 "In this course, we will work to develop your experience with algorithmic problem solving.")

;;; (create-vectors sentence1 sentence2) -> list?
;;; sentence1 : string?
;;; sentence2 : string?
;;; Return a list with two vectors (represented as lists):
;;; The first vector contains counts of words in both sentences that show in 'sentence1'
;;; The second vector contains counts of words in both sentences that show in 'sentence2'
(define create-vectors
  (lambda (sentence1 sentence2)
    (let* ([lst1 (clean-words-in-sentence sentence1)]
           [lst2 (clean-words-in-sentence sentence2)]
           [union-words (set->list (set-union (list->set lst1)
                                              (list->set lst2)))]
           [tally-word (lambda (lst)
                         (map (section tally-value lst <>)
                              union-words))]
           [vector1 (tally-word lst1)]
           [vector2 (tally-word lst2)])
      (list vector1 vector2))))
           
;;; (similarity-index lst1 lst2) -> number?
;;; sentence1 : string?
;;; sentence2 : string?
;;; Return how similiar two sentences are using the cosine similarity formula
;;; Taking the dot product of two vectors (from the two sentences) and dividing
;;; that product by the product of vector 1 (lst1) and vector 2 (lst2)
(define similarity-index
  (lambda (sentence1 sentence2)
    (let* ([vectors (create-vectors sentence1 sentence2)]
           [lst1 (first vectors)]
           [lst2 (second vectors)]
           [dist (lambda (lst)
                   (sqrt (reduce + (map sqr lst))))]
           [prod (reduce + (map * lst1 lst2))])
      (/ prod
         (* (dist lst1)
            (dist lst2))))))

;; +-------------+-------------------------------------------------------------------------------
;; | Procedure 3 | GENERATE A SUMMARY FROM GIVEN SENTENCES (CHOOSE THE MOST RELAVANT SENTENCES)
;; +-------------+-------------------------------------------------------------------------------

;;; (sum-similiarity sentence text) -> number?
;;; sentence : string?
;;; text : list?, a list of sentences
;;; Return the total Cosine Similiarity Index for the sentence against the entire text
(define sum-similarity
  (lambda (sentence text)
    (if (null? text)
        0
        (+ (similarity-index sentence (car text))
           (sum-similarity sentence (cdr text))))))

;;; (hash-similarity-index sentences) -> hash?
;;; sentences : listof string?
;;; Return a hash table with the cosine similarity index of each sentence as
;;; values and the position in the list of each sentence as keys
(define hash-similarity-index
  (lambda (sentences)
    (let ([index-list (map cons
                           (range 0 (length sentences))
                           (map (section sum-similarity <> sentences) sentences))])
      (make-hash index-list))))

;;; (summary-generator sentences n) -> string?
;;; sentences : listof string?
;;; n : integer?
;;; Return 'n' number of sentences with the highest cosine similarity index 
(define summary-generator
  (lambda (sentences n)
    (let* ([sort-value (sort (hash->list (hash-similarity-index sentences))
                             #:key cdr >)]
           [new-n (if (< (length sentences) n)
                      (length sentences)
                      n)]
           [sort-index (sort (take sort-value new-n) #:key car <)]
           [summary (string-join (map (section list-ref sentences <>)
                                      (map car sort-index)))])
      summary)))

;; +-------------+------------------------------------------------------
;; | Procedure 4 | CREATE LIST OF SECTION SUMMARIES
;; +-------------+------------------------------------------------------

;;; (sublist lst start end) -> list?
;;; lst : list?
;;; start : 
(define sublist
  (lambda (lst start end)
    (if (= start end)
        '()
        (cons (list-ref lst start) (sublist lst (add1 start) end)))))

;;; (chunk-it lst n) -> list?
;;; lst : list?
;;; n : integer?
;;; Return 'lst' but broken into 'n' approximately equal sublists
(define chunk-it
  (lambda (lst n)
    (letrec ([avg (/ (length lst) n)]
             [helper (lambda (lst start n)
                       (if (> n (length lst))
                           null
                           (cons (sublist lst (round start) (round n))
                                 (helper lst
                                         (+ start avg)
                                         (+ n avg)))))])
      (helper lst 0 avg))))

;;; (summarize-book-section fname num-section num-sentence) -> list?
;;; fname : string?
;;; num-section : integer?
;;; num-sentence : integer?
;;; Return a list of 'num-section', in which each element is a 'num-sentence' summary of the equally divided sections 
(define summarize-book-section
  (lambda (fname num-section num-sentence)
    (let* ([text (file->sentence fname)]
           [sections (chunk-it text num-section)])
      (map (section summary-generator <> num-sentence) sections))))

;; +-------------+------------------------------------------------------
;; | Procedure 5 | FORMATTING SUMMARY AND PUSH TO FILE 
;; +-------------+------------------------------------------------------

;;; (summary-format summary n) -> string?
;;; summary : list?
;;; n : integer?
;;; Return a formatted summary with 'n' sections each with several sentences 
(define summary-format
  (lambda (summary n)
    (reduce string-append
            (map (section string-append "Section " <> ":\n" <> "\n\n")
                 (map number->string (inclusive-range 1 n))
                 summary))))
                   
;;; (GUI-output input output num-section num-sentence) -> string?
;;; input : string?, input filename
;;; output : string?, output filename
;;; num-section : integer?, number of sections with a summary 
;;; num-sentence : integer?, number of sentences in each section's summary
;;; Return a formatted string with information like:
;;; Total number of sentences
;;; Number of sections
;;; Number of sentences in each summary
;;; A cummulative summary 
(define GUI-output
  (lambda (input output num-section num-sentence)
    (string->file (string-append "THIS IS YOUR GENERATED SUMMARY OF THE BOOK\n\n"
                                 "The total number of sentences in this book is "
                                 (number->string (length (file->sentence input)))
                                 ". There will be a summary for each of the "
                                 (number->string num-section)
                                 " section(s).\n\nSummary:\n\n"
                                 (summary-format (summarize-book-section input num-section num-sentence)
                                                 num-section)) output)))

;; +-------------+------------------------------------------------------
;; | Procedure 6 | CREATE LIST OF WORDS AND THEIR OCCURENCES
;; +-------------+------------------------------------------------------

; A test list to test procedures
(define test-list '("course" "welcome" "csc-151-01"
                             "grinnell" "college’s" "introductory"
                             "computer" "science"
                             "course"))
                    

;;; (dedup lst) -> list?
;;; lst : list?
;;; Return 'lst' except with no duplicates 
(define dedup
  (lambda (lst)
    (if (null? lst)
        null
        (cons (car lst)
              (dedup (filter                               ;; Filters and places the items that are not 
                      (lambda (item)                       ;; the same as (car lst) into a list that is 
                        (not (equal? (car lst) item)))     ;; then used in the recursive call
                      lst))))))

;;; (count-word lst) -> list?
;;; lst : list?
;;; Return a list of pairs with the car being the word in the list and the cdr being their occurences in 'lst'
(define count-word
  (lambda (lst)
    (let [(remove-duplicates (dedup lst))]
      (map cons
           remove-duplicates
           (map (section tally-value lst <>) remove-duplicates)))))

;;; (tally-keywords lst) -> list?
;;; lst : lst?
(define tally-keywords
  (lambda (fname n)
    (let* [(word-list (clean-words-in-file fname))
           (counts (count-word word-list))
           (sorted-list (sort counts #:key cdr >))]
      (take sorted-list n))))

;; +-------------+------------------------------------------------------
;; | Procedure 7 | CREATE KEYWORDS VISUALIZER AND PUSH TO FILE 
;; +-------------+------------------------------------------------------

;;; (rec height color) -> image?
;;; height : pos integer?
;;; color : symbol/string color
;;; returns a solid rectangle with a width of 20 with customized height and color
(define rec
  (lambda (height color)
    (rectangle 50 height 'solid color)))

;;; (bars hash-table) -> image?
;;; hash-table : hash?
;;; returns an image with a bar for each key in hash-table. Also has the key and value written
;;; in text displayed above each bar.
(define bars
  (lambda (lst)
    (let* ([keys (map car lst)]
           [values (map cdr lst)]
           [max-value (apply max values)]
           [scaler (/ max-value 500)])
      (letrec ([bars-helper
                (lambda (keys values)
                  (if (null? (cdr values))
                      (above (text (car keys) 15 "teal")
                             (text (number->string (car values))  15 "teal")
                             (rec (/ (car values) scaler) "spring green"))
                      (beside/align 'bottom (above (text (car keys) 15 "teal")
                                                   (text (number->string (car values))  15 "teal")
                                                   (rec (/ (car values) scaler) "spring green"))
                                    (rectangle 60 500 0 "white")
                                    (bars-helper (cdr keys) (cdr values)))))])
        (bars-helper keys values)))))

;;; (bar-graph lst) -> image?
;;; hash-table : hash?
;;; Return a bar graph with different bars as the different occurrences of the word in the text
(define bar-graph
  (lambda (lst)
    (if (null? lst)
        (text "NO KEY WORDS INPUT" 15 "crimson")
        (let* ([values (map cdr lst)]
               [number-of-rectangles (length values)]
               [width (* 115 number-of-rectangles)]
               [bargraph-bottom (rectangle width 50 'solid "crimson")])
          (above
           (text "BAR GRAPH OF KEY WORDS" 20 "crimson")
           (overlay/align 'center
                          'bottom
                          (bars lst)
                          (rectangle width 550 'outline "white"))
           bargraph-bottom)))))

;;; (save-graph fname n) -> boolean?
;;; fname : string?
;;; n : integer?
;;; Save an image of the bar graph as "keywords.jpg"
(define save-graph
  (lambda (fname n)
    (let* ([word-pairs (tally-keywords fname n)]
           [graph (bar-graph word-pairs)])
      (save-image graph "keywords.jpg"))))

;; +------------+---------------------------------------------------------
;; | EXTRA WORK | GENERATE USER INTERFACE
;; +------------+---------------------------------------------------------

;;; This GUI is something extra that we did on top of our book summary.
;;; This GUI basically serves as an organized way for the user to input their file
;;; choose the summary setting and keywords all in one place. The result would also be
;;; visible to them in a clear and organized manner. 

; The frame of the GUI 
(define GUI 
  (new frame% 
       [label "Book Summary Generator"]
       [style (list 'fullscreen-button)]))

; The message that states the name of the project 
(define objective
  (new message%
       (parent GUI)
       (label "MINI PROJECT 8: BOOK SUMMARIZER AND KEY WORD VISUALIZER")
       (vert-margin 20)
       (horiz-margin 5)
       (color "light yellow")
       (min-width  300)	 
       (min-height 10)))

; The message that states the creators of this project 
(define author
  (new message%
       (parent GUI)
       (label "This project is built by Quang Le, Jeronimo Camargo, and Jack Gillespie.")
       (vert-margin 20)
       (horiz-margin 5)
       (color "light yellow")
       (min-width  300)	 
       (min-height 10)))

; The message that states the disclaimer
(define disclaimer
  (new message%
       (parent GUI)
       (label "***DISCLAIMER: Please make sure your text file has standard spacing and punctutation.***")
       (vert-margin 20)
       (horiz-margin 5)
       (color "tomato")
       (min-width  300)	 
       (min-height 10)))

; The text field that allows the user to put in the text file they want to summarize 
(define input
  (new text-field%
       (label "Insert your book here (filename): ")
       (parent GUI)
       (init-value "")
       (vert-margin 10)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 30)))

; The text field that allows the user to put in the name of the file the summary will be saved to
(define output
  (new text-field%
       (label "Save your book summary at (filename): ")
       (parent GUI)
       (init-value "")
       (vert-margin 10)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 30)))    

; The slider that allows the user to choose the number of sections summarized in the book 
(define number-of-sections
  (new slider%
       (label "Number of sections of summary in the book: ")
       (parent GUI)
       (min-value 1)
       (max-value 20)
       (init-value 5)
       (style '(horizontal))
       (vert-margin 10)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 30)))

; The slider that allows the user to choose the number of sentences to be in each summarized section 
(define number-of-sentences
  (new slider%
       (label "Number of sentences in the summary: ")
       (parent GUI)
       (min-value 1)
       (max-value 20)
       (init-value 5)
       (style '(horizontal))
       (vert-margin 10)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 30)))

; The slider that allows the user to choose the nubmer of keywords and their occurences in the text 
(define number-of-keywords
  (new slider%
       (label "Number of key words for analysis: ")
       (parent GUI)
       (min-value 0)
       (max-value 20)
       (init-value 5)
       (style '(horizontal))
       (vert-margin 10)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 30)))

; This is an auto generated text that changes to let the user know when their summary is ready 
(define user-message-1
  (new message%
       (parent GUI)
       (label "")
       (vert-margin 10)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 40)))

; This is an auto generated text that changes to let the user know where their summary is saved
(define summary-output-path
  (new message%
       (parent GUI)
       (label "")
       (vert-margin 0)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 5)))

; This is an auto generated text that changes to let the user know where their graph is saved
(define graph-output-path
  (new message%
       (parent GUI)
       (label "")
       (vert-margin 0)
       (horiz-margin 5)
       (min-width  300)	 
       (min-height 5)))

; This is a button that gathers all the information the user had inputed above and would start generating the
; summary and keywords graph 
(define submit-button
  (new button%
       [parent GUI]
       [label "Submit"]
       [callback
        (lambda (b e)
          (send user-message-1 set-label "Generating...")
          (GUI-output
           (send input get-value)
           (send output get-value)
           (send number-of-sections get-value)
           (send number-of-sentences get-value))
          (save-graph
           (send input get-value)
           (send number-of-keywords get-value))
          (send user-message-1 set-label "Your summary and graph has been ganerated.\nPlease check your files at: ")
          (send summary-output-path set-label (send output get-value))
          (send graph-output-path set-label "keywords.jpg")
          (newline))]))

; This line helps run the GUI 
(send GUI show #t)









                              
                              
          






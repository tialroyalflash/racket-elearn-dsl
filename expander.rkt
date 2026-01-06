#lang racket
(require (for-syntax syntax/parse))

;; =============================================================================
;; DSL Exports
;; =============================================================================
;; Standard Racket forms and custom DSL keywords for the e-learning system
(provide (all-from-out racket)
         pathway if-score course-status assignment calculate-grade 
         process-csv-report)

;; =============================================================================
;; State Management
;; =============================================================================
;; Thread-safe boxes to manage student state during batch processing
(define current-student-score (box 0))
(define current-student-name (box ""))

;; =============================================================================
;; Macro Definitions (Syntax)
;; =============================================================================

;; 1. Pathway Control Macro: Logic for branching based on results
(define-syntax (pathway stx)
  (syntax-parse stx
    [(_ condition:expr then-part:expr else-part:expr)
     #'(let ([result condition])
         (printf ">>> [PATHWAY] ")
         (if result
             (printf "Decision: PASS. Recommended: ~a\n" then-part)
             (printf "Decision: FAIL. Recommended: ~a\n" else-part)))]))

;; 2. Score Evaluation Helper: Accesses the current student's score in the box
(define-syntax (if-score stx)
  (syntax-parse stx
    [(_ op:id threshold:number)
     #'(op (unbox current-student-score) threshold)]))

;; 3. Individual Assignment Macro: Used for calculating specific task scores
(define-syntax (assignment stx)
  (syntax-parse stx
    [(_ name:str #:weight w:number #:score s:number)
     #'(list name (* w s))]))

;; 4. Grade Calculation Macro: Aggregates multiple assignments (Legacy Support)
(define-syntax (calculate-grade stx)
  (syntax-parse stx
    [(_ assignments ...)
     #'(let* ([results (list assignments ...)]
              [total (apply + (map second results))])
         (set-box! current-student-score total)
         (printf "\n--- Manual Assessment Report ---\n")
         (for ([res results])
           (printf "Component: ~a | Weighted Score: ~a\n" (first res) (second res)))
         (printf "Total Calculated Grade: ~a\n" total))]))

;; =============================================================================
;; Internal Core Logic
;; =============================================================================

;; Internal function to process UoPeople grading logic for CSV rows
(define (internal-calculate-grade name scores)
  (let* ([w-lj (* 0.20 (list-ref scores 0))] ; Learning Journal
         [w-df (* 0.10 (list-ref scores 1))] ; Discussion Forum
         [w-pa (* 0.30 (list-ref scores 2))] ; Programming Assignment
         [w-gq (* 0.40 (list-ref scores 3))] ; Graded Quiz
         [total (+ w-lj w-df w-pa w-gq)])
    (set-box! current-student-name name)
    (set-box! current-student-score total)
    (printf "\n--- Report for Student: ~a ---\n" name)
    (printf "Calculated Final Grade: ~a\n" total)
    total))

;; =============================================================================
;; CSV Processing Logic
;; =============================================================================

;; Orchestrates the batch processing of students from a CSV file
(define (process-csv-report file-path)
  (let* ([lines (file->lines file-path)]
         [data-lines (cdr lines)]) ; Skip header
    (for ([line data-lines])
      (let* ([fields (string-split line ",")]
             [name (string-trim (first fields) "\"")]
             [scores (map string->number (rest fields))])
        
        ;; Perform grading logic
        (internal-calculate-grade name scores)
        
        ;; Execute branching logic
        (pathway (if-score > 70)
                 "Proceed to NEXT COURSE"
                 "Assign REMEDIAL MODULES")))))

;; =============================================================================
;; Runtime Functions
;; =============================================================================

(define (course-status name)
  (printf "\n--- Course Status Check: ~a ---\n" name))

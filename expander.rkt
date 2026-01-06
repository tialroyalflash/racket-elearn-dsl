#lang racket
(require (for-syntax syntax/parse))
(require racket/date) ; Required for timestamps

;; =============================================================================
;; DSL Exports
;; =============================================================================
(provide (all-from-out racket)
         pathway if-score course-status assignment calculate-grade 
         process-csv-report)

;; =============================================================================
;; State Management
;; =============================================================================
(define current-student-score (box 0))
(define current-student-name (box ""))

;; =============================================================================
;; Macro Definitions (Syntax)
;; =============================================================================

;; 1. Pathway Control Macro
(define-syntax (pathway stx)
  (syntax-parse stx
    [(_ condition:expr then-part:expr else-part:expr)
     #'(let ([result condition])
         (define msg (if result
                         (format "Decision: PASS. Recommended: ~a" then-part)
                         (format "Decision: FAIL. Recommended: ~a" else-part)))
         (printf ">>> [PATHWAY] ~a\n" msg)
         msg)]))

;; 2. Score Evaluation Helper
(define-syntax (if-score stx)
  (syntax-parse stx
    [(_ op:id threshold:number)
     #'(op (unbox current-student-score) threshold)]))

;; 3. Individual Assignment Macro
(define-syntax (assignment stx)
  (syntax-parse stx
    [(_ name:str #:weight w:number #:score s:number)
     #'(list name (* w s))]))

;; 4. Grade Calculation Macro (Added back to fix the error)
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

(define (internal-calculate-grade name scores)
  (let* ([w-lj (* 0.20 (list-ref scores 0))]
         [w-df (* 0.10 (list-ref scores 1))]
         [w-pa (* 0.30 (list-ref scores 2))]
         [w-gq (* 0.40 (list-ref scores 3))]
         [total (+ w-lj w-df w-pa w-gq)])
    (set-box! current-student-name name)
    (set-box! current-student-score total)
    (define report (format "\n--- Report for Student: ~a ---\nCalculated Final Grade: ~a\n" name total))
    (display report)
    report))

;; =============================================================================
;; CSV Processing & File Logging Logic
;; =============================================================================

(define (process-csv-report input-path)
  (let* ([output-path "data/results.txt"]
         [lines (file->lines input-path)]
         [data-lines (cdr lines)])
    
    (call-with-output-file output-path
      #:exists 'replace
      (lambda (out-port)
        ;; Add a header with a date
        (date-display-format 'iso-8601)
        (fprintf out-port "UoPeople DSL Report - Generated on ~a\n" (date->string (current-date) #t))
        (fprintf out-port "==================================================\n")
        
        (for ([line data-lines])
          (let* ([fields (string-split line ",")]
                 [name (string-trim (first fields) "\"")]
                 [scores (map string->number (rest fields))])
            
            (define grade-report (internal-calculate-grade name scores))
            (display grade-report out-port)
            
            (define pathway-msg (pathway (if-score > 70)
                                         "Proceed to NEXT COURSE"
                                         "Assign REMEDIAL MODULES"))
            (fprintf out-port ">>> [PATHWAY] ~a\n" pathway-msg)))))
    
    (printf "\n[SUCCESS] Results saved to ~a\n" output-path)))

;; =============================================================================
;; Runtime Functions
;; =============================================================================
(define (course-status name)
  (printf "\n--- Course Status Check: ~a ---\n" name))

#lang s-exp "expander.rkt"

;; Initialize the simulation status
(course-status "UoPeople Computer Science Pathway Simulation")

;; Trigger the batch processing of students from the target CSV directory
;; The logic iterates through each student and generates a personalized report
(process-csv-report "data/students.csv")

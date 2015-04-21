#lang racket
;;PPL COURSEWORK 2015 RACKET BY ZSOLT BALVANYOS, USER ID: zbalva01

;;Declaring the sample input
(define input '((0 2 5 0 0 1 0 0 0) (1 0 4 2 5 0 0 0 0) (0 0 6 0 0 4 2 1 0) (0 5 0 0 0 0 3 2 0) (6 0 0 0 2 0 0 0 9) (0 8 7 0 0 0 0 6 0) (0 9 1 5 0 0 6 0 0) (0 0 0 0 7 8 1 0 3) (0 0 0 6 0 0 5 9 0)))

;;Sends the to-be-solved sudoku into the loop and send the result to the build function to create a nested list from it.
(define (solve matrix)
  (build (flatten (trans-back (looping (flatten (transform matrix))) '()))))
    
;;Changes the sets back to integers.
(define (trans-back set-matrix list-matrix)
  (if (empty? set-matrix)
      list-matrix
      (trans-back (cdr set-matrix) (cons list-matrix (set-first (car set-matrix))))))
    
;;Changes the integers to sets.
(define (transform matrix)
  (build (trans-helper (flatten matrix))))

(define (trans-helper matrix)
  (cond ((empty? matrix) '())
        ((= (car matrix) 0) (cons (set 1 2 3 4 5 6 7 8 9) (trans-helper (cdr matrix))))
        (else (cons (set (car matrix)) (trans-helper (cdr matrix))))))

;;Creates a matrix of 9*9 from the list of 81 elements.
(define (build long-list)
  (if (empty? long-list)
      '()
      (cons (take long-list 9) (build (drop long-list 9)))))

;=================================================================================================================================================================

;;Loops the process of scanning the matrix for sets that have a single element or sets that have a 
;;unique element in their row, column or box.
(define (looping matrix)
  (if (= (count-single matrix 0 1) 81)
      matrix
      (let ([scanned-for-single-matrix (scan-for-single matrix 1)])
        (if (equal? scanned-for-single-matrix matrix)
            (scan-for-unique matrix 1)
            (looping scanned-for-single-matrix)))))
      

;;Counts the number of sets that has 1 element. This has to be 81 to determine the looping.
(define (count-single matrix total index)
  (if (empty? matrix)
      total
      (if (= (set-count (car matrix)) 1)
          (count-single (cdr matrix) (+ total 1) (+ index 1))
          (count-single (cdr matrix) total (+ index 1)))))
  

;;Searches for sets with more than one element, when finds one it calls the remove-common-elements function.
(define (scan-for-unique matrix index)
  (if (= index 82) 
       matrix
       (let ([fat-set (car (take (drop matrix (- index 1)) 1))])
                (if (= (set-count fat-set) 1) 
                    (scan-for-unique matrix (+ 1 index))
                    (let ([revised-set (remove-common-elements 
                                        matrix
                                        fat-set
                                        fat-set
                                        fat-set
                                        (get-row-number index)
                                        (get-column-number index) 
                                        (get-box-number index)
                                        1)])
                      (if (= (set-count revised-set) 1)
                          (looping (append (take matrix (- index 1)) (list revised-set) (drop matrix index)))
                          (scan-for-unique matrix (+ 1 index))))))))


;;Subtracts the other sets from the subject set in its row, column or box to find if there is any unique number. If it cannot find one it returns an empty set.
(define (remove-common-elements matrix row-set column-set box-set row column box index)
  (cond ((= index 82) (cond ((= (set-count row-set) 1) row-set)
                            ((= (set-count column-set) 1) column-set)
                            ((= (set-count box-set) 1) box-set)
                            (else '())))
                            
        ((and (= row (get-row-number index))(= column (get-column-number index))) (remove-common-elements 
                                                                                   (cdr matrix) 
                                                                                   row-set column-set box-set 
                                                                                   row column box 
                                                                                   (+ 1 index)))
        
        ((and (= box (get-box-number index))(= row (get-row-number index))) (remove-common-elements 
                                                                             (cdr matrix) 
                                                                             (set-subtract row-set (car matrix)) column-set (set-subtract box-set (car matrix)) 
                                                                             row column box 
                                                                             (+ 1 index)))
       
        ((and (= box (get-box-number index))(= column (get-column-number index))) (remove-common-elements 
                                                                                   (cdr matrix) 
                                                                                   row-set (set-subtract column-set (car matrix)) (set-subtract box-set (car matrix)) 
                                                                                   row column box 
                                                                                   (+ 1 index)))
       
        ((= box (get-box-number index)) (remove-common-elements 
                                         (cdr matrix) 
                                         row-set column-set (set-subtract box-set (car matrix)) 
                                         row column box 
                                         (+ 1 index)))
       
        ((= row (get-row-number index)) (remove-common-elements 
                                         (cdr matrix) (set-subtract row-set (car matrix)) column-set box-set 
                                         row column box 
                                         (+ 1 index)))
        
        ((= column (get-column-number index)) (remove-common-elements 
                                               (cdr matrix) 
                                               row-set (set-subtract column-set (car matrix)) box-set 
                                               row column box 
                                               (+ 1 index)))
        (else (remove-common-elements 
               (cdr matrix) 
               row-set column-set box-set 
               row column box 
               (+ 1 index)))))
    

;;Searches for single sets and calls for the remove-element function where necessary.
(define (scan-for-single matrix index)
  (cond ((= index 82) matrix)
        ((= (set-count (car (take (drop matrix (- index 1)) 1))) 1) (scan-for-single (remove-element 
                                                                                      matrix
                                                                                      (car (take (drop matrix (- index 1)) 1))
                                                                                      (get-row-number index)
                                                                                      (get-column-number index) 
                                                                                      (get-box-number index)
                                                                                      1) 
                                                                                     (+ 1 index)))
        (else (scan-for-single matrix (+ 1 index)))))
 

;;Subtracts a one-element-long set from sets in the same row, column or box.
(define (remove-element matrix single-set row column box index)
  (cond((empty? matrix) '())
       ((equal? (car matrix) single-set) (cons (car matrix) (remove-element
                                                             (cdr matrix) 
                                                             single-set 
                                                             row column box 
                                                             (+ 1 index))))
       ((= row (get-row-number index)) (cons (set-subtract (car matrix) single-set) (remove-element 
                                                                                     (cdr matrix) 
                                                                                     single-set 
                                                                                     row column box 
                                                                                     (+ 1 index))))
       ((= column (get-column-number index)) (cons (set-subtract (car matrix) single-set) (remove-element 
                                                                                           (cdr matrix) 
                                                                                           single-set 
                                                                                           row column box 
                                                                                           (+ 1 index))))
       ((= box (get-box-number index)) (cons (set-subtract (car matrix) single-set) (remove-element 
                                                                                     (cdr matrix) 
                                                                                     single-set 
                                                                                     row column box 
                                                                                     (+ 1 index))))
       (else (cons (car matrix) (remove-element 
                                 (cdr matrix) 
                                 single-set 
                                 row column box 
                                 (+ 1 index))))))
  

;;Returns the row number of an element in a flattened list representation of the matrix.       
(define (get-row-number index)
  (floor (+ (/ (- index 1) 9) 1)))

;;Returns the column number of an element in a flattened list representation of the matrix. 
(define (get-column-number index)
  (remainder index 9))

;;Returns the box number of an element in a flattened list representation of the matrix. 
(define (get-box-number index)
  (+ (* 10 (floor (+ (/ (- index 1) 27) 1))) (remainder (floor (+ (/ (- index 1) 3) 1)) 3)))

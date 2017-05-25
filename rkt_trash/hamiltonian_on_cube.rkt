#lang racket
(require racket/trace)

(define (add_prefix pre lst)
  (map (lambda (x) (cons pre x)) lst))

; Example:
;> (add_prefix 0 '((0) (1)))
;'((0 0) (0 1))

(define (hamiltonian_cycle_on_cube n)
  (cond
    [(<= n 1) '((0) (1))]
    [else
     (let* ((inner_cube (hamiltonian_cycle_on_cube (- n 1)))
           (zerolist (add_prefix 0 inner_cube))
           (onelist (add_prefix 1 (reverse inner_cube))))
        (append zerolist onelist))]))

; Example:
(trace hamiltonian_cycle_on_cube)
(hamiltonian_cycle_on_cube 3)
;>(hamiltonian_cycle_on_cube 3)
;> (hamiltonian_cycle_on_cube 2)
;> >(hamiltonian_cycle_on_cube 1)
;< <'((0) (1))
;< '((0 0) (0 1) (1 1) (1 0))
;<'((0 0 0) (0 0 1) (0 1 1) (0 1 0) (1 1 0) (1 1 1) (1 0 1) (1 0 0))
;'((0 0 0) (0 0 1) (0 1 1) (0 1 0) (1 1 0) (1 1 1) (1 0 1) (1 0 0))


;; check if it's correct
(define (equal_but_one vert1 vert2 diffs)
  (cond
    [(empty? vert1) (= diffs 1)]
    [(= (car vert1) (car vert2))
     (equal_but_one (cdr vert1) (cdr vert2) diffs)]
    [else (equal_but_one (cdr vert1) (cdr vert2) (+ 1 diffs))]))

(define (ham-check-aux lst)
  (cond
    [(< (length lst) 2) #t]
    [else (let* ((vert1 (car lst))
                 (vert2 (cadr lst)))
            (and (equal_but_one vert1 vert2 0) (ham-check-aux (cdr lst))))]))

(define (ham-check n lst)
  (let ((len (length lst)))
    (and
     ;; correct number of visited vertices
     (= len (expt 2 n))
     ;; no duplicates
     (= len (length (remove-duplicates lst)))
     ;; All adjacent vertices differ by exactly one.
     (ham-check-aux lst))))

;(ham-check 1 (hamiltonian_cycle_on_cube 1))
;(ham-check 2 (hamiltonian_cycle_on_cube 2))
;(ham-check 4 (hamiltonian_cycle_on_cube 4))
;(ham-check 5 (hamiltonian_cycle_on_cube 5))
;(ham-check 10 (hamiltonian_cycle_on_cube 10))
;#t
;#t
;#t
;#t
;#t



;; esc control = is uncomment
; esc control ; is comment

;> (allcombs 2)
;'((0 0) (0 1) (1 0) (1 1))
;> (allcombs 3)
;'((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
(define (allcombs n)
  (cond
    [(<= n 0) '(())]
    [else
     (let ((lst (allcombs (- n 1))))
       (append (map (lambda (x) (cons 0 x)) lst) (map (lambda (x) (cons 1 x)) lst)))]))

;> (cubesort (allcombs 2) 4)
;'((0 0) (0 1) (1 1) (1 0))
;> (cubesort (allcombs 3) 8)
;'((0 0 0) (0 0 1) (0 1 1) (0 1 0) (1 1 0) (1 1 1) (1 0 1) (1 0 0))
(define (cubesort lst len)
  (cond
    [(<= len 2) '((0) (1))]
    [else
     (let* (
           (newlen (floor (/ len 2)))
           (inner_cube (cubesort (map (lambda (x) (cdr x)) (take lst newlen)) newlen))
           (zerolist (add_prefix 0 inner_cube))
           (onelist (add_prefix 1 (reverse inner_cube))))
        (append zerolist onelist))]))
  

;> (hamiltonian_cycle_on_cube 2)
;'((0 0) (0 1) (1 1) (1 0))
;> (hamiltonian_cycle_on_cube 3)
;'((0 0 0) (0 0 1) (0 1 1) (0 1 0) (1 1 0) (1 1 1) (1 0 1) (1 0 0))
;(define (hamiltonian_cycle_on_cube n)
;  (cubesort (allcombs n) (expt 2 n)))









;>(cubesort
;  '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
;  8)
;> (cubesort '((0 0) (0 1) (1 0) (1 1)) 4)
;> >(cubesort '((0) (1)) 2)
;< <'((0) (1))
;> >(cubesort '((1) (0)) 2)
;< <'((1) (0))
;< '((0 0) (0 1) (1 1) (1 0))
;> (cubesort '((1 1) (1 0) (0 1) (0 0)) 4)
;> >(cubesort '((1) (0)) 2)
;< <'((1) (0))
;> >(cubesort '((0) (1)) 2)
;< <'((0) (1))
;< '((1 0) (1 1) (0 1) (0 0))
;<'((0 0 0) (0 0 1) (0 1 1) (0 1 0) (1 1 0) (1 1 1) (1 0 1) (1 0 0))
;'((0 0 0) (0 0 1) (0 1 1) (0 1 0) (1 1 0) (1 1 1) (1 0 1) (1 0 0))




;(hamiltonian_cycle_on_cube 10)
;(let ((cycle (hamiltonian_cycle_on_cube 5))) (- (length cycle) (length (remove-duplicates cycle))))
;(hamiltonian_cycle_on_cube 4)
;(hamiltonian_cycle_on_cube 2)
;(hamiltonian_cycle_on_cube 1)
;(hamiltonian_cycle_on_cube 0)
;(hamiltonian_cycle_on_cube -1)



; scrap
;        (append
;         (map (lambda (x) (cons 0 x)) (cubesort (map (lambda (x) (cdr x)) (take lst newlen)) newlen))
;         (map (lambda (x) (cons 1 x)) (cubesort (map (lambda (x) (cdr x)) (reverse (list-tail lst newlen))) newlen))
;         )
;        (append
;         (map (lambda (x) (cons 1 x)) (cubesort (map (lambda (x) (cdr x)) (reverse (list-tail lst newlen))) newlen))
;         (map (lambda (x) (cons 0 x)) (cubesort (map (lambda (x) (cdr x)) (take lst newlen)) newlen))
;         )))]))
;(cons 1 '())
;(cons '((1)) '((0)))
;(append '((1)) '((0)))
;(list-tail '(1 2 3 4) 2)
;(take '(1 2 3 4) 2)
;(expt 2 3)
;(floor (/ 1 2))
;(map (cons 0) '(1 2 3 4))
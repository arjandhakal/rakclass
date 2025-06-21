#lang racket

;; point takes in x and y 
;; and returns a function
;; with x and y in closure
(define point
  (lambda (x y)
    (lambda (m)
      (case m
        [(getX) (lambda () x)]
        [(getY) (lambda () y)]
        [(translateX) (lambda (offset) (+ x offset))]))))


(define p1 (point 1 2))
;; => p1 (our object) is now a function that accepts a method name as a symbol

;; (p1 'getX) returns the getX lambda
(p1 'getX)

;; to actually invoke the getX, we would 
;; need to call the return value once more
((p1 'getX))
;; => 1

((p1 'translateX) 2)
;; => 3 (offsets 1 by 2)

;; It would expand to what what we did
;; while calling methods on p1
(define-syntax apply
  (syntax-rules ()
    [(apply o mn args ...)
     ((o 'mn) args ...)]))

;; A cleaner syntax
(apply p1 getX)
;; => 1

;; ((p1 'translateX) 2) becomes: (apply p2 translateX 2)
(apply p1 translateX 2)
;; => 3

;; an initial attempt at something like a class that
;; gives us point object
(define (make-point)
  (let ([displayName "Point"]
        [distance (lambda (a b)
                    (let* ([dx (- (apply a getX)
                                  (apply b getX))]
                           [dy (- (apply a getY)
                                  (apply b getY))])
                      (sqrt (+ (* dx dx)
                               (* dy dy)))))])
    ;; The Class Dispatcher
    (lambda (cm) 
      (case cm
          [(displayName) displayName]
          [(distance) distance]
          [(new) 
           (lambda (x y) ;; <= what we used before, our object creator
             (lambda (m)
               (case m
                 [(getX) (lambda () x)]
                 [(getY) (lambda () y)]
                 [(translateX) (lambda (offset) (+ x offset))])))]))))

;; `Point` is our "class"
(define Point (make-point))

;; making the point objects
(define p3 ((Point 'new) 1 2))
(define p4 ((Point 'new) 5 6))

;; calling the static method distance 
((Point 'distance) p3 p4)


;; This macro just allows writing
;; syntax for creating object
(define-syntax object
  (syntax-rules (fields methods)
    [(_ (fields [fn iv] ...)
        (methods [mn imp] ...))
     (let ([fn iv]
           ...)
       (lambda (m)
         (case m
           [(mn) imp]
           ...)))]))

;; Similar to previous example
;; just using macro that lets us
;; create class 
(define-syntax class
  (syntax-rules (fields methods)
    [(_ name
        (statics [stn is] ...)
        (fields [fn iv] ...)
        (methods [mn imp] ...))
     (define name
       (let ([stn is] ...)
         (lambda (cm)
           (case cm
             [(new) 
              (object (fields [fn iv] ...)
                      (methods [mn imp] ...))]
             [else cm]))))]))


(class BetterPoint
  (statics [displayName "Better Pointer"])
  (fields [x 0]
          [y 0])
  (methods [setX (lambda (v) (set! x v))]
           [setY (lambda (v) (set! y v))]
           [getX (lambda () x)]
           [getY (lambda () y)]))

;; Calling static methods
(BetterPoint 'displayName)

;; A simple object 
(define betterPoint1 (BetterPoint 'new))
(apply betterPoint1 getX) ;; should give 1

;; Mutating state
(apply betterPoint1 setX 2) 
(apply betterPoint1 getX) ;; should give 2

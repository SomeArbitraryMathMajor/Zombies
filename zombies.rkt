;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname zombies) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =============================================================================
;; Bryan Zang
;; CS135 Fall 2020
;; A10
;; =============================================================================


;; Definitions

;; A Location is a Nat
;; A Town is a (listof (list Location (listof Location)))
;; Requires: Town represents a valid graph as defined in Module 16

;; A Horde is a (listof (list Location Nat))

;; Constants
(define waterloo '((0 (1 2 3))
                   (1 (2 3))
                   (2 (0 4))
                   (3 (1))
                   (4 (5))
                   (5 (3))))
(define toronto '((0 (1 2))
                  (1 (0))
                  (2 (3 4))
                  (3 (2))
                  (4 (1))))
(define test-horde '((0 1000)
                     (1 1000)
                     (2 1000)
                     (3 1000)
                     (4 1000)
                     (5 1000)))
(define test-horde2 '((0 1000)
                      (1 1000)
                      (2 1000)
                      (3 1000)
                      (4 1000)))


;; i)

;;(infect town zombies) consumes a Location and a Nat to produce
;;   a Horde
;;examples
(check-expect (infect waterloo 1000)
              '((0 1000)
                (1 1000)
                (2 1000)
                (3 1000)
                (4 1000)
                (5 1000)))
;;infect: Location Nat -> Horde
(define (infect town zombies)
  (cond
    [(empty? town) empty]
    [else (cons (list (first (first town)) zombies)
                (infect (rest town) zombies))]))
;;testing
(check-expect (infect waterloo 6)
              '((0 6)
                (1 6)
                (2 6)
                (3 6)
                (4 6)
                (5 6)))
(check-expect (infect toronto 71)
              '((0 71)
                (1 71)
                (2 71)
                (3 71)
                (4 71)))
(check-expect (infect '() 0) empty)


;; ii)

;;(sink horde) consumes a Horde to produce a list of two
;;   elements which are a Nat and a Horde
;;examples
(check-expect (sink (infect waterloo 1000))
              '(300
                ((0 950)
                 (1 950)
                 (2 950)
                 (3 950)
                 (4 950)
                 (5 950))))
(check-expect (sink (infect waterloo 6))
              '(0
                ((0 6)
                 (1 6)
                 (2 6)
                 (3 6)
                 (4 6)
                 (5 6))))
;;sink: Horde -> (listof Nat Horde)
(define (sink horde)
  (local
    [;;(five-percent element) consumes an element from a horde to
     ;;   produce a rounded 5% of the infected population
     ;;(listof Nat Nat) -> Nat
     (define (five-percent element)
       (round (* (second element) 0.05)))

     ;;(total lst) produces the total of the sunken infected
     ;;Horde -> Int
     (define (total lst)
       (cond
         [(empty? lst) 0]
         [else (+ (five-percent (first lst)) (total (rest lst)))]))

     ;;(replace lst) produces a horde with the sunken infected population
     ;;Horde -> Horde
     (define (replace lst)
       (cond
         [(empty? lst) empty]
         [else (cons
                (list (first (first lst))
                      (- (second (first lst)) (five-percent (first lst))))
                (replace (rest lst)))]))
     ]    
    (cond
      [(empty? horde) empty]
      [else (list (total horde) (replace horde))])
    ))
;;testing
(check-expect (sink (infect '((0 (1 2 3))) 1)) '(0 ( (0 1))))
(check-expect (sink '()) empty)
(check-expect (sink (infect toronto 21))
              '(5
                ((0 20)
                 (1 20)
                 (2 20)
                 (3 20)
                 (4 20))))


;; iii)

;;(apportion zombies n) consumes num and a Nat to produce a list of
;;   length n with each element having a difference of at most 1
;;requires: n is a natural number
;;examples
(check-expect (apportion 100 3) '(34 33 33))
(check-expect (apportion 55 4) '(14 14 14 13))
;;apportion: num Nat -> listofNat
(define (apportion zombies n)
  (local
    [;;(repeated num times) produces a list of num
     ;;   a repeated amount of times
     ;;repeated: num Nat -> listofnum
     (define (repeated num times)
       (cond
         [(zero? times) empty]
         [else (cons num (repeated num (sub1 times)))]))

     ;;(final n1 n2) divides n1 by n2 such that the result is the
     ;;   remainder with a difference of 1 relative to the quotient
     ;;final: num Nat -> Nat
     (define (final n1 n2)
       (- n1 (* (round (/ n1 n2)) (sub1 n2))))
     ]   
    (cond
      [(>=
        (round (/ zombies n)) (final zombies n))
       (append
        (repeated (round (/ zombies n)) (sub1 n))
        (list (final zombies n)))]
      [else
       (append
        (list (final zombies n))
        (repeated (round (/ zombies n)) (sub1 n)))]
      )))
;;testing
(check-expect (apportion 0 1) '(0))
(check-expect (apportion 1 3) '(1 0 0))
(check-expect (apportion 209 6) '(35 35 35 35 35 34))
(check-expect (apportion 211 6) '(36 35 35 35 35 35))


;; iv)

;; Constant
(define braaaaains (second (sink (infect waterloo 1000))))

;;(shamble town horde) consumes a town and horde to produce a new horde
;;examples
(check-expect (shamble waterloo braaaaains)
              '((0 475)
                (1 1267)
                (2 792)
                (3 1741)
                (4 475)
                (5 950)))
(check-expect (shamble waterloo test-horde)
              '((0 500)
                (1 1334)
                (2 833)
                (3 1833)
                (4 500)
                (5 1000)))
;;shamble: Town Horde -> Horde
(define (shamble town horde)
  (local
    [;;(component t-lst h-lst) consumes two lists to produce a list where
     ;;   the first element from each list is listed together
     ;;component: listofX listofY -> (listof (list X Y))
     (define (component t-lst h-lst)
       (cond
         [(empty? t-lst) empty]
         [else (cons
                (list (first t-lst) (first h-lst))
                (component (rest t-lst) (rest h-lst)))]))
     
     ;;(ap-list t0 t h) consumes a Town twice and a Horde to list together the
     ;;   location number and the apportion of the zombie population there
     ;;ap-list: Town Town Horde -> (listof (list num num))
     (define (ap-list t0 t h)
       (cond
         [(empty? h) empty]
         [(= (first (first t0)) (first (first h)))
          (append (component
                   (second (first t0))
                   (apportion (second (first h))
                              (length (second (first t0)))))
                  (ap-list t t (rest h)))]
         [else (ap-list (rest t0) t h)]))

     ;;(add-up locat lst) sums up all the second elements in lst that have
     ;;   a common first element aka locat
     ;;add-up: num (listof (list num num)) -> num
     (define (add-up locat lst)
       (cond
         [(empty? lst) 0]
         [else
          (+
           (cond
             [(= (first (first lst)) locat) (second (first lst))]
             [else 0]
             ) (add-up locat (rest lst)))]))

     ;;(added-lst t t2 h basecase) consumes a Town twice and a Horde to
     ;;   produce a new horde from applying the previous functions
     ;;added-lst: Town Town Horde Int -> Horde
     (define (added-lst t t2 h basecase)
       (cond
         [(= basecase (length h)) empty]
         [else (cons
                (list (first (first t))
                      (add-up (first (first t)) (ap-list t2 t2 h)))
                (added-lst (rest t) t2 h (add1 basecase)))]))
     ]
    (added-lst town town horde 0)
    ))
;;testing
(check-expect (shamble toronto test-horde2)
              '((0 1000)
                (1 1500)
                (2 1500)
                (3 500)
                (4 500)))
(check-expect (shamble waterloo braaaaains)
              '((0 475)
                (1 1267)
                (2 792)
                (3 1741)
                (4 475)
                (5 950)))


;; v)

;;(rise zombies horde) produces a horde with an apportioned
;;   amount of zombies added to the inputted horde
;;requires: zombies is a natural number
;;examples
(check-expect (rise 4 braaaaains)
              '((0 951)
                (1 951)
                (2 951)
                (3 951)
                (4 951)
                (5 949)))
(check-expect (rise 6 (infect toronto 21))
              '((0 23)
                (1 22)
                (2 22)
                (3 22)
                (4 22)))
;;rise: Nat Horde -> Horde
(define (rise zombies horde)
  (local
    [;;evenly-divided takes the apportion of zombie
     ;;evenly-divided: listofNat
     (define evenly-divided
       (cond
         [(empty? horde) empty]
         [else (apportion zombies (length horde))]))

     ;;(add-elements h e) adds each corresponding
     ;;   element in e with that of h
     ;;add-elements: Horde listofNat -> Horde
     (define (add-elements h e)
       (cond
         [(empty? e) empty]
         [else
          (cons
           (list
            (first (first h)) (+ (second (first h)) (first e)))
           (add-elements (rest h) (rest e)))]))
     ]
    (add-elements horde evenly-divided)
    ))
;;testing
;;(check-expect (rise 12 (list )) empty)
(check-expect (rise 1 '()) empty)
(check-expect (rise 6 (infect waterloo 1000))
              '((0 1001)
                (1 1001)
                (2 1001)
                (3 1001)
                (4 1001)
                (5 1001)))
(check-expect (rise 724 (infect toronto 38))
              '((0 183)
                (1 183)
                (2 183)
                (3 183)
                (4 182)))

;; vi)

;;(night town horde)
;;examples
(check-expect (night toronto test-horde2)
              '((0 1000)
                (1 1475)
                (2 1475)
                (3 525)
                (4 525)))
;;night: Town Horde -> Horde
(define (night town horde)
  (rise (first (sink horde)) (shamble town (second (sink horde)))))
;;testing
(check-expect (night waterloo braaaaains)
              '((0 499)
                (1 1251)
                (2 800)
                (3 1701)
                (4 499)
                (5 950)))
(check-expect (night waterloo test-horde)
              '((0 525)
                (1 1317)
                (2 842)
                (3 1791)
                (4 525)
                (5 1000)))


;; vii)

;;(apocalypse town infection nights) consumes a town an initial amount
;;   of infected and the amount of days to pass and produces a horde
;;      after said amount of days
;;examples
(check-expect (apocalypse waterloo 950 3)
              '((0 429)
                (1 1800)
                (2 1049)
                (3 1541)
                (4 427)
                (5 454)))
(check-expect (apocalypse toronto 1000 9)
              '((0 1077)
                (1 1397)
                (2 1397)
                (3 565)
                (4 564)))
(check-expect (apocalypse waterloo 950 6)
              '((0 525)
                (1 1580)
                (2 988)
                (3 1524)
                (4 525)
                (5 558)))
;;apocalypse: Town Nat Nat -> Horde
(define (apocalypse town infection nights)
  (local
    [;;(apocalst t h n) produces a list of the horde after each night
     ;;apocalst: Town Horde Nat -> listofHorde
     (define (apocalst t h n)
       (cond
         [(zero? n) empty]
         [else (cons (night t h) (apocalst t (night t h) (sub1 n)))]))

     ]
    (cond
      [(zero? nights) (infect town infection)]
      [else (first (reverse (apocalst town (infect town infection) nights)))])))
;;testing
(check-expect (apocalypse waterloo 1 0)
              '((0 1)
                (1 1)
                (2 1)
                (3 1)
                (4 1)
                (5 1)))
(check-expect (apocalypse toronto 31 4)
              '((0 44)
                (1 31)
                (2 33)
                (3 25)
                (4 22)))
(check-expect (apocalypse waterloo 1000 31)
              '((0 545)
                (1 1723)
                (2 1042)
                (3 1579)
                (4 544)
                (5 567)))
(check-expect (apocalypse waterloo 1000 14)
              '((0 545)
                (1 1728)
                (2 1040)
                (3 1576)
                (4 545)
                (5 566)))
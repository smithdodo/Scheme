;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hengwen.z.uebung6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;;#reader(lib "htdp-advanced-reader.ss" "lang")((modname backpack_AppStore-template) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; An app is defined as (make-app name price rel fun), where
;; name: String - the name of the app
;; price: number - the price for this app in euro (>0)
;; size: number - the size in MB for this application
;; rel: number - the perceived relevance of/interest in this app (the higher, the better)
;; fun: number - how much fun having this app will bring (the higher, the better)
(define-struct app (name price size rel fun))
(define blek   (make-app "Blek" 0.89 28.0 70 40))
(define ctr2   (make-app "Cut the Rope 2" 0.89 84.4 75 80))
(define plague (make-app "Plague Inc." 0.99 53.5 80 78))
(define examples (list blek ctr2 plague))

;;Aufgabe 7.1:
;;rel-sum: (listof app) -> number
;;consumes a list of apps and produces the sum of the apps' relevance values
;;Example: (rel-sum (list blek ctr2) -> 145
(define (rel-sum aloa)
  (foldr (lambda (app sum) (+ (app-rel app) sum)) 0 aloa))
  
;; Tests
(check-expect (rel-sum examples) 225)
(check-expect (rel-sum (list ctr2 plague)) 155)

;; fun-sum: (listof app) -> number 
;;consumes a list of apps and produces the sum of the apps' fun values
;;Example: (fun-sum examples) -> 198
(define (fun-sum aloa)
  (foldr (lambda (app sum) (+ (app-fun app) sum)) 0 aloa))
  
;; Tests
(check-expect (fun-sum examples) 198)
(check-expect (fun-sum (list ctr2 plague)) 158)

;;best-app: (listof app) fun-sum/rel-sum number number -> (listof app)
;;find out which app combination worth to buy with given balance and store space
;;Example:(best-apps examples rel-sum 1 120) -> (list plague)
(define (best-apps apps-to-choose selector balance space)
  (local (;;balance-sum: (listof app) -> number
          ;;calculate how much does a app combination cost
          (define (balance-sum aloa) (foldr (lambda (app sum) (+ (app-price app) sum)) 0 aloa))
          ;;space-sum: (listof app) -> number
          ;;calculate how much space does a app combination need
          (define (size-sum aloa) (foldr (lambda (app sum) (+ (app-size app) sum)) 0 aloa))
          ;;enumerator: (listof app) -> (listof (listof app))
          ;;return a list that contain every combinations, no matter possible or not
          ;;implementation is like a tree: one site with element a, the other site without element a... and do it recursively
          (define (enumerator aloa)
            (cond [(empty? aloa) empty]
                  [else (append (enumerator (rest aloa)) 
                                (local ((define (app-adder app combs)
                                          (cond [(empty? combs) (list (list app))]
                                                [else (cons (cons app (first combs))
                                                            (app-adder app (rest combs)))])))
                                  (app-adder (first aloa) (enumerator (rest aloa)))))])))
    (foldr (;;return the best combination
            lambda (comb best-comb)
             (if (> (selector comb) (selector best-comb)) comb best-comb))
           empty
           (filter (;cut out those impossible combinations
                    lambda (comb)
                     (and (>= balance (balance-sum comb)) (>= space (size-sum comb))))
                   (enumerator apps-to-choose)))
    );end local
  )

;; Tests
(check-expect (best-apps empty rel-sum 30 100) empty)
(check-expect (best-apps examples rel-sum 3 25) empty)
(check-expect (best-apps examples rel-sum 1 50) (list blek))
(check-expect (best-apps examples rel-sum 3 60) (list plague))
(check-expect (best-apps examples rel-sum 1 120) (list plague))
(check-expect (best-apps examples rel-sum 3 120) (list blek plague))
(check-expect (best-apps examples rel-sum 1 170) (list plague))
(check-expect (best-apps examples rel-sum 2 170) (list ctr2 plague))
(check-expect (best-apps examples rel-sum 3 170) (list blek ctr2 plague))

(check-expect (best-apps empty fun-sum 30 100) empty)
(check-expect (best-apps examples fun-sum 3 25) empty)
(check-expect (best-apps examples fun-sum 1 50) (list blek))
(check-expect (best-apps examples fun-sum 1 120) (list ctr2))
(check-expect (best-apps examples fun-sum 3 120) (list blek ctr2))
(check-expect (best-apps examples fun-sum 1 170) (list ctr2))
(check-expect (best-apps examples fun-sum 2 170) (list ctr2 plague))
(check-expect (best-apps examples fun-sum 3 170) (list blek ctr2 plague))
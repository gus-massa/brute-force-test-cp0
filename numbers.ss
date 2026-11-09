;;; Copyright 2026 Gustavo Massaccesi
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.


(define pxnums (list #e1E-300 #e1E-500
                     1/2 1 2 3 8 10
                     (- (greatest-fixnum) 1) (greatest-fixnum) (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 2) 
                     #e1E300 #e1E500 (+ #e1E500 1) (- #e1E500 1)
                     1+i 1-i))

(define pnums (append (map inexact pxnums)
                      pxnums
                      (list 0 0+i 0-i 0.0+0.0i 0.0-0.0i)))

(define nums (append (list +nan.0 '(fl- (black-box +nan.0)) "0" #f)
                     (list
                       '(let ([x (black-box 0.0)]) (if (flonum? x) x (error)))
                       '(let ([x (black-box -0.0)]) (if (flonum? x) x (error)))
                       '(let ([x (black-box 0)]) (if (fixnum? x) x (error)))
                       )
                     (map - pnums)
                     pnums))

(define bnums (append (map (lambda (x) `(black-box ,x)) nums)
                      nums))

(define (test/nn f)
  (display f)
  (newline)
  (let loopx ([lx bnums])
    (unless (null? lx)
      (let ([x (car lx)])
        (let loopy ([ly bnums])
          (if (null? ly)
              (loopx (cdr lx))
              (let* ([y (car ly)]
                     [a (guard (x [else (list 'error 'x)])
                          ((eval `(lambda () (,f (black-box ,x) (black-box ,y))) (interaction-environment))))]
                     [b (guard (x [else (list 'error 'x)])
                          ((eval (parameterize ([run-cp0 (lambda (cp0 x) (cp0 (#3%$cptypes x)))])
                                   (expand/optimize `(lambda () (,f ,x ,y)))) (interaction-environment))))])
                (unless (equal? a b)
                  (display (list f x y '-> a b (equal? a b)))
                  (newline))
                (loopy (cdr ly)))))))))

(test/nn 'min)
(test/nn 'max)

(test/nn '+)
(test/nn '-)
(test/nn '/)
(test/nn '*)

(test/nn 'quotient)
(test/nn 'remainder)
(test/nn 'modulo)
(test/nn 'div)
(test/nn 'mod)
(test/nn 'div0)
(test/nn 'mod0)

(test/nn '=)
(test/nn '>)
(test/nn '<)
(test/nn '>=)
(test/nn '<=)


(exit)

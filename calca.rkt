#lang racket
(define (Cand x)(match x #|using match case for most efficent operator evaluation|#       
  [(list a '& c d ...)
   (flatten (list* (and a c) d (and a c)))]#|the changed value is put at the end to make it directly accessable|#
  [else  x]))
(define (Cor x)(match x             
  [(list a 'V c d ...)
   (flatten(list* (or a c) d (or a c)))]
  [else x]))

(define iter #|this function is iterating the matching over a list |#
  (lambda (list fun)
    (cond ((null? (cdr list)) list)
          ((boolean? (cadr(reverse list))) list)
          (else (let ([x (fun list)])(flatten (cons (car x) (iter (cdr x) fun)))))))) #| flatten to fix improper lists|#

(define operate #|evaluating each operator|#
  (lambda (list)
  (cond ((null?  list)  '())
        ((eq? (car list) '~) (list* (not (cadr list)) '())) #|end with null to make single element sublist|#
        ((list? (member '& list)) (cons  (last (iter list Cand)) (operate(reverse(cdr(reverse(iter list Cand)))))))#|the reverse-cdr-reverse is removing the last element|#
        ((list? (member 'V list)) (cons  (last (iter list Cor)) (operate(reverse(cdr(reverse(iter list Cor)))))))
        (else '()))))

(define extra #|convert single element lists into expressions|#
 (lambda (list)
   (cond ((null? list) '())
         ((list? (car list)) (list* (car list) '& (car list) (extra(cdr list))))
         (else (cons (car list) (extra (cdr list)))))))

(define de-list #|deal with sublists|#
  (lambda (list)
    (cond ((null? list) '())
          ((and (list? (car list))(null? (cdar list)))  (cons (operate (list* (car list) '& (car list))) (de-list (cdr list))))
          ((list? (car list))  (cons (operate (car list)) (de-list (cdr list))))
          (else (cons (car list) (de-list(cdr list)))))))

(define evaluate-expression #|Starts each operation chain|#
(lambda (lst) (operate #|apply operators|#
               (flatten #|fix improper lists && remove sublists|#
                (extra #|single element sublists -> expressions|#
                 (de-list lst #|simplify sublists|#
                          ))))))


#|These test expressions are pulled directly from the book|#
#|The two commented expressions are the only two that are not correctly expressed.
The reason for this is because there are arbitrary layers of parentheses.
This results in some of the computed middle terms to be missing but with the final term being correct.|#

(evaluate-expression '(#f V  #t & #t & #f))
(evaluate-expression '((~ #t) V (~ #f) & #t))
(evaluate-expression '((#f) & (#t) V (#f) & (~ #t)))

#|(evaluate-expression '(((((~((((#t V #f))))) & ((~ #t)))))))|#
#|(evaluate-expression '(((~ #t) V #t V (#f & (~ #f))) & #t & (~ (#t V #f))))|#
(evaluate-expression '(#t & #t))
(evaluate-expression '(#t & #t & (#t & #f)))
(evaluate-expression '(#t & (#t & #f)))
(evaluate-expression '(#t & (#t & #f) & (#f & #t)))
(evaluate-expression '((#t V #f) & #t))
(evaluate-expression '((#t V #f) & #t & #f))
(evaluate-expression '((#t V #f) & (#t V #t)))
(evaluate-expression '((#t V #f) & (#t V #t) V #t))
(evaluate-expression '(#t V #t))
(evaluate-expression '(#t V #t & (#t & #f)))
(evaluate-expression '(#t V (#t & #f)))
(evaluate-expression '(#t V (#t & #f) & (#f & #t)))
(evaluate-expression '((#t V #f) V #t))
(evaluate-expression '((#t V #f) V #t & #f))
(evaluate-expression '((#t V #f) V (#t V #t)))
(evaluate-expression '((#t V #f) V (#t V #t) V #t))




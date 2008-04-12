(module pragmatic mzscheme
  (require (lib "list.ss")
           (lib "process.ss")
           (lib "match.ss")
           (lib "string.ss"))
  (provide (all-from (lib "list.ss"))
           (all-from (lib "process.ss"))
           (all-from (lib "match.ss"))
           (all-from (lib "string.ss")))

(define-syntax rename-syntax
  (syntax-rules () 
    [(_ from to) 
     (define-syntax to
       (syntax-rules () [(to . rest) (from . rest)]))]))

(rename-syntax match-let let/m)
(rename-syntax match-lambda lambda/m)

(define (string-join joiner strings)
  (foldl (lambda (x y) (string-append y joiner x)) (car strings)
         (cdr strings)))

(define (each f x)
  (cond ((null? x) (values))
        (else (f (car x)) (each f (cdr x)))))
    
(provide string-join)
(provide rename-syntax let/m lambda/m)
)
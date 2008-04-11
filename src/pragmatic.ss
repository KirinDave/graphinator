(module pragmatic mzscheme
  (require (lib "list.ss")
           (lib "process.ss")
           (lib "match.ss"))
  (provide (all-from (lib "list.ss"))
           (all-from (lib "process.ss"))
           (all-from (lib "match.ss")))

(define-syntax rename-syntax
  (syntax-rules () 
    [(_ from to) 
     (define-syntax to
       (syntax-rules () [(to . rest) (from . rest)]))]))

(rename-syntax match-let let/m)
(rename-syntax match-lambda lambda/m)


(provide rename-syntax let/m lambda/m)
)
(module git-graphinator mzscheme
  (require "pragmatic.ss")

; Data primitives 
(define-struct commit (name parents date author title marked?))
(define (commit-root? x) (null? (commit-parents x)))
(define (commit-mark! x) (set-commit-marked?! x #t))

(define orphaned-commit (make-commit 'bad '() 0 "noone!" "failboat" #f))

; Lame-ass parser
(define commit-from-form
  (match-lambda 
   [(n p d a t)    (make-commit n p d a t #f)]
   [(n p d a . ig) (make-commit n p d a "(malformatted title)" #f)]))

; Fetching from git
(define git-command-strings
  '("git --no-pager " "' log --date=rfc --reverse --pretty='format:(%H (%P) %at \"%ae\" \"%t\")'"))

(define (git-cmd dir)
  (let [(base (first git-command-strings))
        (tail (second git-command-strings))]
    (string-append base "--git-dir=\'" dir tail)))

(define (process-git-commits dir handler)
  (let/m ([(in out id err control) (process (git-cmd dir))])
    (close-output-port out) (close-input-port err)
    (let loop ((next (read in)))
      (cond
       ((eof-object? next) 'done)
       (else
        (handler (commit-from-form next))
        (loop (read in)))))
    (close-input-port in)))

; Reading step
; 1. Read in and accumulate the commits
; While doing so, keep a buffer head commits
; return a list of head commits and a hash of all commits

(define (accumulate-commits . directories)
  (let ([chs (make-hash-table)]
        [cls (list)]
        [sort 
         (lambda (lst) 
           (sort lst 
                 (lambda (x y) (< (commit-date x) (commit-date y)))))])
    (let loop-dirs ((dirs directories))
      (cond
       ((null? dirs) (list (sort cls) chs))
       (else (process-git-commits (car dirs)
                (lambda (item)
                  (hash-table-put! chs
                                   (commit-name item)
                                   item)
                  (set! cls (cons item cls))))
             (loop-dirs (cdr dirs)))))))

(define (make-graph-major-ordered commits entity-table)
  (letrec ([fand (lambda (x y) (and x y))] ; Annoying
           [parents (lambda (commit)
                      (map (lambda (x) 
                             (hash-table-get entity-table
                                             x
                                             orphaned-commit))
                           (commit-parents commit)))]
                    [parents-marked? 
            (lambda (commit)
              (foldl fand #t 
                     (map commit-marked? (parents commit))))]
           [eligable? (lambda (x) 
                        (and (not (commit-marked? x))
                             (parents-marked? x)))]
           [find-eligable 
            (lambda (cls)
              (cond ((null? cls) #f)
                    ((eligable? (car cls)) (car cls))
                    (else (find-eligable (cdr cls)))))])
    (display "Looping starts now.")
    (let loop ((cls commits) (output (list)))
      (cond ((null? cls) (display "Top null bailout\n") (reverse output))
            ((commit-marked? (car cls)) (loop (cdr cls) output))
            (else 
             (let ((n (find-eligable cls)))
              (cond 
               (n (commit-mark! n)
                  (loop cls (cons n output)))
               (else
                (display "base-level bailout\n")
                (reverse output))))))))) ; No more valid commits.
    

(provide process-git-commits)
(provide accumulate-commits)
(provide make-graph-major-ordered)
(provide commit-name commit-parents commit-marked?)
) ; end module
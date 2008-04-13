(module git-graphinator mzscheme
        (require (lib "list.ss")
                 (lib "process.ss")
                 (lib "match.ss")
                 (lib "string.ss")
                 (lib "pregexp.ss"))

; Utility
(define (string-join joiner strings)
  (foldl (lambda (x y) (string-append y joiner x)) (car strings)
         (cdr strings)))

; Data primitives 
(define-struct commit (name parents date author title marked?))
(define (commit-root? x) (null? (commit-parents x)))
(define (commit-mark! x) (set-commit-marked?! x #t))
(define orphaned-commit (make-commit 'bad '() 0 "noone!" "failboat" #f))

(define (write-json-commit commit out) (display (commit->json commit) out))
(define (commit->json commit)
  (let* ([sy->st (lambda (x) (format "~s" (symbol->string x)))]
         [format-parents 
          (lambda (parents)
            (cond ((null? parents) "")
                  (else (string-join "," (map sy->st parents)))))])
    (format 
     (string-append "{"
                    "\"id\":\"~a\","
                    "\"author\":\"~a\","
                    "\"date\":\"~a\","
                    "\"parents\":[~a],"
                    "\"message\":~s"
                    "}")
     (commit-name commit)
     (commit-author commit)
     (commit-date commit)
     (format-parents (commit-parents commit))
     (commit-title commit))))


(define rsplit (pregexp "!split!"))
(define (commit-from-line line)
  (let ((parts (pregexp-split rsplit line)))
    (match-let ([(n p d a) (lex-left-side (first parts))])
               (make-commit n p d a (second parts) #f))))

(define lex-left-side
  (let ((rx (pregexp "!-!"))
        (sx (pregexp " ")))
    (lambda (ls)
      (let ([items (pregexp-split rx ls)]
            [mpar (lambda (ps)
                    (if (> (string-length ps) 0)
                        (map string->symbol (pregexp-split sx ps))
                        (list)))])
        (list 
         (string->symbol (first items))
         (mpar (second items))
         (string->number (third items))
         (fourth items))))))

; Fetching from git
(define git-command-strings
  '("git --no-pager " "' log --reverse --date=rfc --pretty='format:%H!-!%P!-!%at!-!%ae!split!%s'"))

(define (git-cmd dir)
  (let [(base (first git-command-strings))
        (tail (second git-command-strings))]
    (string-append base "--git-dir=\'" dir tail)))

(define (process-git-commits dir handler)
  (match-let ([(in out id err control) (process (git-cmd dir))])
    (close-output-port out) (close-input-port err)
    (let loop ((next (read-line in)))
      (cond
       ((eof-object? next) 'done)
       (else
        (handler (commit-from-line next))
        (loop (read-line in)))))
    (close-input-port in)))

; Reading step
; 1. Read in and accumulate the commits
; While doing so, keep a buffer head commits
; return a list of head commits and a hash of all commits

(define (accumulate-commits directories)
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
    (let loop ((cls commits) (output (list)))
      (cond ((null? cls) (reverse output))
            ((commit-marked? (car cls)) (loop (cdr cls) output))
            (else 
             (let ((n (find-eligable cls)))
              (cond 
               (n (commit-mark! n)
                  (loop cls (cons n output)))
               (else
                (reverse output))))))))) ; No more valid commits.

(define (write-commits commits)
  (let l ((cs commits) (comma #f))
    (cond ((null? cs) (values))
          (else (if comma (display ","))
                (write-json-commit (car cs) 
                                   (current-output-port))
                (l (cdr cs) #t)))))

(define (output-for-dirs dirs)
  (match-let ([(commits dict) (accumulate-commits dirs)])
             (write-commits (make-graph-major-ordered commits dict))))
             ;(write-commits commits)))

(provide accumulate-commits write-commits make-graph-major-ordered commit->json)
(provide output-for-dirs)
) ; end module
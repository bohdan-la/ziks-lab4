(import utf8 utf8-case-map unicode-char-sets alist-lib)

(define l '())
(alist-set! l 'aa 1)
(when (assoc 'aa l)
    (alist-update! l 'aa add1 (lambda () 0)))

(print l)

(define (main args)
    (print args))
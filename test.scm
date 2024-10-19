(import scheme (chicken base))
(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))


(define (main args)

    (define lst '(1 2 3 4 5))
    ; (print (ch-to-sym #\vvv))
(append! lst 2)
    (print lst))
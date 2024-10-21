(import scheme (chicken base))
(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))

(define plaintext '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й
#\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч
#\ш #\щ #\ь #\ю #\я))

(define (list-index pred lst)
(let loop ((lst lst) (i 0))
(cond
    ((null? lst) #f) ;; Якщо елемент не знайдено
    ((pred (car lst)) i) ;; Якщо знайдено елемент
    (else (loop (cdr lst) (+ i 1)))))) ;; Продовжуємо пошук

(define (main args)

    (define lst '(1 2 3 4 5))
    (print (list-index (lambda (x) (eq? x -6)) lst))
)
(module mylib (ch-to-sym)
(import scheme (chicken base))
(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))


(define ukr-alphabet '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і
	#\ї #\й #\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц
	#\ч #\ш #\щ #\ь #\ю #\я))

(define ch-to-sym
	(lambda (ch)
		(string->symbol (string ch))))

(define (sym-to-ch sym)
	(string-ref (symbol->string sym) 0))

(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

(define (add-file-sym-occur file occur)
	(with-input-from-file file
		(lambda ()
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
					(set! ch (ch-to-lower ch))
					(if (assq (ch-to-sym ch) occur)
						(alist-update! occur (ch-to-sym ch) add1 (lambda () 0))
						(alist-set! occur (ch-to-sym ch) 1))
			(loop (read-char))))))
	occur)

; ; функція для знаходження індексу елемента у списку
; (define (list-index pred lst)
; 	(let loop ((lst lst) (i 0))
; 	(cond
; 		((null? lst) #f) ; якщо елемент не знайдено
; 		((pred (car lst)) i) ; якщо знайдено елемент
; 		(else (loop (cdr lst) (+ i 1)))))) ; продовжуємо пошук

(define (true-ukr-alphabet-order? ch1 ch2)
    (let ((index1 (list-index (lambda (x) (char=? x ch1)) ukr-alphabet))
		(index2 (list-index (lambda (x) (char=? x ch2)) ukr-alphabet)))
		(and index1 index2 (< index1 index2))))


		; (for-each (lambda (pair)
	; 	(unless (memq (car pair) ukr-alphabet-sym)
	; 		(set! occur (alist-delete (car pair) occur))))
	; 	occur)
)
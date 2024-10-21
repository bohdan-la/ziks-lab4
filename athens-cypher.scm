(import utf8 utf8-case-map)
; 33 total
(define letters '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й
	#\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч
	#\ш #\щ #\ь #\ю #\я))

;; знижує регістр символа
(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

(define (list-index pred lst)
	(let loop ((lst lst) (i 0))
	(cond
		((null? lst) #f) ;; Якщо елемент не знайдено
		((pred (car lst)) i) ;; Якщо знайдено елемент
		(else (loop (cdr lst) (+ i 1)))))) ;; Продовжуємо пошук

(define (main args)
	(define a 7)
	(define b 13)
	(define file-in (car args))
	; (define file-out (open-output-file "result.txt"))
	; (define file-out (open-output-file (cadr args)))

	(define letters-cypher (map (lambda (lett) 
		(list-ref letters 
			(modulo (+ (* a (list-index (lambda (x) 
										(eq? x lett)) letters)) 
						b) 33)))
			letters))
	; (for-each (lambda (lett) (display lett)) letters)
	; (newline)

	; (for-each (lambda (lett) (display lett)) letters-cypher)
	; (newline)

	(with-input-from-file file-in
		(lambda ()
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
					(set! ch (ch-to-lower ch))
					; (display ch)
					; (when (eq? ch #\space) (display #\space))
					(if (memq ch letters)
						; (write-char (list-ref letters-cypher (list-index ch)) file-out))
						(display (list-ref letters-cypher (list-index (lambda (x) (eq? x ch)) letters)))
						(display #\space)
					)
			(loop (read-char))))))
	; (close-output-port file-out)
)
(import utf8 utf8-case-map)
; 33 total
(define letters '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й
	#\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч
	#\ш #\щ #\ь #\ю #\я))

; знижує регістр символа
(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

(define (list-index pred lst)
	(let loop ((lst lst) (i 0))
	(cond
		((null? lst) #f) ; якщо елемент не знайдено
		((pred (car lst)) i) ; якщо знайдено елемент
		(else (loop (cdr lst) (+ i 1)))))) ; продовжуємо пошук

(define (main args)
	(define a 10)
	(define b 25)
	(define file-in (car args))

	(define letters-cypher (map (lambda (lett) 
		(list-ref letters 
			(modulo (+ (* a (list-index (lambda (x) 
							(eq? x lett)) letters)) b) 33)))
				letters))

	(with-input-from-file file-in
		(lambda ()
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
					(set! ch (ch-to-lower ch))
					(if (memq ch letters)
						(display (list-ref letters-cypher (list-index (lambda (x) (eq? x ch)) letters)))
						(display ch))
			(loop (read-char))))))
	(newline)
)
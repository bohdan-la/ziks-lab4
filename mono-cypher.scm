(import utf8 utf8-case-map)
; 33 total
(define letters '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й #\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч #\ш #\щ #\ь #\ю #\я))
(define cryptoo '(#\б #\ц #\м #\є #\р #\щ #\п #\х #\і #\о #\н #\ї #\ш #\ч #\ф #\г #\ю #\д #\ж #\я #\а #\и #\ґ #\л #\к #\й #\в #\с #\у #\т #\з #\ь #\е))

; знижує регістр символа
(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

(define (list-index pred lst)
	(let loop ((lst lst) (i 0))
	(cond
		((null? lst) #f)
		((pred (car lst)) i)
		(else (loop (cdr lst) (+ i 1))))))

(define (main args)
	(define file-in (car args))

	(with-input-from-file file-in
		(lambda ()
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
					(set! ch (ch-to-lower ch))
					(if (memq ch letters)
						(display (list-ref cryptoo (list-index (lambda (x) (eq? x ch)) letters)))
						(display ch))
			(loop (read-char))))))
	(newline)
)
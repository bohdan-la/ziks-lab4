(import utf8 utf8-case-map)

(define letters '(#\space #\б #\е #\й #\ф #\д #\ї #\і #\р #\в #\х #\ц #\с))
(define decrypt '(#\space #\о #\а #\л #\н #\и #\в #\г #\б #\ч #\е #\й #\і))

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
						(display (list-ref decrypt (list-index (lambda (x) (eq? x ch)) letters)))
						(display #\-))
			(loop (read-char))))))
	(newline)
)
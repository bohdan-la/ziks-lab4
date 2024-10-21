(import utf8 utf8-case-map)

(define allowed-letters '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й
	#\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч
	#\ш #\щ #\ь #\ю #\я #\space
	#\А #\Б #\В #\Г #\Ґ #\Д #\Е #\Є #\Ж #\З #\И #\І #\Ї #\Й
	#\К #\Л #\М #\Н #\О #\П #\Р #\С #\Т #\У #\Ф #\Х #\Ц #\Ч
	#\Ш #\Щ #\Ь #\Ю #\Я))

(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))


(define (main args)
	(define file-in (car args))

	(with-input-from-file file-in
		(lambda ()
			(let ((prev-ch 'nil))
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
					(when (eq? ch #\newline) (set! ch #\space))
					(when (memq ch allowed-letters)
						(when (not (and (eq? prev-ch #\space) (eq? ch #\space)))
							(display (ch-to-lower ch)))
							(set! prev-ch ch))
			(loop (read-char)))))))
)
(import utf8)

(define allowed-letters '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й
	#\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч
	#\ш #\щ #\ь #\ю #\я #\space
	#\А #\Б #\В #\Г #\Ґ #\Д #\Е #\Є #\Ж #\З #\И #\І #\Ї #\Й
	#\К #\Л #\М #\Н #\О #\П #\Р #\С #\Т #\У #\Ф #\Х #\Ц #\Ч
	#\Ш #\Щ #\Ь #\Ю #\Я))

(define (main args)
	(define file-in (car args))
	; (define file-out (open-output-file "result.txt"))
	(define file-out (open-output-file (cadr args)))

	(with-input-from-file file-in
		(lambda ()
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
				(when (memq ch allowed-letters)
					(write-char ch file-out))
					; (print ch))
			(loop (read-char))))))
	(close-output-port file-out)
)
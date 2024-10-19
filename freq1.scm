(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))

(define ukrainian-alphabet '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й
				#\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч
				#\ш #\щ #\ь #\ю #\я #\' #\space))

(define ch-to-sym
	(lambda (ch)
		(string->symbol (string ch))))

(define (sym-to-ch sym)
	(string-ref (symbol->string sym) 0))

(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

(set! ukrainian-alphabet (map ch-to-sym ukrainian-alphabet))

(define (main args)
  (define file (car args))
  (define occur '())

  (with-input-from-file file
		(lambda ()
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
					(set! ch (ch-to-lower ch))
					(if (assq (ch-to-sym ch) occur)
						(alist-update! occur (ch-to-sym ch) add1 (lambda () 0))
						(alist-set! occur (ch-to-sym ch) 1))
			(loop (read-char))))))
  ; (print occur)

	; (for-each (lambda (pair)
	; 	(unless (memq (car pair) ukrainian-alphabet)
	; 		(set! occur (alist-delete (car pair) occur))))
	; 	occur)
	; (newline)
	; (print occur)

	; підрахунок загальної кількості символів файлу
	(define all_symbols 0)
	(for-each (lambda (pair) (set! all_symbols (+ all_symbols (cdr pair)))) occur)

	; підрахунок ймовірностей кожного символу
	(define freqs (alist-copy occur))
	(map (lambda (pair) (set-cdr! pair (/ (cdr pair) all_symbols))) freqs)

	; (print "Кількість символів: " all_symbols)
	; (print "Ймовірності симовлів:\n" freqs)

	; (for-each (lambda (pair)
	; 	(printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair))))
	; freqs)

	; (print (sort freqs (lambda (a b)
	; 	(> (cdr a) (cdr b)))))

	(print (sort freqs (lambda (a b)
		(char>? (sym-to-ch (car a)) (sym-to-ch (car b))))))
)
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


(define (main args)
	(define ukr-alphabet-sym (map ch-to-sym ukr-alphabet))
	(define occur '())

	; підрахунок кількості появи символів усів заданих файлів
	(for-each (lambda (file) 
		(set! occur (add-file-sym-occur file occur))) args)

	; підрахунок загальної кількості символів файлу
	(define all_symbols 0)
	(for-each (lambda (pair) (set! all_symbols (+ all_symbols (cdr pair)))) occur)

	; підрахунок ймовірностей кожного символу
	(define freqs (alist-copy occur))
	(map (lambda (pair) (set-cdr! pair (/ (cdr pair) all_symbols))) freqs)

	; сортування за алфавітним порядком
	; (define freqs-alpha-order '())
	; (for-each (lambda (letter)
	; 	(let ((pair (assq letter freqs)))
	; 		(when pair
	; 		(set! freqs-alpha-order (append freqs-alpha-order (list pair))))))
	; 	ukr-alphabet-sym)
	; (set! freqs freqs-alpha-order)

	; сортування за спаданням ймовірності
	; (set! freqs (sort freqs (lambda (a b)
	; 	(> (cdr a) (cdr b)))))

	; вивід пар символ-ймовірність у csv форматі
	; (for-each (lambda (pair)
	; 	(printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair))))
	; 	freqs)

	; ; послідовність літер по мірі спадання частоти появи
	; (for-each (lambda (pair)
	; 	(display (car pair))
	; 	(display " ")) freqs)
	; (newline)
)
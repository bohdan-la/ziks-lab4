(import utf8 srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port))

(define (main args)
	(define file (car args))
	(define sym_freq '((сміття . 0)))

	; заповнення базового масиву частотами появу кожного символу
	(with-input-from-file file
		(lambda ()
			(let loop ((ch (read-char)))
				(unless (eof-object? ch)
				(alist-update! sym_freq (string->symbol (string ch)) add1 (lambda () 0))
			(loop (read-char))))))

	; видалення службової пари 
	(set! sym_freq (alist-delete 'сміття sym_freq))
	; заміна символу нового рядка на принтабельний
	;(set-car! (assq (string->symbol (string #\newline)) sym_freq) '\\n')
	(let ((nline_pair (assq (string->symbol (string #\newline)) sym_freq)))
		(when nline_pair
			(set-car! nline_pair '\\n')))

	; підрахунок загальної кількості символів файлу
	(define total_symbols 0)
	(for-each (lambda (pair) (set! total_symbols (+ total_symbols (cdr pair)))) sym_freq)
	
	; підрахунок ймовірностей кожного символу
	(define sym_posib (alist-copy sym_freq))
	(map (lambda (pair) (set-cdr! pair (/ (cdr pair) total_symbols))) sym_posib)
	
	; підрахунок ентропії
	(define entropy 0)
	(for-each (lambda (val) (set! entropy (- entropy (* val (log val 2))))) (alist-values sym_posib))
	(define info_amount (* entropy total_symbols))

	(print "Кількість символів: " total_symbols)
	(print "Ймовірності симовлів:\n" sym_posib)
	(newline)
	(print "Ентропія: " entropy " бітів")
	(print "Кількість інформації: " (/ info_amount 8) " байтів")
	(print "Розмір файлу: " (file-size file) " байтів")
)

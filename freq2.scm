(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))

(define ukrainian-alphabet '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і
	#\ї #\й #\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц
	#\ч #\ш #\щ #\ь #\ю #\я #\' #\space))

;; Перетворює символ у символ типу symbol
(define ch-to-sym
	(lambda (ch)
		(string->symbol (string ch))))

;; Перетворює символ типу symbol у character
(define (sym-to-ch sym)
	(string-ref (symbol->string sym) 0))

;; Знижує регістр символа
(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

;; Функція для підрахунку біграм у файлі
(define (add-file-bigram-occur file occur)
	(with-input-from-file file
		(lambda ()
			(let loop ((prev-ch (read-char)) (ch (read-char)))
				(unless (or (eof-object? prev-ch) (eof-object? ch))
					(set! prev-ch (ch-to-lower prev-ch)) ;; робимо обидва символи нижнього регістру
					(set! ch (ch-to-lower ch))
					
						(let* ((bigram-str (string prev-ch ch)) ;; створюємо рядок із двох символів
							   (bigram (string->symbol bigram-str))) ;; перетворюємо його у символ
							(if (assq bigram occur)
								(alist-update! occur bigram add1 (lambda () 0))
								(alist-set! occur bigram 1)))
				(loop ch (read-char))))))
	occur)

;; Основна функція
(define (main args)
	(set! ukrainian-alphabet (map ch-to-sym ukrainian-alphabet))
	(define occur '())

	;; Підрахунок кількості появи біграм у всіх заданих файлах
	(for-each (lambda (file) 
		(set! occur (add-file-bigram-occur file occur))) args)

	;; Підрахунок загальної кількості біграм
	(define all_bigrams 0)
	(for-each (lambda (pair) (set! all_bigrams (+ all_bigrams (cdr pair)))) occur)

	;; Підрахунок ймовірностей кожної біграми
	(define freqs (alist-copy occur))
	(map (lambda (pair) (set-cdr! pair (/ (cdr pair) all_bigrams))) freqs)

	;; Сортування в алфавітному порядку
	(set! freqs (sort! freqs (lambda (a b)
		(string<? (symbol->string (car a)) (symbol->string (car b))))))

	;; Сортування за спаданням ймовірності (закоментовано для прикладу)
	; (set! freqs (sort freqs (lambda (a b) (> (cdr a) (cdr b)))))

	;; Виведення пар біграма-ймовірність у форматі CSV
	(for-each (lambda (pair)
		(printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair))))
	freqs)

	;; Виведення біграм для перевірки
	(for-each (lambda (pair)
		(display (car pair))
		(display " "))
	freqs)
	(newline))
(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))

;; знижує регістр символа
(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

;; функція для підрахунку біграм у файлі
(define (add-file-bigram-occur file occur)
	(with-input-from-file file
		(lambda ()
			(let loop ((prev-ch (read-char)) (ch (read-char)))
				(when (eq? ch #\space) (set! ch #\_))
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

;; основна функція
(define (main args)
	(define occur '())

	;; підрахунок кількості появи біграм у всіх заданих файлах
	(for-each (lambda (file) 
		(set! occur (add-file-bigram-occur file occur))) args)

	;; підрахунок загальної кількості біграм
	(define all_bigrams 0)
	(for-each (lambda (pair) (set! all_bigrams (+ all_bigrams (cdr pair)))) occur)

	;; підрахунок ймовірностей кожної біграми
	(define freqs (alist-copy occur))
	(map (lambda (pair) (set-cdr! pair (/ (cdr pair) all_bigrams))) freqs)

	;; сортування за спаданням ймовірності (закоментовано для прикладу)
	(set! freqs (sort freqs (lambda (a b) (> (cdr a) (cdr b)))))

	;; виведення пар біграма-ймовірність у форматі CSV
	; (for-each (lambda (pair)
	; 	(printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair))))
	; 	freqs)

	; виведення 30 найбільш ймовірних біграм
	(define freqs-30 '())
	(let loop ((freqs freqs) (i 0))
		(when (and (not (null? freqs)) (< i 30))
			(let ((pair (car freqs)))
				(printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair)))
				(set! freqs-30 (append freqs-30 (list (caar freqs)))))
			(loop (cdr freqs) (+ i 1))))
	(for-each (lambda (bigram)
		(printf "\"~A\" " bigram))
		freqs-30)
	(newline)

	;; виведення біграм для перевірки
	; (for-each (lambda (pair)
	; 	(display (car pair))
	; 	(display " ")) freqs)
	; (newline)
)
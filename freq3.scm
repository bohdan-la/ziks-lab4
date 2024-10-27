(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))

; знижує регістр символа
(define (ch-to-lower ch)
	(string-ref (utf8-string-downcase (string ch)) 0))

; функція для підрахунку триграм у файлі
(define (add-file-trigram-occur file occur)
	(with-input-from-file file
		(lambda ()
			(let loop ((prev2-ch (read-char)) (prev-ch (read-char)) (ch (read-char)))
				(when (eq? ch #\space) (set! ch #\_))
				(unless (or (eof-object? prev2-ch) (eof-object? prev-ch) (eof-object? ch))
					(set! prev2-ch (ch-to-lower prev2-ch)) ;; робимо всі три символи нижнього регістру
					(set! prev-ch (ch-to-lower prev-ch))
					(set! ch (ch-to-lower ch))
					(let* ((trigram-str (string prev2-ch prev-ch ch)) ;; створюємо рядок із трьох символів
						   (trigram (string->symbol trigram-str))) ;; перетворюємо його у символ
						(if (assq trigram occur)
							(alist-update! occur trigram add1 (lambda () 0))
							(alist-set! occur trigram 1)))
				(loop prev-ch ch (read-char))))))
	occur)

;; основна функція
(define (main args)
	(define occur '())

	;; підрахунок кількості появи триграм у всіх заданих файлах
	(for-each (lambda (file) 
		(set! occur (add-file-trigram-occur file occur))) args)

	;; підрахунок загальної кількості триграм
	(define all_trigrams 0)
	(for-each (lambda (pair) (set! all_trigrams (+ all_trigrams (cdr pair)))) occur)

	;; підрахунок ймовірностей кожної триграми
	(define freqs (alist-copy occur))
	(map (lambda (pair) (set-cdr! pair (/ (cdr pair) all_trigrams))) freqs)

	;; сортування за спаданням ймовірності
	(set! freqs (sort freqs (lambda (a b) (> (cdr a) (cdr b)))))

    ;; виведення пар триграма-ймовірність у форматі csv
	(for-each (lambda (pair)
		(printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair))))
		freqs)


	;; виведення 30 найбільш ймовірних триграм
	; (define freqs-30 '())
	; (let loop ((freqs freqs) (i 0))
	; 	(when (and (not (null? freqs)) (< i 30))
	; 		(let ((pair (car freqs)))
	; 			(printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair)))
	; 			(set! freqs-30 (append freqs-30 (list (caar freqs)))))
	; 		(loop (cdr freqs) (+ i 1))))
	; (for-each (lambda (trigram)
	; 	(printf "\"~A\" " trigram))
	; 	freqs-30)
	; (newline)

	;; виведення триграм для перевірки
	; (for-each (lambda (pair)
	; 	(display (car pair))
	; 	(display " ")) freqs)
	; (newline)
)

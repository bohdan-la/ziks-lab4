(import utf8 utf8-case-map srfi-1 alist-lib args (chicken file posix) (chicken process-context) (chicken port) (chicken format) (chicken sort))

(define ukrainian-alphabet '(#\а #\б #\в #\г #\ґ #\д #\е #\є #\ж #\з #\и #\і #\ї #\й
                            #\к #\л #\м #\н #\о #\п #\р #\с #\т #\у #\ф #\х #\ц #\ч
                            #\ш #\щ #\ь #\ю #\я #\' #\space))

;; Функція для перетворення символа в символ типу symbol
(define ch-to-sym
  (lambda (ch)
    (string->symbol (string ch))))

;; Функція для зниження регістру символа
(define (ch-to-lower ch)
  (string-ref (utf8-string-downcase (string ch)) 0))

;; Основна функція для підрахунку біграм
(define (main args)
  (define file (car args))  ;; Вхідний файл
  (define occur '())  ;; Алфавітний список біграм

  (with-input-from-file file
    (lambda ()
      (let loop ((prev-ch (read-char)) (ch (read-char)))
        (unless (or (eof-object? ch) (eof-object? prev-ch))  ;; Перевіряємо, чи не кінець файлу
          (set! prev-ch (ch-to-lower prev-ch))  ;; Робимо обидва символи нижнього регістру
          (set! ch (ch-to-lower ch))
          (if (and (memq prev-ch ukrainian-alphabet) (memq ch ukrainian-alphabet))
              (let* ((bigram-str (string prev-ch ch))  ;; Формуємо рядок із двох символів
                     (bigram (string->symbol bigram-str)))  ;; Перетворюємо рядок у symbol
                (if (assq bigram occur)
                    (alist-update! occur bigram add1 (lambda () 0))
                    (alist-set! occur bigram 1))))
          (loop ch (read-char))))))  ;; Оновлюємо біграму: поточний стає попереднім

  ;; Підрахунок загальної кількості біграм
  (define all_bigrams 0)
  (for-each (lambda (pair) (set! all_bigrams (+ all_bigrams (cdr pair)))) occur)

  ;; Підрахунок ймовірностей кожної біграми
  (define freqs (alist-copy occur))
  (map (lambda (pair) (set-cdr! pair (/ (cdr pair) all_bigrams))) freqs)

  ;; Виведення результату
  (for-each (lambda (pair)
    (printf "~A, ~A~N" (car pair) (exact->inexact (cdr pair))))
  freqs)
(print occur)  ;; Повідомлення у випадку відсутності біграм
)
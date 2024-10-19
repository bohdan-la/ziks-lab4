(import (chicken io) (chicken format) (chicken bitwise) shell bitwise-utils)

; рядок-таблиця base64 кодування
(define base64_table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define (main args)
(define file-in (open-input-file (car args)))

; функція, що додає до блоку бітів новий байт
(define (make-block block byte)
		(bitwise-cons (arithmetic-shift-left block (- 8 (integer-length byte))) byte))

; функція, що перетворює блок бітів у символи згідно таблиці base64
(define (base64-convert block)	
		(list->string (map (lambda (n) (string-ref base64_table n)) (bitwise-split block 6))))

(define block 0)
(define str_part "")
; головний цикл
(let loop ((byte (read-byte file-in)))		; зчитує вхідний файл по байту
	(if (not (eof-object? byte))			; якщо не кінець файлу
		(begin										; то 
			(set! block (make-block block byte))	; додати байт до робочого блоку
			(when (> (integer-length block) 16)		; якщо блок уже мість 3 байти
				(begin											; то
					(set! str_part (base64-convert block))		; виконати конвертацію блока у base64
					(display str_part)							; і вивести результат
				(set! block 0)))								; та очистити блок
			(loop (read-byte file-in)))				; якщо блок не повний - читати наступний
		(begin								; якщо кінець файлу
			(case (integer-length block)		
			((1 2 3 4 5 6 7 8) 					; якщо залишився лише один байт, то перетворити його у base64 згідно алгоритму
				(set! str_part (string-append (base64-convert (arithmetic-shift-left block 4)) "=="))	; 
				(display str_part))					; і вивести
			((9 10 11 12 13 14 15 16)			; якщо залишилося два байти, то перетворити його у base64 згідно алгоритму
				(set! str_part (string-append (base64-convert (arithmetic-shift-left block 2)) "="))
				(display str_part)))				; і вивести
			)))
)
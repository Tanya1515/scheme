#lang scheme/base
(require scheme/string)
(require racket/format)
(require racket/set)

;размер окна + 1
(define N 3)

;ОБУЧАЮЩАЯ ЧАСТЬ: создаем сруктуру -> обучаемся на текстовых данных -> заполняем структуру -> сохранить структуру в файле -> при необходимости пополнять структуру в файле
;ГЕНЕРИРУЮЩАЯ ЧАСТЬ: считываем структуру из файла -> используем ее при построении результата "прямым" и "смешанным" способом 

(define end_punct (set "." "!" "?"))
(define punct (set "," ";" ":" "." "!" "?"))

;файл, где находится результат
(define file_f "/Users/tanya/Desktop/MSU/4 курс/fp/test_forward.txt")
(define file_b "/Users/tanya/Desktop/MSU/4 курс/fp/test_back.txt")
(define file_s "/Users/tanya/Desktop/MSU/4 курс/fp/test_start.txt")
;файл, куда писать резултат
(define file "/Users/tanya/Desktop/MSU/4 курс/fp/test.txt")

;записываем хэш-таблицу в файл 
(define (res_to_file file_1 ht)
(with-output-to-file file_1
 (lambda () (print ht)
    )
 )
)

;считываем хэш-таблицу из файла (пока криво)
(define (text_from_file file)
  (call-with-input-file file
    (lambda (in) (read-line in))
   )
 )

;задаем хэш-таблицы
(define ht_res_forward ( make-hash ))
(define ht_res_back ( make-hash ))
(define ht_start ( make-hash ))


(define (string_to_list str)
 (filter non-empty-string? (string-split str #px"\\s*\\b\\s*"))
 )

(define (list_to_string lst)
  (string-append (car lst) (string-join (map (lambda (x)
                    (if (not(set-member? punct x))
                        (string-append " " )
                        x
                     )) (cdr lst)) "") )
  )

;делим входиной текст на предложения 
(define (text_to_sentence text)
  (let split_text ((text_lst (string_to_list text)) (res '()))
    (if (null? text_lst) null
        (cond ((not(set-member? end_punct (car text_lst)))
               (split_text (cdr text_lst) (cons (car text_lst) res ) )
               )
              (else
               (begin
               (split_sentence (reverse(cons (car text_lst) res)) 0 1 ht_res_forward 0) 
               (split_sentence (reverse(cons (car text_lst) res)) 1 0 ht_res_back 0)
               (split_sentence (reverse(cons (car text_lst) res)) 0 1 ht_start 1)
               (split_text (cdr text_lst) '())
              )
            )
         )
       )
     )
  )

;парсинг каждого предложения

;Nowadays data analysis has become main advantage in all companies.
;Nowadays it = 1 (it_first 0)
;Nowadays data it = 2 (it_first 0)
;Nowadays data analysis it = 3 (it_first 0)
;Nowadays data analysis - > key; has -> field it = 0 (it_first 1)

(define (split_sentence lst it_start const ht_res start)
  (let split ((it_first 0) (it it_start) (res '()))
    (cond ((>= (+ it it_first) (length lst))
             ht_res
            )
            ((= it (- N const))
                (let ((word (list-ref lst (+ it_first (* it const)))) (ht (hash-ref ht_res (reverse res) #f)))
                   (if ht
                       (if start
                           (+ ht 1)
                        (if (hash-ref ht word #f)
                            (hash-set! ht word (+ (hash-ref ht word #f) 1))
                            (hash-set! ht word 1)
                        )
                       )
                        (hash-set! ht_res (reverse res) (if start
                                                            1
                                                            (make-hash (list (cons word 1)))))
                     )
                  )
                   (split (+ it_first 1) it_start '())
            ) 
          (else
            (if (and ( = start 1) (>= it_first 1))
             (print start)
             (split it_first (+ it 1) (cons (list-ref lst (+ it_first it)) res))
            )
          )
   )
  )
)






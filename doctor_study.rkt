#lang scheme/base
(require racket/string)
(require racket/format)
(require racket/set)
(require racket/vector)

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

;записываем хэш-таблицы в файл 
(define (res_to_file)
(define out_s (open-output-file file_s #:exists 'replace))
  (write ht_res_start out_s)
(close-output-port out_s)

(define out_f (open-output-file file_f #:exists 'replace))
  (write ht_res_forward out_f)
(close-output-port out_f)

(define out_b (open-output-file file_b #:exists 'replace))
  (write ht_res_back out_b)
(close-output-port out_b)
)



;считываем текст из файла
(define (text_from_file)
  (call-with-input-file file
    (lambda (in) (read-line in))
   )
  )

;задаем хэш-таблицы
(define ht_res_forward ( make-hash ))
(define ht_res_back ( make-hash ))
(define ht_res_start ( make-hash ))



(define (string_to_list str)
 (filter non-empty-string? (string-split str #px"\\s*\\b\\s*"))
 )

(define (list_to_string lst)
  (string-append (car lst) (string-join (map (lambda (x)
                    (if (not(set-member? punct x))
                        (string-append " " x)
                        x
                     )) (cdr lst)) "") )
  )

;Nowadays data analysis has become main advantage in all companies.
;Nowadays it = 1 (it_first 0)
;Nowadays data it = 2 (it_first 0)
;Nowadays data analysis it = 3 (it_first 0)
;Nowadays data analysis - > key; has -> field it = 0 (it_first 1)

(define (split_sentence lst it_start const ht_res start)
  (let split ((it_first 0) (it it_start) (res '()))
    (cond ((>= (+ it it_first) (length lst))
             (if (= it_start 1)
                  (let ((word (list-ref lst it_first)) (ht (hash-ref ht_res (reverse res) #f)))
                    (if ht
                        (if (hash-ref ht word #f)
                             (hash-set! ht word (+ (hash-ref ht word #f) 1))
                             (hash-set! ht word 1)
                             )
                         (hash-set! ht_res (reverse res) (make-hash (list (cons word 1))))
                      )
                   )
                  null
              )
            )
            ((= it (- N const))
                (let ((word (list-ref lst (+ it_first (* it const)))) (ht (hash-ref ht_res (reverse res) #f)))
                   (if ht
                       (if (= start 1)
                           (hash-set! ht_res (reverse res) (+ ht 1))
                        (if (hash-ref ht word #f)
                            (hash-set! ht word (+ (hash-ref ht word #f) 1))
                            (hash-set! ht word 1)
                        )
                       )
                       (if (= start 1)
                           (hash-set! ht_res (reverse res) 1)
                           (hash-set! ht_res (reverse res) (make-hash (list (cons word 1))))
                           )
                     )
                  )
                   (split (+ it_first 1) it_start '())
            ) 
          (else
            (if (and ( = start 1) (>= it_first 1))
             null
             (split it_first (+ it 1) (cons (list-ref lst (+ it_first it)) res))
            )
          )
   )
  )
)



;делим входиной текст на предложения
;добавить считаывание из файла 
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
               (split_sentence (reverse(cons (car text_lst) res)) 0 1 ht_res_start 1)
               (split_text (cdr text_lst) '())
              )
            )
         )
       )
     )
  ) 
   
;(foldl + 1 (hash-values ht_res_start))
(define (select ht)
  (let find ( (list_keys (hash-keys ht)) (num (random 1 (foldl + 1 (hash-values ht))) )) 
    (if (null? list_keys) null
        (if (<= num (hash-ref ht (car list_keys) #f))
            (car list_keys)
            (find (cdr list_keys) (- num (hash-ref ht (car list_keys) #f)))
         )
    )
   )
)

;смешанное составление предложения 
(define (make-answer-mix part_user_response)
   ;(res_from_file file_start file_forward file_back)
   (text_to_sentence (text_from_file))
   (append (make-answer-back part_user_response) (make-answer-forward part_user_response) )
  
 ) 

;(make-answer-back '("very" "quickly"))
(define (make-answer-back first)
  ;(res_from_file file_start file_forward file_back)
  (let make_answer ((part_phrase first) (all_phrase '()))
    (let ((next (select (hash-ref ht_res_back part_phrase))))
      (let ((part_phrase_next (cons next (reverse (cdr (reverse part_phrase))))))
     (if (hash-ref ht_res_start part_phrase_next #f)
            (cons next all_phrase)
            (make_answer part_phrase_next (cons next all_phrase))
       )
   )
  )    
 )
)

;выбираем с учетом веса начало предложения -> ищем в структуре начало предложения -> с учетом веса выбираем слово для продолжения -> 
;прямое составление предложения

(define (make-answer-forward first)
  ;(res_from_file file_start file_forward file_back)
  (let make_answer ((part_phrase first) (all_phrase (reverse first)))
    (let ((next (select (hash-ref ht_res_forward part_phrase))))
     (if (set-member? end_punct next)
            (reverse (cons next all_phrase))
            (make_answer (append (cdr part_phrase) (list next) ) (cons next all_phrase))
            )
           )
       )
   )
;парсинг каждого предложения








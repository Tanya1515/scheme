#lang scheme/base


; "Доктор". Осень 2021
; В учебных целях используется базовая версия Scheme

; подключаем функции для работы с векторами
(require racket/vector)
; подключаем функции для работы со строками
(require scheme/string)
(require racket/format)
(require racket/set)

(define keywords_structure
 '#(
  ( ; начало данных 1й группы
    ("depressed" "suicide" "exams" "university") ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  ("when you feel depressed, go out for ice cream")
          ("depression is a disease that can be treated")
          ("i will help you to overcome depression")
          ("a lot of people suffer from depression, you are not alone")
    )
  ) ; завершение данных 1й группы
  
  ( ; начало данных 2й группы 
    ("mother" "father" "parents" "brother" "sister" "uncle" "ant" "grandma" "grandpa")
        (
	  ("tell me more about your" * ", i want to know all about your" *)
          ("why do you feel that way about your" * "?")
          ("your relationships with" * "are very important for diagnosis, so continue please")
          ("how do you feel while talking about you problems with" * "?")
	)
  ) ; завершение данных 2й группы
  
  ( ; начало данных 3й группы 
    ("university" "scheme" "lections" "seminars" "studies")
	(
	  ("your education is important")
	  ("how many time do you spend for learning ?")
          ("you do not have to spend all time for studying, you should have a relax day once a week")
          ("i suggest to make a timetable of your work in order not to overextend")
	)
  ) ; завершение данных 3й группы

   ( ; начало данных 4й группы 
    ("sleep" "insomnia" "drowsiness" "tiredness")
	(
	  ("do you sleep good?")
	  ("how long do you sleep?")
          ("at what time do you go to bed?")
          ("what do you do before going to bed?")
	)
  ) ; завершение данных 4й группы

    ( ; начало данных 5й группы 
    ("scared" "horror" "stress" "consternation")
	(
	  ("do you have any phobias?")
	  ("what were you most afraid of as a child?")
          ("it is ok to be scared of something, we need to understand the reason of the horror, so please go on")
          ("many people suffer from stress every day, do not worry")
	)
  ) ; завершение данных 5й группы
 )
)

(define struct_strat ;структура всех стратегий генерации ответа 
  (list (list  (lambda (x y) #t) 1 (lambda(x y)(hedge)))
        (list  (lambda (x y) #t) 2 (lambda(x y)(qualifier-answer x)))
        (list  (lambda (x y) (not(vector-empty? y))) 3 (lambda (x y) (history-answer y)))
        (list  (lambda (x y) (check-for-keywords x)) 4 (lambda (x y) (find_key_answer x)))) 
  )

;размер окна 
(define N 3)

;функция обработки ввода
;\b - конец слова
;\s - символ, в том числе пробел 
(define (string_to_list str)
 (filter non-empty-string? (string-split str #px"\\s*\\b\\s*"))
 )

;множество пунктуационных знаков
(define punct (set "," ";" ":" "." "!" "?"))
(define end_punct (set "." "!" "?"))

;файл, где лежит текст 
(define file_f "/Users/tanya/Desktop/MSU/4 курс/fp/test_forward.txt")
(define file_b "/Users/tanya/Desktop/MSU/4 курс/fp/test_back.txt")
;файл, куда писать резултат
(define file "/Users/tanya/Desktop/MSU/4 курс/fp/test.txt")

(define (res_to_file file_1 ht)
(with-output-to-file file_1
 (lambda () (print ht)
    )
 )
)
(define (text_from_file file)
  (call-with-input-file file
    (lambda (in) (read-line in))
   )
 )

;задаем хэш-таблицы
(define ht_res_forward ( make-hash ))
(define ht_res_back ( make-hash ))
(define ht_start ( make-hash ))


;функция разделения текста на предложения 
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

;добавление данных в хэш-таблицу для прямого, смешанного обходов и для начал предложений
(define (split_sentence lst it_start const ht_res start)
  (let split ((it_first 0) (it it_start) (res '()))
    (cond ((>= (+ it it_first) (length lst))
             ht_res
            )
            ((= it (- N const))
                (let ((word (list-ref lst (+ it_first (* it const)))) (ht (hash-ref ht_res (reverse res) #f)))
                   (if ht
                       (if (hash-ref ht word #f)
                            (hash-set! ht word (+ (hash-ref ht word #f) 1))
                            (hash-set! ht word 1)
                        )
                        (hash-set! ht_res (reverse res) (make-hash (list (cons word 1))))
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

;функция, преобразующая список в строку 
(define (list_to_string lst)
  (string-append (car lst) (string-join (map (lambda (x)
                    (if (not(set-member? punct x))
                        (string-append " " x)
                        x
                     )) (cdr lst)) "") )
  )


; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (printf "what seems to be the trouble?")
  (doctor-driver-loop name)
)

(define (ask-patient-name) ;возвращает имя пациента  
 (begin
  (printf "next!\n")
  (printf "who are you?\n")
  (print '**)
  (car (string_to_list (read-line)))
 ) 
)

(define (visit-doctor-v2 stop_word amount_of_patients) ;передается стоп слово и количество обслуживаемых пациентов
  (if (<= amount_of_patients 0)
      (printf "time to go home\n") ;если количество пациентов <= 0, то доктор уходит домой
  (let ask ((patient_name (ask-patient-name)) (stop stop_word) (clients amount_of_patients)) ;считывается имя пациента   
    (cond
      ((equal? patient_name (~v stop)) ;если имя пациента и стоп слово не совпадают, то начинаем сеанс с доктором, иначе время идти домой 
        (printf "time to go home\n"))
        (else
           (visit-doctor patient_name) 
           (if (= clients 1) 
               (printf "time to go home")
               (ask (ask-patient-name) stop_word (- clients 1))) ;считываем имя следующего пациента и уменьшаем количество оставшихся для обслуживания пациентов на 1
         )
        )
      )
     )
   )


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (string_to_list (read-line))))
      (cond
            ((equal?  (list (car user-response)) '("goodbye")) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (printf "see you next week\n"))
            (else (printf (list_to_string (reply user-response))) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
     )
)

(define (doctor-driver-loop-v2 name)
  (let loop (( answer-vctr #() ))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (string_to_list (read-line))))
      (cond
            ((equal? (list (car user-response)) '("goodbye")) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (printf "see you next week\n"))
            (else (printf (list_to_string (reply-v2 user-response struct_strat answer-vctr))) ; иначе Доктор генерирует ответ, печатает его 
                  (loop (vector-append (vector user-response) answer-vctr)); Доктор продолжает цикл
             )  
      )
    )
  )
)

(define (reply-v3 user-response answer-vctr)
      (case (random 4)  
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge)) ; 2й способ
          ((2)((if (vector-empty? answer-vctr)
                   (history-answer answer-vctr)
                   hedge)
               )) ;3й способ
          ((3) (if (check-for-keywords user-response)
                   (find_key_answer user-response)
                   hedge) )) ;4й способ 
)

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply user-response)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge)) ; 2й способ
      )
)


(define (weight lst) ;возвращает список весов всех стратегий (номер стратегии = номер веса в списке)
  (let loop ((it (- (length lst) 1)) (res '())) 
    (if (< it 0) res 
        (loop (- it 1) (cons (cadr (list-ref lst it)) res))
     )
   )
 ) 


(define (choose_strat struct_strat) ;возвращается номер вызываемой стратегии, который совпадает с номером ее веса в списке весов 
  (let loop ((lst struct_strat) (random_number (random (foldl + 0 (weight struct_strat))) ))
    (cond ((null? lst) #f)
          ((<= random_number (list-ref (car lst) 1)) (car lst))
         (else (loop  (cdr lst) (- random_number (list-ref (car lst) 1)))) 
    )
 ) 
) 

(define (reply-v2 user-response struct answer-vctr) ;вызывается функция-чеккер, если она возвращает true, вызываем саму функцию
  (let ((cur_strat (choose_strat (filter (lambda (x) ((list-ref x 0) user-response answer-vctr)) struct) ) ))
    ((caddr cur_strat) user-response answer-vctr)
   )
)

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
   (append (pick-random-vector '#(("you seem to think that")
                                       ("you feel that")
                                       ("why do you believe that")
                                       ("why do you say that")
                                       ("what do you feel when")
                                       ("are you scared when")
                                       ("what are you going to do in situation when"))
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random (vector-length vctr)))
)

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3 '(("am" "are")
                        ("are" "am")
                        ("i" "you")
                        ("me" "you")
                        ("mine" "yours")
                        ("my" "your")
                        ("myself" "yourself")
                        ("you" "i")
                        ("your" "my")
                        ("yours" "mine")
                                                ("yourself" "myself")
                                                ("we" "you")
                                                ("us" "you")
                                                ("our" "your")
                                                ("ours" "yours")
                                                ("ourselves" "yourselves")
                                                ("yourselves" "ourselves")
                                                ("shall" "will"))
                      phrase)
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается прежнее начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
  )
;many-replace-v2 - изменяет местоимения в предложениях клиеента. 
(define (many-replace-v2 replacement-pairs lst)
  (let loop ((lst lst) (res '()))
    (if (null? lst)
        (reverse res)
        (let ((elem (assoc (car lst) replacement-pairs))) ; (assoc <элемент списка> <структурированный список>). проходимся по фразе больного и ищем слова для замены из списка replacement-pairs
               (loop (cdr lst)
                     (cons (if elem
                               (cadr elem) ; (car (cdr x))
                               (car lst))
                           res)
               ) 
        )
    )
  )
)

(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (x)(let ((pat-rep (assoc x replacement-pairs)))
                    (if pat-rep
                        (cadr pat-rep)
                        x
                    )
                  )
       ) lst
  ) 
)

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random-vector '#(("please go on")
                              ("many people have the same sorts of feelings")
                              ("many of my patients have told me the same thing")
                              ("please continue")
                              ("please tell me more details about yor feelings at that moment")
                              ("it looks like you are scared. relax, just be yourself and plaese continue")
                              ("it is normal to feel this way"))
         )
)

;3й способ генерации ответной реплики: earlier you said that + случайны выбор реплики, произносимой клиентом ранее
;по номеру элемента вектора реплик (определяется рандомно при помощи функции random, куда передается длинна вектора) выбирается реплика вдобавок к фразе earlier you said that
(define (history-answer answer-vctr)
  (append `("earlier you said that") (vector-ref answer-vctr (random (vector-length answer-vctr)))) 
  )

(define (list_key vctr) ; составляем список всех ключевых слов
  ( let loop ((res '()) (it (- (vector-length vctr) 1)))
     (if (< it 0)
         res
         (loop (append (car (vector-ref vctr it)) res) (- it 1))
     )
  )
)

(define (check-for-keywords phrase)
  (ormap (lambda (x) (member x (list_key keywords_structure))) phrase) ; проходимся по фразе в поисках первого ключа, если он есть 
  )

(define (all_phrase_keywords phrase)
  (filter (lambda (x)
           (member x (list_key keywords_structure))
         ) phrase
     )
 )

(define (random-elem-lst lst) ;выбирает рандомный лемент списка
  (list-ref lst (random (length lst)))
 )

(define (all_inf_by_key word struct) ;составляет список фраз, относящихся к конкретному ключевому слову 
  ( let loop ((res '()) (it (- (vector-length struct) 1)))
     (if (< it 0)
         res
         (if (member word (car (vector-ref struct it)))
             (loop (append (car (cdr (vector-ref struct it))) res) (- it 1))
             (loop res (- it 1))
          )
     )
  )
)

(define (change_answer phrase word) ; осуществляет замену звездочки
    (many-replace-v3 (list(list '* word)) phrase)
 ) 

(define (find_key_answer phrase) 
  (let ( (word (random-elem-lst (all_phrase_keywords phrase) )) )
   (change_answer (random-elem-lst (all_inf_by_key word keywords_structure) ) word)
   )
)
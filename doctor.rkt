; "Доктор". Осень 2021
#lang scheme/base
; В учебных целях используется базовая версия Scheme

(require racket/vector)
; подключаем функции для работы с векторами

(define keywords_structure
 '#(
  ( ; начало данных 1й группы
    (depressed suicide exams university) ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
          (i will help you to overcome depression)
          (a lot of people suffer from depression, you are not alone)
    )
  ) ; завершение данных 1й группы
  
  ( ; начало данных 2й группы 
    (mother father parents brother sister uncle ant grandma grandpa)
        (
	  (tell me more about your * , i want to know all about your *)
          (why do you feel that way about your * ?)
          (your relationships with * are very important for diagnosis, so continue please)
          (how do you feel while talking about you problems with *?)
	)
  ) ; завершение данных 2й группы
  
  ( ; начало данных 3й группы 
    (university scheme lections seminars studies)
	(
	  (your education is important)
	  (how many time do you spend for learning ?)
          (you do not have to spend all time for studying, you should have a relax day once a week)
          (i suggest to make a timetable of your work in order not to overextend)
	)
  ) ; завершение данных 3й группы

   ( ; начало данных 4й группы 
    (sleep insomnia drowsiness tiredness)
	(
	  (do you sleep good?)
	  (how long do you sleep?)
          (at what time do you go to bed?)
          (what do you do before going to bed?)
	)
  ) ; завершение данных 4й группы

    ( ; начало данных 5й группы 
    (scared horror stress consternation)
	(
	  (do you have any phobias?)
	  (what were you most afraid of as a child?)
          (it is ok to be scared of something, we need to understand the reason of the horror, so please go on)
          (many people suffer from stress every day, do not worry)
	)
  ) ; завершение данных 5й группы
 )
)


; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name)
)

(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

(define (visit-doctor-v2 stop_word amount_of_patients) ;передается стоп слово и количество обслуживаемых пациентов
  (if (<= amount_of_patients 0)
      (print '(time to go home)) ;если количество пациентов <= 0, то доктор уходит домой
  (let ask ((patient_name (ask-patient-name)) (stop stop_word) (clients amount_of_patients)) ;считывается имя пациента   
    (cond
      ((equal? patient_name stop) ;если имя пациента и стоп слово не совпадают, то начинаем сеанс с доктором, иначе время идти домой 
        (print '(time to go home)))
        (else
           (visit-doctor patient_name) 
           (if (= clients 1) 
               (print '(time to go home))
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
    (let ((user-response (read)))
      (cond
            ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
     )
)

(define (doctor-driver-loop-v2 name)
  (let loop (( answer-vctr #() ))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond
            ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply-v2 user-response answer-vctr)) ; иначе Доктор генерирует ответ, печатает его 
                  (loop (vector-append (vector user-response) answer-vctr)); Доктор продолжает цикл
             )  
      )
    )
  )
)

(define (reply-v2 user-response answer-vctr)
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



  (define struct_strat
 '#(
  ( ; начало описания 1й стратегии
    (qualifier-answer) ;название 1й стратегии
    (
	  (qualifier-answer_check) ;функция, проверяющая корректность использования данной стратегии
          (2) ;вес данной стратегии
          ( (lambda ()) (qualifier-answer user-response)) ;функция стратегии
    )
  ) ;конец описания 1й стратегии

  (
   (hedge)
   (
	  (hedge_check)
          (1)
          (hedge) 
    )
  )
  (
   (history-answer)
   (
	  ( (lambda ()) (history-answer_check answer-vctr))
          (3)
          ((lambda ()) (history-answer answer-vctr) )
    )
  )
  (
   (find_key_answer)
   (
	  ( (lambda ()) (find_key_answer_check user-response))
          (4)
          ( (lambda ()) (find_key_answer user-response) )
    )
  )
 )
)

(define (choose_strat struct_strat)
  (let ((random_number (random 10) ))
    (cond ((and (>= random_number 0) (<= random_number 3))
           (all_inf_by_key (cadddr (list_key struct_strat)) struct_strat))
          ((and (>= random_number 4) (<= random_number 6))
           (all_inf_by_key (caddr (list_key struct_strat)) struct_strat))
          ((or (= random_number 7) (= random_number 8))
           (all_inf_by_key (car (list_key struct_strat)) struct_strat))
          ((= random_number 9)
           (all_inf_by_key (cadr (list_key struct_strat)) struct_strat))
     )
    )
 )
  
(define (reply-v3 user-response struct_strat answer-vctr)
 )

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
   (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (what do you feel when)
                                       (are you scared when)
                                       (what are you going to do in situation when))
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
        (many-replace-v3 '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
                        (myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
                                                (yourself myself)
                                                (we you)
                                                (us you)
                                                (our your)
                                                (ours yours)
                                                (ourselves yourselves)
                                                (yourselves ourselves)
                                                (shall will))
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
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (please tell me more details about yor feelings at that moment)
                              (it looks like you are scared. relax, just be yourself and plaese continue)
                              (it is normal to feel this way))
         )
)

;3й способ генерации ответной реплики: earlier you said that + случайны выбор реплики, произносимой клиентом ранее
;по номеру элемента вектора реплик (определяется рандомно при помощи функции random, куда передается длинна вектора) выбирается реплика вдобавок к фразе earlier you said that
(define (history-answer answer-vctr)
  (append `(earlier you said that) (vector-ref answer-vctr (random (vector-length answer-vctr))))
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

(define (member? x list) ;принадлежит ли слово списку
     (if (null? list) #f                           
         (if (equal? x (car list)) #t              
              (member? x (cdr list)))))


(define (all_phrase_keywords phrase)
  (foldl (lambda (x res)
           (if (member? x (list_key keywords_structure))
               (cons x res)
               res)
         ) '() phrase
     )
 )

(define (random-elem-lst lst) ;выбирает рандомный лемент списка
  (list-ref lst (random (length lst)))
 )

(define (all_inf_by_key word struct) ;составляет список фраз, относящихся к конкретному ключевому слову 
  ( let loop ((res '()) (it (- (vector-length struct) 1)))
     (if (< it 0)
         res
         (if (member? word (car (vector-ref struct it)))
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
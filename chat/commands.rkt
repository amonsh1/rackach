#lang typed/racket

(provide
 parse-join-message
 parse-command
 handle-command
 command-message?)

(require typed/rackunit)

(require "exceptions.rkt")
(require "state/type.rkt")
(require "state/functions.rkt")
(require "channel/type.rkt")
(require "channel/functions.rkt")
(require "user/type.rkt")
(require "user/functions.rkt")

(define command-prefix : String "\\")

(: command-message? (-> String Boolean))
(define (command-message? message)
  (string-prefix? message command-prefix))

(: parse-join-message (-> (Listof String) (List String)))
(define (parse-join-message arguments)
  (cond [(= (length arguments) 1)
         (list (first arguments))]
        [else (raise (wrong-command-fail
                      "JOIN:неверное количество аргументов"
                      (current-continuation-marks)))]))


(: parse-create-message (-> (Listof String) (List String)))
(define (parse-create-message arguments)
  (cond [(= (length arguments) 1)
         (list (first arguments))]
        [else (raise (wrong-command-fail
                      "CREATE:неверное количество аргументов"
                      (current-continuation-marks)))]))

(: handle-join-message (-> (Listof String) user-struct state-struct state-struct))
(define (handle-join-message arguments user state)
  (match-define (list channel) (parse-join-message arguments))
  (define user-id (user-struct-id user))
  (define channel-to-join (hash-ref (state-struct-channels state)
                                    channel
                                    (λ ()(raise (wrong-command-fail
                                                 "JOIN:канал отсутствует"
                                                 (current-continuation-marks))))))
  
  (define state-channel-without-user (state-remove-user-from-channel state user-id))
  (define state-channel-with-user (state-add-user-to-channel (channel-struct-name channel-to-join)
                                                             user-id
                                                             state-channel-without-user))
  state-channel-with-user)


(: handle-leave-message (-> (Listof String) user-struct state-struct state-struct))
(define (handle-leave-message arguments user state)
  (state-remove-user-from-channel state (user-struct-id user)))


(: handle-create-message (-> (Listof String) user-struct state-struct state-struct))
(define (handle-create-message arguments user state)
  (state-add-channel state (make-new-channel (first (parse-create-message arguments)))))


(: handle-rename-message (-> (Listof String) user-struct state-struct state-struct))
(define (handle-rename-message arguments user state)
  (struct-copy state-struct state [users
                                   (hash-set (state-struct-users state)
                                             (user-struct-id user)
                                             (struct-copy user-struct
                                                          user
                                                          [nick (first arguments)]))]))


(: handle-signup-user (-> (Listof String) user-struct state-struct state-struct))
(define (handle-signup-user arguments user state)
  (user-signup state (user-struct-id user) (first arguments) (second arguments)))


(define commands-handlers : (Immutable-HashTable String
                                                 (-> (Listof String)
                                                     user-struct
                                                     state-struct
                                                     state-struct))
  (hash
   "join" handle-join-message
   "signup" handle-signup-user
   "create" handle-create-message
   "rename" handle-rename-message
   "leave" handle-leave-message))


(: parse-command (-> String (List String (Listof String))))
(define (parse-command message)
  (let* ([chunks (string-split message)]
         [command (string-trim (list-ref chunks 0) command-prefix)]
         [arguments (list-tail chunks 1)])
    (list command arguments)))


(: handle-command (-> String Integer state-struct state-struct))
(define (handle-command message user-id state)
  (match-define (list command arguments) (parse-command message))
  (define user (hash-ref (state-struct-users state) user-id))
  (println command)
  (println arguments)
  (define handler (hash-ref commands-handlers command))
  (handler arguments user state))


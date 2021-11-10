#lang typed/racket

(require typed/racket/async-channel)

(require "user/type.rkt")
(require "user/functions.rkt")
(require "exceptions.rkt")
(require "commands.rkt")
(require "channel/type.rkt")
(require "channel/functions.rkt")
(require "state/type.rkt")
(require "state/functions.rkt")
(require "user/type.rkt")
(require "messages-types.rkt")

(provide
 run)

#|Обрабатыват входные порты - отправляет в них сообщения или добавляет/удаляет их|#
(: output-process (-> (Async-Channelof OutputCommandMessage) Null))
(define (output-process output-process-channel)
  (let loop ([output-ports : (Immutable-HashTable Integer Output-Port) (hash)]); id->port
    (define message  ((inst sync OutputCommandMessage) output-process-channel))

    (println (format "выходное сообщение ~a" message))
    (sleep 0.5)

    (cond
      [(output-command-message-add? message)
       (let ([client-id (first (second message))]
             [output-port (second (second message))])
         (loop (hash-set output-ports client-id output-port)))]

      [(output-command-message-remove? message)
       (let ([client-id (first (second message))])
         (loop (hash-remove output-ports client-id)))]

      [(output-command-message-send? message)
       (let* ([client-id (first (second message))]
              [port (hash-ref output-ports client-id)]
              [message (second (second message))])
         (write-string message port)
         (newline port)
         (flush-output port)
         (loop output-ports))]
      )

    (println "неизвестная команда")
    (loop output-ports)))


#|Обработка сообщения, не являющегося командой|#
(: handle-not-command-message (-> String Integer state-struct state-struct))
(define (handle-not-command-message message user-id state)
  (define user-channel (channel-by-user user-id state))
  (define user-sender (state-get-user-by-id state user-id))
  (when (not (null? user-channel))
    (define users-ids-channel (map (λ ([user : user-struct]) (user-struct-id user))
                                   (users-in-channel (channel-struct-name user-channel) state)))
    (println users-ids-channel)
    (for-each ; отправляем сообщение всем пользователям в канале
     (λ ([u-id : Integer])
       (when (not (= u-id user-id))
         (async-channel-put output-process-channel (list 'send (list u-id (format "~s: ~s" (user-struct-nick user-sender) message))))))
     users-ids-channel))
  state)


#|Обработка входящий сообщений сообщения|#
(: handle-message (-> String Integer state-struct state-struct))
(define (handle-message message user-id state)
  (if (command-message? message)
      (with-handlers
          ([client-fail? (lambda ([e : client-fail])
                           (let ()
                             (async-channel-put output-process-channel
                                                (list 'send (list user-id (exn-message e))))
                             state))])

        (define updated-state (handle-command message user-id state))
        (println updated-state)
        updated-state)
      (handle-not-command-message message user-id state)))


(: run (-> Integer Null))
(define (run port)
  (println 1)
  (let ([control-channel (make-async-channel)]
        [tcp-listener (tcp-listen port)])
    
    (thread (λ () (output-process output-process-channel)))
    (println 1)
    (let loop : Null ([state : state-struct (make-new-state)]
                      [current-id : Integer 0])

      (sleep 1)
      ;(println state)
      ;оборачиваем эвенты входящих портов, чтобы возвращать сам порт тоже
      (define i-ports-evt-handlers (map (λ ([i-port : Input-Port])
                                          (handle-evt (read-line-evt i-port 'linefeed)
                                                      (λ ([message : (U String EOF)]) (list 'client-message i-port message))))
                                        (hash-keys (state-struct-i-ports-user-ids state))))

      (define m : MainLoopMessage (apply sync
                                         (append (list (tcp-accept-evt tcp-listener))
                                                 i-ports-evt-handlers)))
      (println m)
      (cond
        [(main-loop-message-new-client? m) ; новый клиент
         (let* ([input-port (first m)]
                [output-port (second m)])
           (async-channel-put output-process-channel
                              (list 'add (list current-id output-port)))

           (define usr (make-new-user current-id (list input-port output-port)))
           (define state-with-anon (anonim-user-signup state current-id))
           ;(define state-with-room (state-add-channel state-with-anon (make-new-channel "room")))
           (loop (state-add-user state-with-anon usr)
                 (add1 current-id)))]

        [(and (main-loop-message-client-message? m) (eof-object? (third m))) ; eof - отключился
         (let* ([i-port (second m)]
                [user-id (hash-ref (state-struct-i-ports-user-ids state) i-port)]
                [user (hash-ref (state-struct-users state) user-id)]
                [user-ports (user-struct-io-ports user)])
           (close-input-port (first user-ports))
           (close-output-port (second user-ports))
           (async-channel-put output-process-channel
                              (list 'remove (list user-id)))
           (loop (state-remove-user state user-id)
                 current-id))]

        [(main-loop-message-client-message? m)
         (begin
           (let* ([i-port (second m)]
                  [user-id (hash-ref (state-struct-i-ports-user-ids state) i-port)]
                  [user (hash-ref (state-struct-users state) user-id)]
                  [user-oport (second (user-struct-io-ports user))])
             (define new-state (handle-message (third m) user-id state))
             (loop new-state
                   current-id))
           )]
        )
      )
    )
  )







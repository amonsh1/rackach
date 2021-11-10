#lang typed/racket



(require typed/racket/async-channel)

(require/typed openssl/md5
               [md5 (Input-Port -> String)])

(require/typed racket/random
               [crypto-random-bytes (Exact-Positive-Integer -> Bytes)])

(require racket/random)


(require "type.rkt")
(require "../user/type.rkt")
(require "../channel/type.rkt")
(require "../channel/functions.rkt")
(require "../exceptions.rkt")

(provide
 make-new-state
 state-add-channel
 state-remove-user
 state-add-user
 state-update-channels
 state-get-user-by-iport
 anonim-user-signup
 state-remove-user-from-channel
 state-add-user-to-channel
 users-in-channel
 user-signup
 state-get-user-by-id
 channel-by-user)

(: make-new-state (-> state-struct))
(define (make-new-state)
  (state-struct (hash)
                (hash)
                (hash)
                (hash)
                (hash)
                (hash)))


(: user-login (-> state-struct String String Integer state-struct))
(define (user-login state login password user-id)
  (define hashed-password (hash-ref (state-struct-auth state)
                                    login
                                    (λ () (raise (login-error
                                                  "Пользователь не найден"
                                                  (current-continuation-marks))))))

  (when (not (equal? hashed-password (md5 (open-input-string password))))
    (raise (login-error
            "Неверный логи или пароль"
            (current-continuation-marks))))

  ; ищем нет ли уже залогиненного пользователя с таким логином
  (define already-logged (findf (λ ([kv : (Pairof Integer String)]) (equal? (cdr kv) login))
                                (hash->list (state-struct-user-auth state))))

  (when already-logged
    (raise (login-error
            "Кто-то уже залеш с этим логином"
            (current-continuation-marks))))

  (struct-copy state-struct
               state
               [user-auth (hash-set (state-struct-user-auth state) user-id login)]))


(: user-signup (-> state-struct Integer String String state-struct))
(define (user-signup state user-id login password)

  (struct-copy state-struct
               state
               [auth (hash-set (state-struct-auth state)
                               login
                               (md5 (open-input-string password)))]
               [user-auth (hash-set (state-struct-user-auth state)
                                    user-id
                                    login)])
  )



(: anonim-user-signup (-> state-struct Integer state-struct))
(define (anonim-user-signup state user-id)

  (define random-login (~a (crypto-random-bytes 16)))

  (struct-copy state-struct
               state
               [auth (hash-set (state-struct-auth state) random-login '())]
               [user-auth (hash-set (state-struct-user-auth state) user-id random-login)])
  )


(: state-get-user-by-id (-> state-struct Integer user-struct))
(define (state-get-user-by-id state user-id)
  (hash-ref (state-struct-users state) user-id))


(: state-get-user-by-iport (-> state-struct Input-Port user-struct))
(define (state-get-user-by-iport state iport)
  (define user-id (hash-ref (state-struct-i-ports-user-ids state) iport))
  (hash-ref (state-struct-users state) user-id))


(: state-update-channels (-> state-struct
                             (Immutable-HashTable String channel-struct)
                             state-struct))
(define (state-update-channels state channels)
  (struct-copy state-struct
               state
               [channels channels]))


(: state-add-channel (-> state-struct channel-struct state-struct))
(define (state-add-channel state channel)
  (define channel-name (channel-struct-name channel))
  (define channels (state-struct-channels state))
  (struct-copy state-struct
               state
               [channels (hash-set channels
                                   channel-name
                                   channel)]))


#|Ищем в каком канале находится пользователь|#
(: channel-by-user (-> Integer state-struct (U channel-struct Null)))
(define (channel-by-user user-id state)
  (define channel-name (hash-ref (state-struct-user-channel state)
                                 user-id
                                 (λ () null)))
  (if (null? channel-name)
      null
      (hash-ref (state-struct-channels state) channel-name)))


#|Пользователи на канале|#
(: users-in-channel (-> String state-struct (Listof user-struct)))
(define (users-in-channel channel-name state)
  (define user-channel-pairs (hash->list (state-struct-user-channel state)))
  (define user-channel-filtered (filter (λ ([u-c : (Pair Integer String)]) (equal? channel-name (cdr u-c)))
                                        user-channel-pairs))

  (define state-users (state-struct-users state))

  (map (λ ([u-c : (Pair Integer String)]) (hash-ref state-users (car u-c))) user-channel-filtered))


(: state-remove-channel (-> state-struct String state-struct))
(define (state-remove-channel state channel-name)
  (define channels (state-struct-channels state))
  (struct-copy state-struct
               state
               [channels (hash-remove channels
                                      channel-name)]))


(: state-add-user (-> state-struct user-struct state-struct))
(define (state-add-user state user)
  (struct-copy state-struct
               state
               [users (hash-set (state-struct-users state)
                                (user-struct-id user)
                                user)]
               [i-ports-user-ids (hash-set (state-struct-i-ports-user-ids state)
                                           (first (user-struct-io-ports user))
                                           (user-struct-id user))]))



(: state-remove-user (-> state-struct Integer state-struct))
(define (state-remove-user state user-id)
  (let ([user : user-struct (hash-ref (state-struct-users state) user-id)])
    (struct-copy state-struct
                 state
                 [users (hash-remove (state-struct-users state)
                                     user-id)]
                 [i-ports-user-ids (hash-remove (state-struct-i-ports-user-ids state)
                                                (first (user-struct-io-ports user)))])))


(: state-add-user-to-channel (-> String Integer state-struct state-struct))
(define (state-add-user-to-channel channel-name user-id state)
  (struct-copy state-struct
               state
               [user-channel (hash-set (state-struct-user-channel state) user-id channel-name)]))


(: state-remove-user-from-channel (-> state-struct Integer state-struct))
(define (state-remove-user-from-channel state user-id)
  (struct-copy state-struct
               state
               [user-channel (hash-remove (state-struct-user-channel state) user-id)]))

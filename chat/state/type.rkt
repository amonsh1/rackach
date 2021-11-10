#lang typed/racket

(provide
 (struct-out state-struct)
 output-process-channel)


(require "../channel/type.rkt")
(require "../user/type.rkt")
(require "../messages-types.rkt")
(require typed/racket/async-channel)

(define output-process-channel : (Async-Channelof OutputCommandMessage) (make-async-channel))

(struct state-struct ([users : (Immutable-HashTable Integer user-struct)]
                      [auth : (Immutable-HashTable String (U String Null))] ; null - анонимный юзер
                      [user-auth : (Immutable-HashTable Integer String)]
                      [user-channel : (Immutable-HashTable Integer String)]
                      [channels : (Immutable-HashTable String channel-struct)]
                      [i-ports-user-ids : (Immutable-HashTable Input-Port Integer)])
  #:transparent)


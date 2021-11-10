#lang typed/racket

(provide
 make-new-channel
 channel-set-name)

(require/typed racket/random
               [crypto-random-bytes (Exact-Positive-Integer -> Bytes)])

(require "type.rkt")


(: make-new-channel (->* () (String) channel-struct))
(define (make-new-channel [name (~a (crypto-random-bytes 16))])
  (channel-struct name ""))

(: channel-set-name (-> channel-struct String channel-struct))
(define (channel-set-name channel name)
  (struct-copy channel-struct
               channel
               [name name]))




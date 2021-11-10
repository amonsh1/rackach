#lang typed/racket

(provide
 user-set-nick
 make-new-user)


(require "type.rkt")


(: make-new-user (->* (Integer (List Input-Port Output-Port))
                      (String)
                      user-struct))
(define (make-new-user id io-ports [nick "anonim"])
  (user-struct id io-ports nick))

(: user-set-nick (-> user-struct String user-struct))
(define (user-set-nick user nick)
  (struct-copy user-struct user [nick nick]))

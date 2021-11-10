#lang typed/racket

(provide
 (struct-out channel-struct))

(struct channel-struct ([name : String]
                        [password : String]) #:transparent)


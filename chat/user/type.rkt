#lang typed/racket

(provide
 (struct-out user-struct))


(struct user-struct ([id : Integer]
                     [io-ports : (List Input-Port Output-Port)]
                     [nick : String]))


#lang typed/racket

(provide
 
 (struct-out client-fail)
 (struct-out servet-error)
 (struct-out wrong-command-fail)
 (struct-out login-error))

(struct servet-error exn:fail())
(struct client-fail exn:fail())

(struct wrong-command-fail client-fail())
(struct login-error client-fail())

(match-define (list command arguments) (list 1 2))

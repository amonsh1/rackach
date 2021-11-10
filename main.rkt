#lang typed/racket


(module+ main

  (require racket/cmdline)
  (require "chat/main.rkt")
  
  (define current-port (make-parameter 8888))
  
  (command-line
    #:program "chat"
    #:once-each
    [("-p" "--port") port "Порт" (current-port (cast port Integer))]
    #:args ()
    (with-handlers ([exn:break? (lambda (x) (println 2))])
      (run (current-port)))
    )
  )

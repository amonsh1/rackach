#lang typed/racket


(provide
 MainLoopMessageNewClient
 MainLoopMessageClientMessage
 MainLoopMessage
 main-loop-message-new-client?
 main-loop-message-client-message?
 
 OutputCommandMessageAdd
 OutputCommandMessageRemove
 OutputCommandMessageSend
 output-command-message-add?
 output-command-message-remove?
 output-command-message-send?
 OutputCommandMessage)

#|Типы входных сообщений|#
(define-type MainLoopMessageNewClient (List Input-Port Output-Port))
(define-type MainLoopMessageClientMessage (List 'client-message Input-Port (U EOF String)))

(define-type MainLoopMessage (U MainLoopMessageNewClient MainLoopMessageClientMessage))

(define-predicate main-loop-message-new-client? MainLoopMessageNewClient)
(define-predicate main-loop-message-client-message? MainLoopMessageClientMessage)


#|Типы выходных сообщений|#
(define-type OutputCommandMessageAdd (List 'add (List Integer Output-Port)))
(define-type OutputCommandMessageRemove (List 'remove (List Integer)))
(define-type OutputCommandMessageSend (List 'send (List Integer String)))

(define-predicate output-command-message-add? OutputCommandMessageAdd)
(define-predicate output-command-message-remove? OutputCommandMessageRemove)
(define-predicate output-command-message-send? OutputCommandMessageSend)

(define-type OutputCommandMessage (U OutputCommandMessageAdd
                                     OutputCommandMessageRemove
                                     OutputCommandMessageSend))
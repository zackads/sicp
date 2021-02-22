(load "~cs61a/lib/im-client")

;; USAGE:
;; > (define e (instantiate echo-bot))
;; > (im-enroll-bot "128.32.42.27" 52822 e 'echo)
;

(define realname "")

(define (im-enroll-bot server-address port bot name)
  (set! realname whoiam)  ; so we can go back later
  (set! whoiam name)
  (im-enroll server-address port)
  (if port-from-server
      (begin (setup-bot port-from-server bot) 'okay)
      (begin (set! whoiam realname) #f) ))

(define-class (chat-bot)
  (method (receive-msg sender data) 'okay)   ; For subclassing.
  (method (client-list sender data)
    (update-client-list data) )
  (method (goodbye sender data)
    (close-connection)
    (set! whoiam realname) ))

(define-class (echo-bot)
  (parent (chat-bot))
  (method (receive-msg sender data)
    (im sender data) ))

; Like on Fa10 MT2, though it doesn't know its own name.
(define-class (grumpy-bot tolerance)
  (parent (chat-bot))
  (method (receive-msg sender data)
    (set! tolerance (- tolerance 1))
    (if (>= tolerance 0)
	(im sender '(dont talk to me))
	'message-ignored) ))

;; This is the core of im-bots.scm.
;; It changes the socket's input callback to work with the bot.
;; If you understand this and the original SETUP-REQUEST-HANDLER,
;; you probably get client-server programming.
(define (setup-bot port-from-server bot)
  ;;;Handle messages from the server by turning them into OO messages.
  (define (request-handler)
    (let ((req (get-request port-from-server)))
      (if (not req)
	  (close-connection)
	  (begin
	    (format logging "Received request: ~S~%" req)
	    ; This is the really important line!
	    (ask bot (request-action req) (request-src req) (request-data req))
	    (if (and (not (port-closed? port-from-server))
		     (char-ready? port-from-server))
		(request-handler))))))
  (when-port-readable port-from-server request-handler))

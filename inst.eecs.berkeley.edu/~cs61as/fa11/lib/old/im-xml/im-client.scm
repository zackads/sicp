;NOTES:
;Read NOTES in server.scm:
;   Notes that apply to both server.scm and this file will be kept in 
;   server.scm.


;All logging messages are followed by the comment LOGGING

(load "~cs61a/lib/im-common.scm")


(define to-server '())  	; Socket to server
(define to-server-write '())  	; write port to server
(define to-server-read '())  	; Read port from server
(define clients '())  		; List of known clients
(define whoiam "")  		; Your name


(define (im-enroll server-address port)
  ;;;Start handshake with server.
  ;
  ;Set! global variables.
  ;Send "hello" message.
  ;Check for "welcome" response.
  ;Set! clients variable.
  ;Send "thanks" command.
  ;Set thunk for socket.
  ;
  (set! to-server (make-client-socket server-address port))
  (set! to-server-write (socket-output to-server))
  (set! to-server-read (socket-input to-server))
  (set! whoiam (getenv "USER"))
  (let ((msg ""))
    (display (format #f "Sending 'hello' command to server.~%"))  ;LOGGING
    (if (not (send-msg whoiam "server" "hello" "" to-server-write))
	(close-connection)
	(begin
	 (display (format #f "Reading response...~%"))  ;LOGGING
	 (set! msg (get-msg to-server-read))
	 (if (not msg)
	     (begin
	      (display (format #f "Error in reading socket.~%"))  ;LOGGING
	      (close-connection))
	     (begin
	      (display (format #f "Response received: ~A~%" msg))  ; LOGGING
	      (cond ((string=? "sorry" (xml-cdata "command" msg))
		     (display (format #f "Another client using same login!~%"))
		     (close-connection))
		    ((not (and (string=? "welcome" (xml-cdata "command" msg))
			       (string=? whoiam (xml-cdata "to" msg))))
		     (close-connection))
		    (else
		     (display (format #f "Received 'welcome' message.~%"))  ;LOGGING
		     (display (format #f "Sending 'thanks' command.~%"))  ;LOGGING
		     (send-msg whoiam "server" "thanks" "" to-server-write)
		     (when-port-readable to-server-read
					 (lambda () (handle-msg))))))))))
  (display (format #f "(im-enroll) done.~%~%"))  ;LOGGING
)

(define (handle-msg)
  ;;;Handle messages from the server.
  ;
  ;Only handles "receive-msg", "client-list", and "goodbye".
  ;
  (let ((msg (get-msg to-server-read)))
    (if (not msg)
	(close-connection)
	(begin
	 (cond
	  ((string=? "receive-msg" (xml-cdata "command" msg))
	   (received-msg (xml-cdata "from" msg) (xml-cdata "data" msg)))
	  ((string=? "client-list" (xml-cdata "command" msg))
	   (update-client-list (split-string (xml-cdata "data" msg))))
	  ((string=? "goodbye" (xml-cdata "command" msg))
	   (close-connection)))
	 #t))))

(define (received-msg from-whom msg)
  ;;;Handles message received from other clients.
  ;Change to GUI in future.
  (display (format #f "~%Message from ~A:~%    ~A~%~%" from-whom msg)))

(define (im who message)
  ;;;Send message to who.
  (if (not (send-msg whoiam who "send-msg" message to-server-write))
      (close-connection)
      #t))

(define (update-client-list client-list)
  ;;;Deal with a new client list.
  (set! clients client-list)
  ;Change to GUI in future.
  (display (format #f "~%New client-list: ~A~%~%" client-list)))

(define (close-connection)
  ;;;Closes connection to the server.
  (display (format #f "Closing down socket and ports..."))  ;LOGGING
  (if (and to-server-read (not (port-closed? to-server-read)))
      (when-port-readable to-server-read #f))
  (set! to-server-read #f)
  (set! to-server-write #f)
  (if (and to-server (not (socket-down? to-server)))
      (socket-shutdown to-server #f))
  (set! to-server #f)
  (set! clients #f)
  (set! whoiam #f)
  ;Change to GUI in future.
  (display (format #f "Connection to server closed.~%~%")))

(define (im-exit)
  ;;;Log out client.
  (display (format #f "Letting server know that logging out.~%"))  ;LOGGING
  (if (and to-server to-server-write
	   (not (null? to-server)) (not (null? to-server-write)))
      (send-msg whoiam "server" "logout" "" to-server-write)
      (close-connection)
      ;When "goodbye" message is received, that should run (close-connection)
      ; and finish logout.
  ))

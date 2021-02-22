;DEAL WITH:
;   EMPTY

;CLEANUP:
;   EMPTY

;POSSIBLE MESSAGES
;=================
;hello -- Used to initialize three-way handshake; client->server.
;welcome -- Lets client know server got "hello"; server->client.
;thanks -- Tells server that client is done logging in; client->server.
;goodbye -- Tells client that its connection to the server is gone; server->client.
;send-msg -- Informs server to send this message to another client; client->server.
;receive-msg -- Message to be received by client; server->client.
;logout -- Informs the server that the client is logging off; client->server.
;client-list -- List of clients logged into the server; server->client.


(load "~cs61a/lib/im-common.scm")

;clients variable stores all known clients.
;It's a table of entries in the format ("client name" . client-socket)
(define clients (list '*table*))

;Data abstraction for above:
(define key car)
(define value cdr)

;server variable stores the server socket
(define server '())


(define (clients-add name sock)
  ;;;Add sock to the clients list bound to name
  ;
  ;Broadcasting the new client list is left up to other places in the code.
  ;This is done so as to function as the opposite to (clients-remove).
  ;
  (if (not (assoc name (cdr clients)))
      (begin (set-cdr! clients (cons (cons name sock) (cdr clients))) #t)
      #f))


(define (clients-remove name)
  ;;;Remove name from the clients list
  ;
  ;Broadcasting the new client list is left up to other places in the code.
  ;This is because if the server is shutting down the traffic created by 
  ;sending a new client list after every remove client could be a issue.
  ;
  (define (helper who table)
    ;Remove key-value pair from table, return its value.
    (cond ((null? (cdr table)) #f)
	  ((equal? who (key (cadr table)))
	   (let ((result (value (cadr table))))
	     (set-cdr! table (cddr table))
	     result))
	  (else (helper who (cdr table)))))
  (let ((to-close-socket (helper name clients)))
    (if (not (socket-down? to-close-socket))
	(if (socket-input to-close-socket)
	    (when-port-readable (socket-input to-close-socket) #f))
	(socket-shutdown to-close-socket #f))))


(define (clients-find name)
  ;;;Return the socket bound to name; if the name does not exist, return #f
  (let ((result (assoc name (cdr clients))))
    (if result
	(value result)
	#f)))


(define (clients-list)
  ;;;Return a list of known client names.
  (map key (cdr clients)))


(define (clients-string)
  ;;;Returns a string of space-separated names of known clients.
  ;If the list is empty, "nil" is returned.
  (if (null? (cdr clients))
      "nil"
      (apply string-append (map (lambda (name) (string-append " " name))
				(clients-list)))))


(define (im-server-start)
  ;;;Start the server.
  ;
  ;Set! server variable
  ;Set thunk for handling handshake with new client
  ;
  (display (format #f "~%Server starting...~%"))
  (set! server (make-server-socket))
  (display (format #f "Server IP address: ~A, server port: ~A~%" 
;; BROKEN:     	   (car (split-string (exec "hostname -i")))
		   (get-ip-address-as-string)
		   (socket-port-number server)))
  (when-socket-ready server
		     (lambda () 
		       (begin
			(display (format #f "New client connecting.~%"))
			(handshake (socket-dup server)))))
  (display (format #f "(im-server-start) done.~%~%")))


(define (im-server-close)
  ;;;Close  the server by no longer accepting connections,
  ;
  ;Remove thunk on server.
  ;Broadcast "goodbye" message.
  ;Close all client sockets.
  ;Close server socket.
  ;
  (display "Server shutting down...~%")
  (broadcast "goodbye" "")
  (for-each clients-remove (clients-list))
  (if (and server (not (socket-down? server)))
      (begin
       (when-socket-ready server #f)
       (socket-shutdown server #f)))
  (set! server #f)
  (set! clients (list '*table*))
  (display (format #f "(im-server-close) done.~%~%")))


(define (handshake sock)
  ;;;Handle the three-way handshake with a client.
  ;
  ;Handshaking should go as follows:
  ;client->server:
  ;"<command>hello</command><from>CLIENT</from><to>server</to><data></data>"
  ;server->client:
  ;"<command>welcome</command><from>server</from><to>CLIENT</to><data></data>"
  ;client->server:
  ;"<command>thanks</command><from>CLIENT</from><to>server</to><data></data>"
  ;
  ;Accept the socket connection.
  ;Check message is "hello".
  ;Send "welcome" message back.
  ;Check response message is "thanks".
  ;Call (register-client)
  ;
  (socket-accept-connection sock)
  (display (format #f "Connection accepted for ~A...~%" sock))
  (let* ((read-port (socket-input sock))
	 (write-port (socket-output sock))
	 (msg (get-msg read-port)))
    (if (not msg)
	(socket-shutdown sock #f)
	(begin
	 (cond ((not (string=? "hello" (xml-cdata "command" msg)))
		;Need to make sure that won't intercept messages from
		;other clients trying to connect.
		(socket-shutdown sock #f))
	       ((assoc (xml-cdata "from" msg) (cdr clients))
		(display (format #f "Name ~A already exists.~%"
				    (xml-cdata "from" msg)))
		(send-msg "server"
			  (xml-cdata "from" msg)
			  "sorry" "" write-port)
		(socket-shutdown sock #f))
	       (else
		(display (format #f "Sending welcome message.~%"))
		(if (not (send-msg "server"
				   (xml-cdata "from" msg)
				   "welcome" "" write-port))
		    (socket-shutdown sock #f)
		    (begin
		     (set! msg (get-msg read-port))
		     (if (not msg)
			 (socket-shutdown sock #f)
			 (begin
			  ;If response from welcome is thanks, handshake is done.
			  ;Can add client as a known client.
			  (if (not (string=? "thanks" (xml-cdata "command" msg)))
			      (socket-shutdown sock #f)
			      (begin
			       (display (format #f "~A has logged on.~%"
						(xml-cdata "from" msg)))
			       (register-client (xml-cdata "from" msg)
						sock)))))))))))))


(define (register-client name sock)
  ;;;Store socket to client and start handling of the client socket.
  (display (format #f "~A (~A) is being registered...~%" name sock))
  (if (clients-add name sock)
      (begin
       (display (format #f "clients: ~A.~%" clients))
       (handle-client name sock)
       (broadcast "client-list" (clients-string))
       (display (format #f "~A is now registered.~%~%" name)))))


(define (handle-client name sock)
  ;;;Handle messages from the client.
  ;
  ;Only handles "send-msg" and "logout" messages.
  ;
  (let ((read-port (socket-input sock))
	(write-port (socket-output sock)))
    (when-port-readable read-port
			(lambda ()
			  (let ((msg (get-msg read-port)))
			    (if (not msg)
				(remove-client name)
				(begin
				 (display (format #f "Received message: ~A~%" msg))
				 (cond  ;Unrecognized commands fall through the (cond).
				  ((string=? "send-msg" (xml-cdata "command" msg))
				   (send-command (xml-cdata "from" msg) 
						 (xml-cdata "to" msg) "receive-msg" 
						 (xml-cdata "data" msg))
				   (if (char-ready? read-port)
				       ((when-port-readable read-port))))
				  ((string=? "logout" (xml-cdata "command" msg))
				   (remove-client (xml-cdata "from" msg)))))))))))


(define (send-command from to cmd data)
  ;;;Sends msg to someone from someone.
  ;
  ;Make sure socket still good.
  ;Send message.
  ;
  (let ((to-sock (clients-find to)))
    (display (format #f "Sending message to ~A (~A)...~%" to to-sock))
    (if to-sock
	(let ((write-port (socket-output to-sock)))
	  (display (format #f "Sending the command ~A to ~A from ~A.~%" cmd to from)
		   ) ;logging
	  (if (not write-port)
	      (remove-client to)
	      (if (not (send-msg from to cmd data write-port))
		  (remove-client to)))
	  (display (format #f "message sent.~%~%"))))))


(define (remove-client who)
  ;;;Remove client from living clients and send out a new  list of clients.
  (display (format #f "Removing ~A as a client.~%" who))
  (clients-remove who)
  (broadcast "client-list" (clients-string))
  (display (format #f "~A removed as a client.~%~%" who)))


(define (broadcast cmd data)
  ;;;Send COMMAND to all clients containing DATA.
  (display (format #f "Broadcasting the command ~A with data ~A to all clients.~%" 
		   cmd data))
  (for-each (lambda (name) (send-command server name cmd data))
	    (clients-list))
  (display (format #f "Broadcast done.~%~%")))


;; horrible kluge by benrg; please replace with a portable solution
(define (get-ip-address-as-string-old)
  (car (split-string (exec "perl -e '$_ = `hostname`; $_ = `host $_`; if (/(\\d+\\.){3}\\d+/) { print $& } else { print q(UNKNOWN) }"))))

;; alternate version, more portable but inefficient and relies on
;; www-inst.cs.berkeley.edu being up
(define (get-ip-address-as-string-new)
  (let* ((s (make-client-socket "www-inst.cs.berkeley.edu" 80))
         (a (socket-local-address s)))
    (socket-shutdown s)
    a))

(define get-ip-address-as-string get-ip-address-as-string-new)

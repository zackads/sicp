;;;Common code between the client and server

(define (send-msg from to cmd data write-port)
  ;;;Send a formatted message to write-port
  (if (and (not (null? write-port)) (not (port-closed? write-port)))
      (begin
       (format write-port
	       "<command>~A</command><from>~A</from><to>~A</to><data>~A</data>\n" 
	       cmd from to data)
       (flush write-port)
       #t)
      #f))

(define (get-msg read-port)
  ;;;Read port to get socket output.
  ;
  ;Returns #f if there was an issue reading from the port.  Otherwise returns
  ;the string read from the port.
  ;
  (if (and (not (null? read-port)) (not (port-closed? read-port)))
      (let ((raw-msg (read-line read-port)))
	(if (eof-object? raw-msg)
	    #f
	    (list->real_string raw-msg)))
      #f))

(define (list->real_string lst . spacer)
  ;;;Convert a list of strings into a single string with an optional spacer
  (if (null? spacer) (set! spacer " "))
  (if (null? (cdr lst))
      (format #f "~A~%" (car lst))
      (format #f "~A~A~A" (car lst) spacer (list->real_string (cdr lst)))))

(define (xml-cdata tag data)
  ;;;Find and return CDATA of a tag.
  ;Cannot handle the nesting of tags of the same name.
  (let* ((opening-tag (string-append "<" tag ">"))
	 (opening-tag-length (string-length opening-tag))
	 (opening-tag-start (string-index opening-tag data)))
    (if opening-tag-start  ;Make sure that the opening tag even exists.
	(let ((opening-tag-end (+ opening-tag-start opening-tag-length))
	      (closing-tag-start
	       (string-index (string-append "</" tag ">") data)))
	  (if (and opening-tag-end closing-tag-start)
	      (substring data opening-tag-end closing-tag-start) 
	      #f))
	#f)))

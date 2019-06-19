(require 'ironclad)
(require 'sb-concurrency)
(require 'bordeaux-threads)
(require 'log4cl)

(load "./message.lisp")
(load "./mail.lisp")
(load "./ping-token.lisp")

(defparameter message-list nil)    
(defparameter message-list-file nil)
(defparameter message-id-list nil)
(defparameter id-counter 0)
(defparameter key nil)
(defparameter host nil)

(defparameter env-format '(key host))
(defparameter env-keys '("DMS_KEY=" "HOST="))

(defparameter timer-queue (SB-concurrency:make-queue :name "timers"))
(defparameter next-timer nil)

(defparameter seconds-in-day 86400)
(defparameter retry-interval seconds-in-day)

(defparameter timer-thread nil)

(defun get-message (message-id)
  (loop for x in message-list
     do (if (eq (id x) message-id) (return x))))

(defun is-timer-queue-empty ()
  (SB-concurrency:queue-empty-p timer-queue))

(defun add-message-to-timer-queue (id)
  (SB-concurrency:enqueue (list "M" id) timer-queue))

(defun add-ping-to-timer-queue (id)
  (SB-concurrency:enqueue (list "P" id) timer-queue))

(defun timer-callback ()
  (let* ((id next-timer)
	 (msg (get-message id))
	 (random-suffix (random-string url-length)))
      (if (not (eq nil (sb-ext:list-all-timers)))
	  (setf next-timer (sb-ext:timer-name (first (sb-ext:list-all-timers))))
	  (setf next-timer nil))
    (if (eq msg nil) ; message not found
	(progn
	  (log:info "Message not found")
	  (return-from timer-callback nil)))
    (if (< (pings-sent msg) (retry-count msg))	
	(progn
	  (set-new-ping-token msg random-suffix)	  
	  (send-ping msg (make-random-url random-suffix)) ; ping w/ unique token
	  (setf (pings-sent msg) (incf (pings-sent msg))) ; increment pings
	  (setf (last-ping-time msg) (get-universal-time)) ; update last ping time
	  (add-ping-to-timer-queue id)) ; put timer in queue to be scheduled

	(progn
	  (send-message msg)
	  (setf (slot-value msg 'message-sent?) t)
	  ))))

(defun second-timer (seconds id)
  (sb-ext:schedule-timer (sb-ext:make-timer #'timer-callback :name id) seconds)
  (setf next-timer (sb-ext:timer-name (first (sb-ext:list-all-timers)))))  

(defun schedule-timers-in-queue ()
  (loop while t
     do
       (sleep 5)
       (if (not (is-timer-queue-empty))
	   (let* ((new-timer (SB-concurrency:dequeue timer-queue)))
	     (if (equal "P" (car new-timer))		 
		 (second-timer retry-interval (nth 1 new-timer)))
	     (if (equal "M" (car new-timer))		   
		 (second-timer (ping-interval (get-message (nth 1 new-timer))) (nth 1 new-timer)))))))

(defun make-timer-scheduler-thread ()
  (setq timer-thread (bt:make-thread #'schedule-timers-in-queue :name "timer-scheduler")))

(defun unschedule-message-timer (message-id)
  (loop for timer in (sb-ext:list-all-timers)
     do (if (equal (sb-ext:timer-name timer) message-id)
		   (sb-ext:unschedule-timer timer))))

(defun kill-timer-scheduler-thread ()
  (loop for id in message-id-list
       do (unschedule-message-timer id))
  (bt:destroy-thread timer-thread)
  (setq timer-thread nil))

(defun delete-message-from-list ()
  (let ((delete-id  (parse-integer (get-input "Delete message id: "))))
    (if (not (eq delete-id nil))
	(if (get-message delete-id)
	    (progn
	      (unschedule-message-timer delete-id)
	      (setf message-list (remove (get-message delete-id) message-list))
	      (format t "Message ~d removed~%" delete-id)
	      t)
	    (progn
	      (format t  "Message not found~%")
	      nil)))))

(defun new-message-to-list ()
  (push (new-message-ui-prompts id-counter) message-list)
  (second-timer (ping-interval (car message-list)) id-counter)
  (push id-counter message-id-list)
  (incf id-counter))

(defun print-message-list ()
  (loop for x in message-list
     do (print x))
  (terpri))

(defun ping-received (message-id)
  (let ((msg (get-message message-id)))
    (unschedule-message-timer message-id)
    (setf (slot-value msg 'pings-sent) 0)
    (setf (slot-value msg 'ping-token) nil)
    (add-message-to-timer-queue message-id)))    

(defun change-retry-interval ()
  (format t "Current retry interval is ~d seconds~%" retry-interval)
  (let ((interval (get-input "New interval (seconds): ")))
    (handler-case (setq retry-interval (parse-integer interval))
		 (error ()
			(format t "Bad input. Only intergers.")))))

(defun trim-env-key (env-key-string val)
  (string-trim env-key-string  val))

(defun adjust-key (k)
  (if (> (length k) 32)
      (setq k (subseq k 0 32)))
  (let ((s (make-array (length k)
		       :fill-pointer (length k)
		       :adjustable t
		       :initial-contents k
		       :element-type 'CHARACTER)))
    (if (< (length s) 32)
	(loop repeat (- 32 (length s))
	   do (vector-push-extend (char s (- (length s) 1)) s)))
    (return-from adjust-key s)))

(defun load-env ()
  (let ((env (uiop/stream:read-file-lines ".env")))
    (map nil #'(lambda (f v) (set f v)) ENV-format (map 'LIST #'TRIM-ENV-KEY env-keys env)))
  (setq key (adjust-key key)))

(defun get-cipher (key)
  (ironclad:make-cipher :twofish
    :mode :ecb
    :key (ironclad:ascii-string-to-byte-array key)))

(defun encrypt (plaintext key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:ascii-string-to-byte-array plaintext)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))

(defun decrypt (ciphertext-int key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:integer-to-octets ciphertext-int)))
    (ironclad:decrypt-in-place cipher msg)
    (coerce (mapcar #'code-char (coerce msg 'list)) 'string)))

(defun save-message-list()
  (with-open-file (message-list-file "messages.dat" :if-exists :append :direction :output :if-does-not-exist :create)
    (loop for msg in message-list
	 do (write-line (format nil "~d~%" (encrypt (message-to-string msg) key)) message-list-file)))
  (format t "Message list saved~%"))

(defun read-in-message (str)
  (let ((msg (eval (READ-FROM-STRING (concatenate 'string "(make-instance 'message " str ")" )))))
    (if (equal nil msg)
	(progn
	  (format t "Error reading message")
	  (return-from read-in-message nil)))
    (if (equal nil (find (id msg) message-id-list))
	(progn (push msg message-list)
	       (push (id msg) message-id-list))
	(format t "Message ~D previously loaded~%" (id msg)))))

(defun load-message-list()
  (with-open-file (message-list-file "messages.dat" :if-exists :append :direction :input :if-does-not-exist :error)
    (do ((line (read-line message-list-file nil)
	       (read-line message-list-file nil)))
	((null line))
      ;(print line) 
      (if (and (not (string= "" line)) (not (eq line nil))) (read-in-message (decrypt (parse-integer line) key)))))
  (if (not (equal message-list nil))
      (loop for msg in message-list
	 do (if (> (id msg) id-counter)
		(setq id-counter (id msg)))))
  (format t "Message list loaded~%~%"))

(defun start-loaded-message-timers ()
  (loop for msg in message-list
     do (let ((timer-found nil))
	  ; check if timer is running
	  (loop for timer in (sb-ext:list-all-timers)
	     do (if (equal (sb-ext:timer-name timer) (id msg))
		    (setq timer-found t)))

	  ; timer not running
	  (if (and (equal nil (message-sent? msg)) (eq timer-found nil))
	      (if (eq (pings-sent msg) 0) ; if no pings sent
		  (add-message-to-timer-queue (id msg)) ; use message interval
		  (add-ping-to-timer-queue (id msg))))))) ; else use ping interval
		

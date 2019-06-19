(load "./ping-token.lisp")

(defclass message ()
  ((id :accessor id :initarg :id)
   (dm-email :accessor dm-email :initarg :dm-email)
   (recipient-email :accessor recipient-email :initarg :recipient-email)
   (message-text :accessor message-text :initarg :message-text)   
   (ping-interval :accessor ping-interval :initarg :ping-interval)
   (retry-count :accessor retry-count :initarg :retry-count)
   (pings-sent :accessor pings-sent :initarg :pings-sent)
   (last-ping-time :accessor last-ping-time  :initarg :last-ping-time)
   (ping-token :accessor ping-token :initarg :ping-token)
   (message-sent? :accessor message-sent? :initarg :message-sent?) 
   (date-created :accessor date-created :initarg :date-created)))

;; output functions

(defmethod print-object ((obj message) out)
  (with-slots (id dm-email recipient-email message-text ping-interval retry-count pings-sent last-ping-time ping-token message-sent? date-created) obj
    (print-unreadable-object (obj out :type t)
      (format out ":id ~d~%" id)
      (format out ":dm-email ~S~%" dm-email)
      (format out ":recipient-email ~S~%" recipient-email)
      (format out ":message-text ~S~%" message-text)
      (format out ":ping-interval ~d~%" ping-interval)
      (format out ":retry-count ~d~%" retry-count)
      (format out ":pings-sent ~d~%" pings-sent)      
      (format out ":last-ping-time ~d~%" last-ping-time)
      (format out ":ping-token ~S~%" ping-token)
      (format out ":message-sent? ~S~%" message-sent?)
      (format out ":date-created ~d~%" date-created))))

(defmethod message-to-string (obj)
  (concatenate 'string
      (format nil ":id ~d~%" (id obj))
      (format nil ":dm-email ~S~%" (dm-email obj))
      (format nil ":recipient-email ~S~%" (recipient-email obj))
      (format nil ":message-text ~S~%" (message-text obj))
      (format nil ":ping-interval ~d~%" (ping-interval obj))
      (format nil ":retry-count ~d~%" (retry-count obj))
      (format nil ":pings-sent ~d~%" (pings-sent obj))
      (format nil ":last-ping-time ~d~%" (last-ping-time obj))
      (format nil ":ping-token ~d~%" (ping-token obj))
      (format nil ":message-sent? ~S~%" (message-sent? obj))
      (format nil ":date-created ~d~%" (date-created obj))))

;; ui input functions

(defun get-input (&optional (message ""))
  (clear-input)           ; get rid of pending input
  (write-string message)  ;
  (finish-output)         ; make sure output gets visible
  (read-line))            ; read a line as a string

(defun dm-email-prompt () 
    (get-input "Enter your email that we'll send the periodic dead man check: "))

(defun recipient-email-prompt () 
    (get-input "Recipient email of message on dead man switch activation: "))

(defun message-text-prompt()
    (get-input "Message: "))

(defun get-ping-unit()  
  (format t "Email ping interval (Days[1] Weeks[2] Months[3]): ")
  (parse-integer (get-input)))

(defun get-retry-count()  
  (format t "How many retries (1 per day) before your message is sent? ")
  (parse-integer (get-input)))

(defun check-unit (unit)
  (ignore-errors (parse-integer unit)))
  

(defun ping-unit-to-string (unit)
  (case unit (1 "days")
	(2 "weeks")
	(3 "months")
	(9 "minutes")))

(defun get-ping-length (unit)
  (format t  "How many ~a? " unit)
  (parse-integer (get-input)))

(defun get-ping-frequency ()
  (get-ping-length (ping-unit-to-string (get-ping-unit))))

(defun get-ping-frequency-in-seconds ()
  (let* ((unit (get-ping-unit))
	(n-of-unit (get-ping-length (ping-unit-to-string unit))))
	(case unit
	  (1 (* n-of-unit 86400))
	  (2 (* n-of-unit 604800))
	  (9 (* n-of-unit 60)))))

(defun set-new-ping-token (msg token)
  ; remove old hash if present
  (if (not (eq (gethash (ping-token msg) url-id-hash-table) nil))
      (remhash (ping-token msg) url-id-hash-table))

  (setf (slot-value msg 'ping-token) token)
  (setf (gethash token url-id-hash-table) (id msg)))

;; message constructors

(defun new-message-ui-prompts(id)
    (let ((message (make-instance 'message
                        :id id
                        :dm-email (dm-email-prompt)
			:recipient-email (recipient-email-prompt)
                        :message-text (message-text-prompt)
			:ping-interval (get-ping-frequency-in-seconds)
			:retry-count (get-retry-count)
			:pings-sent 0
			:last-ping-time (get-universal-time)
			:ping-token nil
			:message-sent? nil
			:date-created (get-universal-time))))
      (terpri)
      (write-string "New message:")
      (print message)
      (terpri)
      (return-from new-message-ui-prompts message)))

(defun new-message (id dm-email recipient-email message-text
			   ping-interval retry-count pings-sent
			   date-created last-ping-time ping-token message-sent?)
    (make-instance 'message
                        :id id
                        :dm-email dm-email
			:recipient-email recipient-email
                        :message-text message-text
			:ping-interval ping-interval
			:retry-count retry-count
			:pings-sent pings-sent
			:last-ping-time last-ping-time
			:ping-token ping-token
			:message-sent? message-sent?
			:date-created date-created
			))

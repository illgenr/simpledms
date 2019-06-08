(defclass message ()
  ((id :accessor id :initarg :id)
   (email :accessor email :initarg :email)
   (message-text :accessor message-text :initarg :message-text)   
   (reminder-interval :accessor reminder-interval :initarg :reminder-interval)))

(defun get-input (&optional (message ""))
  (clear-input)           ; get rid of pending input
  (write-string message)  ;
  (finish-output)         ; make sure output gets visible
  (read-line))            ; read a line as a string

(defun email-prompt () 
    (get-input "Enter email address: "))

(defun message-text-prompt()
    (get-input "Message: "))

(defun create-new-message(id)
    (make-instance 'message
                        :id id
                        :email (email-prompt)
                        :message-text (message-text-prompt)
			:reminder-interval (get-reminder-frequency-in-seconds)
			))

(defun get-reminder-unit()  
  (format t "Days - 1~@
             Weeks - 2~@
             Months- 3~@
             Which interval (1, 2, 3)? ")
  (parse-integer (get-input)))

(defun check-unit (unit)
  (ignore-errors (parse-integer unit)))
  

(defun reminder-unit-to-string (unit)
  (case unit (1 "days")
	(2 "weeks")
	(3 "months")))

(defun get-reminder-length (unit)
  (format t  "How many ~a? " unit)
  (parse-integer (get-input)))

(defun get-reminder-frequency ()
  (get-reminder-length (reminder-unit-to-string (get-reminder-unit))))

(defun get-reminder-frequency-in-seconds ()
  (let* ((unit (get-reminder-unit))
	(n-of-unit (get-reminder-length (reminder-unit-to-string unit))))
	(case unit
	  (1 (* n-of-unit 86400))
	  (2 (* n-of-unit 604800)))))

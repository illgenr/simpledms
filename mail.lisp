(require 'uiop)

(load "./message.lisp")

(defvar to-header "to:")
(defvar subject-header "subject:")
(defvar from-header "from:simpledms")

(defparameter ping-subject "Dead Man Switch Ping")
(defparameter ping-message "Hello! Please click the link to confirm you're alive: ")

(defparameter activation-subject "Dead Man Switch Activation")
(defparameter activation-message "You've been set as a recipient of the following message in the event our attempts to contact the sender have failed. Hopefully the sender has previously been in contact with you about the contents. We unfortunately have no further information to provide.")


(defun make-message-stream (subject text)
  (make-string-input-stream (format nil "~a~%~a~a~%~%~a~%" from-header subject-header subject text)))

(defun sendmail (to subject text)
    ; (let ((text-stream (make-string-input-stream text)))
    (log:info "~%to: ~a~%subject: ~a~%text: ~a" to subject text)
    (uiop:launch-program (list "sendmail" "-v" to "-f" from-header)
			 :input (make-message-stream subject text)))

(defun send-ping (msg url)
  (sendmail (dm-email msg) ping-subject (concatenate 'string ping-message url)))

(defun send-message (msg)
  (sendmail (recipient-email msg) activation-subject (format nil "~A~%~A" activation-message (message-text msg))))


(defun test-make-message-stream ()
    (let ((subject "dms test")
	  (text "aljdfladkjeiqorncxvkhlqe3223421")
	  )
      (make-message-stream subject text)))

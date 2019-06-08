;(ql:quickload "iterate")

(load "./message.lisp")
(load "./timers.lisp")

(defparameter message-list nil)    
(defparameter id-counter 0)

(defun new-message ()
  (push (create-new-message id-counter) message-list)
  (second-timer (reminder-interval (car message-list)))
  (incf id-counter)
)

(defvar command-list '(("m" NEW-MESSAGE)))

;(defvar *current-command* nil)

(defun ui-top ()
  (let ((current-command ""))
  (loop while (not (string= current-command "q")) DO
       (write-string "next command?: ")
       (finish-output)
       (setq  current-command (read-line))

       (loop for item being the elements of command-list do
	    (if (string= (car item) current-command)
			 (funcall (car (cdr item))))))))

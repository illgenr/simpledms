(load "./message-list.lisp")
(load "./http-server.lisp")

(defparameter welcome-message "Simple Dead Man's Switch~%")
(defparameter help-string "Commands
q - Quit
m - Create a new message
d - Delete a message
p - Print the message list
s - Save current message list
l - Load message list from file (automatic on startup)
r - Retry interval on ping not acknowledged~%~%") 
(defparameter command-list '(("m" new-message-to-list)
			     ("d" delete-message-from-list)
			     ("p" print-message-list)
			     ("s" save-message-list)
			     ("l" load-message-list)
			     ("r" change-retry-interval)))

(defun ui ()
  (format t welcome-message)
  (format t help-string)
  (load-env)
  (if (eq key nil) (write-string ".env not found"))
  (load-message-list)
  (start-loaded-message-timers)

  (setq url-id-hash-table (make-hash-table :test 'equal))
  (make-timer-scheduler-thread)
  (create-http-server host)
  (start-server)
  
  (let ((current-command ""))
    (loop while (not (string= current-command "q"))
       DO
	 (write-string "next command?: ")
	 (finish-output)

	 (setq  current-command (read-line))

	 (loop for item being the elements of command-list do
	    (if (string= (car item) current-command)
		(funcall (car (cdr item)))))))

  (stop-server)
  (kill-timer-scheduler-thread))

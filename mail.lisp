(require 'uiop)

(defvar to-header "to:")
(defvar subject-header "subject:")
(defvar from-header "from:simpledms")


(defun sendmail (to subject text)
  (let ((text-stream (make-string-input-stream text)))
    (uiop:launch-program (list "sendmail" "-v" to)
			 :input (make-message-stream subject text))))


(defun make-message-stream (subject text)
  (make-string-input-stream (format nil "~a~%~a~a~%~%~a~%" from-header subject-header subject text)))


(defun test-make-message-stream ()
    (let ((subject "dms test")
	  (text "aljdfladkjeiqorncxvkhlqe3223421")
	  )
      (make-message-stream subject text)))

(require 'hunchentoot)

(load "./ping-token.lisp")

(defparameter server-thread nil)
(defparameter server-thread-name "hunchentoot-server")
(defparameter server nil)

(defun setup-token-handler ()
  (hunchentoot:define-easy-handler (live :uri "/") ((token :parameter-type 'string))
  (setf (hunchentoot:content-type*) "text/plain")  
  (if (not (eq (gethash token URL-ID-HASH-TABLE) nil))
      (progn
	(ping-received (gethash token URL-ID-HASH-TABLE))
	(remhash token URL-ID-HASH-TABLE)
        ;(format nil "Hey~@[ ~A~]!" (gethash token URL-ID-HASH-TABLE)))
	(format nil "Glad you're alive!~%The switch timer has been reset."))
      (format nil "Ping token not found!"))
  ))

(defun create-http-server (host)
  (setq server (make-instance 'hunchentoot:easy-acceptor :port 4242 :address host :name "dms-http-listener")))

(defun start-server ()
  (hunchentoot:start server)
  (setup-token-handler))

(defun stop-server()
  (hunchentoot:stop server))

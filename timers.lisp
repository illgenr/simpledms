;(ql:quickload "trivial-timer")
;(in-package :trivial-timer)
;(use-package :trivial-timer)

(require 'trivial-timer)
(trivial-timer::initialize-timer)

(defun timer-callback (registered-time delay id)
  (log:info "It's been ~a ms since you set a reminder. Expected interval was ~a ms.~%
Send message id ~a"
    (- (get-internal-real-time) registered-time) delay id))

(defun second-timer (seconds id)
  (trivial-timer::register-timer-call (* seconds 1000) #'timer-callback id))

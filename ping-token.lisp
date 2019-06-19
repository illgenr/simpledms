;;; $ cat /dev/urandom | tr -cd [:alnum:] | fold -w 15 | head -n 5
;;; See CLHS especially alpha-char-p digit-char-p upper-case-p lower-case-p graphic-char-p alphanumericp
;;; when you need get radix 16 strings, you can use (lambda(c)(digit-char-p c 16))

(defparameter url nil)
(defparameter http-prefix "http://")
(defparameter host "localhost")
(defparameter port ":4242")
(defparameter http-suffix nil)
(defparameter query "/?token=")

(defparameter url-id-hash-table nil)
(defparameter url-length 64)

(defun ensure (byte-input-stream pred)
  (loop(let((c(code-char(abs(read-byte byte-input-stream)))))
         (when(funcall pred c)
           (return c)))))

(defun random-string(length &optional (pred #'alpha-char-p))
  (with-open-file(s "/dev/urandom" :element-type '(signed-byte 8))
    (loop :with string = (make-string length)
          :for index :upfrom 0 :below length
          :do (setf(schar string index)(ensure s pred))
          :finally (return string))))

(defun make-random-url (random)
  (concatenate 'string http-prefix host port query random http-suffix))

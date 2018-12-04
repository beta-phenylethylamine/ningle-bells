(in-package :cl-user)
(defpackage ningle-bells
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:export
   :*app*
   :*http-status-codes*
   :*handler*
   :with-request-params
   :set-route
   :with-route
   :string-response
   :html-response
   :json-response
   :get-param-value
   :start
   :stop))

(in-package :ningle-bells)

;; Left there so you can use them someday...
(defparameter *http-status-codes*
  '(:ok 200
    :not-found 404
    :server-error 500)
  "Useful HTTP status codes")

(defun get-param-value (param-list name)
  (declare (type string name) (type list param-list))
  "Obtain the value of a request parameter called 'name' (string)"
  (let ((cons-cell (assoc name param-list :test 'equal)))
     (if cons-cell
         (cdr cons-cell)
         ;; else
         (error (format nil "Request parameter not found: ~a" name)))))

(defmacro with-request-params (params-variable param-list &body body)
  (declare (type list param-list) (type symbol params-variable))
  "Binds symbols to values corresponding to the request's param values,
then executes the body.

Params-variable is the variable that has the params.
Param-list should be list of (symbol param-name-as-string).
"
  `(symbol-macrolet
       ,(mapcar (lambda (entry)
                  (let ((var-name 
                          (first entry)) ; name of var to create (symbol)
                        (param-name   ; name of param (string)
                          (second entry)))
                    (list var-name ; define symbol
                          ;; which will expand to:
                          `(get-param-value ,params-variable ,param-name)
                          )))
         param-list)
     ,@body))

;; Important Note: Clack expects that the return value from
;; the route function is:
;; (STATUS-CODE list body)
;; where body must be a vector of character, OR otherwise
;; it can be a list of strings. 
(defun string-response (text-string &key
                                      (status-code 200)
                                      (content-type "text/plain"))
  "Creates standard text (string) response for use in route handlers."
  `(,status-code
    (:content-type ,content-type)
    (,text-string)))  

;; HTML response
(defmacro html-response (html-string &rest args)
  "Creates standard HTML response from string, for use in route handlers."
  `(string-response ,html-string :content-type "text/html" ,@args))
                  
;; JSON response
(defmacro json-response (html-string &rest args)
  "Standard response for JSON"
  `(string-response ,html-string :content-type "application/json" ,@args))

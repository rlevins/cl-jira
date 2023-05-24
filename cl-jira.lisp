;;;; cl-jira.lisp

(in-package #:cl-jira)

;;; "cl-jira" goes here. Hacks and glory await!

(defvar *basic-auth* nil)
(defvar *jira-url* nil)




(define-condition other-return-code (error) 
  ((status-code   :initarg :status-code
		  :initform nil :reader status-code)
   (reason-phrase :initarg :reason-phrase
		  :initform nil :reader reason-phrase))
  (:report (lambda (condition stream)
	     (format stream
		     "Unknown Return Code~%    Status-code: ~a~%     Reason-Phrase: ~a~%"
		     (when (slot-boundp condition 'status-code)
		       (status-code condition))
		     (when (slot-boundp condition 'reason-phrase)
		       (reason-phrase condition))))))

(define-condition rate-limit-exceeded (other-return-code)
  ((rate :initarg :rate :reader rate))
  (:report (lambda (condition stream)
	     (format stream
		     "Rate Limit Exceeded~%    Rate: ~a~%     Status-code: ~a~%    Reason-phrase: ~a~%"
		     (status-code condition) (rate condition) (reason-phrase condition)))))


;;;;  Rate limiting versions



(defmacro rate-function-creator (name rate-limit rate-limit-seconds )
  "Defines a function, NAME-rate, which returns the rate
   at which the function has been called after the rate is 
   less than RATE-LIMIT in a given block of RATE-LIMIT-SECONDS"
  (let ((fn-name (alexandria:symbolicate name  "-RATE" )))
   `(let* ((rate 0)
	   (rate-lock (bt:make-lock "rate-lock"))
	   (time-last-call 0)
	   (start-time 0)
	   (initialize nil)
	   (rate-limit ,rate-limit)
	   (rate-limit-seconds ,rate-limit-seconds)
	   (counts (make-array rate-limit-seconds :initial-element 0)))

      (flet  ((current-rate (increment-p)
		"Calculates the current rate"
		(unless initialize
		  (setf start-time (get-universal-time))
		  (setf time-last-call start-time)
		  (setf initialize t))
		(let* ((time-current-call (get-universal-time))
		       (index (mod (- time-current-call start-time) rate-limit-seconds))
		       (last-index (mod (- time-last-call start-time) rate-limit-seconds))
		       (delta (- time-current-call time-last-call )))
		  (log:trace increment-p  index delta rate)
		  (log:trace start-time time-last-call time-current-call )

		  (cond
		    ;; If it's been more than 10 seconds since
		    ;;  the time-last-call, reset
		    ((>= (- time-current-call time-last-call)
			 rate-limit-seconds)
		     (log:trace "resetting counts start-time time-last-call")
		     (fill counts 0)
		     (when increment-p (incf (elt counts 0)))
		     (setf start-time (get-universal-time))
		     (setf time-last-call start-time))
		    ;;   0  1  2  3  4  5  6  7  8  9
		    ;;  10 11 12 13 14 15 16 17 18 19
		    ;;  start = 0
		    ;;  time last call = 8
		    ;;  time-current = 12
		    ;;  have to 0 out 9, 0, 1 and 2
		    ;;  and incr 2
		    (t
		     (dotimes (i (-  delta 1) )
		       (log:trace "zeroing stuff" i  (- delta 1)  )
		       (log:trace (mod (+ last-index i 1) rate-limit-seconds))
		       (setf (elt counts (mod (+ last-index i 1) rate-limit-seconds))
			     0)
		       (log:trace counts))
		     ;;  When the index moves forward, zero the count at index
		     (when (not (= 0 (- index last-index)))
		       (setf (elt counts index) 0))
		     (when increment-p (incf (elt counts index)))))
		  (setf rate (reduce #'+ counts)))))
    
	(defun ,fn-name  ()
	  "Returns rate after rate limit has not been exceeded"
	  (bt:acquire-lock rate-lock t)
    
	  (do ((current-rate (current-rate t)
			     (current-rate nil)))
	      ((< current-rate rate-limit))
	    (log:trace "Sleeping 1" )
	    (log:debug "hit rate limit ~a" current-rate)
	    (sleep 1))
      
	  (setf time-last-call (get-universal-time))
	  (bt:release-lock rate-lock)
	  (values rate counts))))))

(rate-function-creator jira 400 60)

(defmacro jira-http-request (&rest args)
    `(progn
       (multiple-value-bind (rate counts)
	   (jira-rate)
	 (log:trace rate counts)
	 (multiple-value-bind
	       (body-or-stream status-code headers uri
			       stream must-close reason-phrase)
	     (drakma:http-request ,@args)
	   (declare (ignore stream must-close))
	   (cond
	     ((= 429 status-code)
	      (log:error headers)
	      (log:warn "Rate Limit Exceeded" reason-phrase uri status-code )
	      (error 'rate-limit-exceeded :status-code status-code
		     :rate rate :reason-phrase reason-phrase))
	     ((= 200 status-code) body-or-stream)
	     (t (error 'other-return-code :status-code status-code
		       :reason-phrase reason-phrase)))))))

(defmacro get-jira (api params &optional (preserve-uri nil))
  `(with-input-from-string
       (in  (flexi-streams:octets-to-string
     (restart-case
	 (jira-http-request  
	  (concatenate 'string *jira-url* ,api)
	  :parameters ,params   :force-ssl t
	  :basic-authorization *basic-auth*
	  :preserve-uri ,preserve-uri)
       (skip ()
	 :report "Skip the request."
	(flexi-streams:string-to-octets ""))
       (retry-get ()
	 :report "Retry the request."
	 (jira-http-request  
	  (concatenate 'string *jira-url* ,api)
	  :parameters ,params   :force-ssl t
	  :basic-authorization *basic-auth*
	  :preserve-uri ,preserve-uri)))))
     (cl-json:decode-json in)))

(defmacro put-jira (api content &optional (preserve-uri nil))
  "Macro to use PUT method with http-request to jira"
  `(with-input-from-string
       (in (flexi-streams:octets-to-string
	    (drakma:http-request  
	     (concatenate 'string *jira-url* ,api)
	     :method :put
	     :content-type "application/json"
	     :content ,content   :force-ssl t
	     :external-format-out :utf-8
	     :external-format-in :utf-8
	     :basic-authorization *basic-auth*
	     :preserve-uri ,preserve-uri )))
     (cl-json:decode-json in)))

(defun get-jira-by-key (jira-key)
  "Retrieves jira by key"
  (log:debug jira-key)
  (let ((issue (car
		(cdr
		 (assoc :issues 
			(get-jira-with-jql
			 (format nil "key = '~a'" jira-key)))))))
    (if (not (equalp jira-key (cdr (assoc :key issue))))
	(log:warn "Jira Issues has been renamed from ~a to ~a"
		   jira-key (cdr (assoc :key issue))))
    issue))


(defun get-jira-with-jql ( &optional
			       (jql "labels = \"#first-world-problems\"") (start-at 0)
			       (max-results 100))
  "Get's Jiras with given query in JQL"
  (get-jira  "/rest/api/latest/search?"
	     (list  (cons "jql"  jql)
		    (cons "startAt" (format nil "~a" start-at))
		    (cons "maxResults" (format nil "~a" max-results))
		    (cons "expand" "changelog")
		    ;;(cons "fields" "[\"resolutiondate\"]")
		    ;; (cons "expand" "[\"changelog\",\"transitions\"]")
		    )))


(defun get-all-jiras-with-jql (jql )
  "Returns all jiras with the given JQL"
  (log:debug jql)
  (do* ((num-results 500)
	(start 0 (+ start num-results))
	(response (get-jira-with-jql jql start num-results)
		  (get-jira-with-jql jql start num-results))
	(total (alexandria:assoc-value response :total))
	(jiras (alexandria:assoc-value response :issues)
	       (append (alexandria:assoc-value response :issues) 
		       jiras)))
       
       ((= total (length jiras)) jiras)
    (log:debug start (length jiras) total)))











;;;; cl-jira.asd

(asdf:defsystem #:cl-jira
  :description "Describe cl-jira here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:cl-json
               #:log4cl)
  :serial t
  :components ((:file "package")
               (:file "cl-jira")))


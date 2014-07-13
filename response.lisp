(in-package :stupid-chyat)

(defparameter *show-messages* 10
  "Show no more than this number of mesages")
(defvar *id* 0)

(defun get-time-string (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~d/~d"
            hour minute second date month year)))

(defun generate-html-response ()
  "Produce a HTML response "
  (setf (html-mode) :xml)
  (with-html-output-to-string (out nil :prologue t)
    (:html
     (:head
      (:meta :http-equiv "content-type"
             :content "text/html; charset=utf-8")
      #+nil (:meta :http-equiv "refresh"
                   :content "15"))
     (:body
      (:h1 "Welcome to Stupid Chyat!")
      (terpri out)
      (:h2 "Archives")
      "In development"
      (:h2 "Chat")
      (terpri out)
      (:form :action "postmsg" :method "post"
             "Your message"
             (:input :type "text" :name "msg")
             (:button :type "submit" "Post!"))
      (terpri out)
      (:table :border 0 :cellpadding 4
              (:tr
               (:th "ID")
               (:th "Message")
               (:th "Date")
               (:th  "UA"))
              (loop for msg in (db-get-recent *show-messages*) do
                   (htm
                    (:tr
                     (:td (str (db-entry-id msg)))
                     (:td (str (db-entry-message msg)))
                     (:td (str (get-time-string (db-entry-time msg))))
                     (:td (str (db-entry-ua msg)))))
                   (terpri out)))
      (terpri out)
      (:h2 "Features of my CL implementation:")
      (terpri out)
      (pprint-logical-block (out *features* :per-line-prefix "<br>")
        (pprint-fill out *features* nil))))))

(defun post-message (message user-agent)
  "Post a message. Does not return a response itself"
  (let ((msg (make-db-entry :id *id*
                            :message message
                            :time (get-universal-time)
                            :ua user-agent)))
    (db-output msg)
    (incf *id*)))

(export 'show-messages)

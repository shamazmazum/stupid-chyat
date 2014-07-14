(in-package :stupid-chyat)

(defvar *id* 0)

(eval-when (:load-toplevel :execute)
  (setf (html-mode) :sgml))

(defun get-time-string (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~d/~d"
            hour minute second date month year)))

(defun generate-go-away!-response ()
  "Produce a `go away!' HTML response for spammers"
  (with-html-output-to-string (out nil :prologue t)
    (:html
     (:head
      (:title "Stupid Chat")
      (:meta :http-equiv "content-type"
             :content "text/html; charset=utf-8")
      (:meta :http-equiv "refresh"
             :content "4;URL=/"))
     (:body
      (:h1 "Welcome to Stupid Chyat!")
      (:h2 "No, wait! You are no more welocmed here. Begone, spamer!")))
    (terpri out)))

(defun generate-html-response ()
  "Produce a HTML response "
  (with-html-output-to-string (out nil :prologue t)
    (:html
     (:head
      (:title "Stupid Chat")
      (:meta :http-equiv "content-type"
             :content "text/html; charset=utf-8")
      (:meta :http-equiv "refresh"
             :content "35")) ; FIXME (str *refresh-after*)
     (:body
      (:h1 "Welcome to Stupid Chyat!")
      (terpri out)
      (:h2 "Archives")
      "In development"
      (:h2 "Chat")
      (terpri out)
      (:form :action "postmsg" :method "post"
             "Your message" (:br)
             (terpri out)
             (:input :type "text" :name "msg") (:br)
             (terpri out)
             (let ((captcha-str (generate-captcha)))
               (if (and *captcha-enable* captcha-str)
                   (htm
                    "CAPTCHA (enter the first number and an answer separated by space, like `12 33'): "
                    (write-string captcha-str out)  (:br)
                    (terpri out)
                    (:input :type "text" :name "captcha") (:br)
                    (terpri out))))

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
        (pprint-fill out *features* nil))
      (terpri out)))))

(defun post-message (message user-agent)
  "Post a message. Does not return a response itself"
  (let ((msg (make-db-entry :id *id*
                            :message message
                            :time (get-universal-time)
                            :ua user-agent)))
    (db-output msg)
    (incf *id*)))

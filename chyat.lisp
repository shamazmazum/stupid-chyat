(in-package :stupid-chyat)

(defvar *acceptor*)

(define-easy-handler (root :uri "/") ()
  (generate-html-response))

(define-easy-handler (postmsg :uri "/postmsg") ((message :real-name "msg"))
  (let ((ua-stripped (strip-tags (user-agent)))
        (message-stripped (strip-tags message)))
    (cond
      ((and (string/= "" message-stripped)
            (message-allowed message-stripped))
       (post-message message-stripped ua-stripped)
       (generate-html-response))
      (t (generate-go-away!-response)))))

(defun start-chat ()
  "Start the chat"
  (flush-db)
  (reopen-db)
  (setq *acceptor* (make-instance 'easy-acceptor :ipv6 nil :port 8080 :address #(127 0 0 1)))
  (start *acceptor*))

(defun stop-chat ()
  "Stop the chat"
  (stop *acceptor*)
  (finalize-db))

(export '(start-chat stop-chat))

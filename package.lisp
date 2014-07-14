(defpackage stupid-chyat
  (:nicknames #:chyat)
  (:use #:cl #:hunchentoot #:cl-who)
  (:export #:*not-the-same-after-posts*
           #:*not-the-same-after-time*
           #:*captcha-enable*

           #:*db-directory*
           #:*start-over-after*
           #:*sync-after*
           #:*name-prefix*

           #:*show-messages*
           #:*refresh-after*

           #:start-chat
           #:stop-chat))

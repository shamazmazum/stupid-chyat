(in-package :stupid-chyat)

;; Related to CAPTCHA
(defparameter *not-the-same-after-posts* 30
  "The same messages are not considered the same after
   this number of posts")
(defparameter *not-the-same-after-time* 40
  "The same messages are not considered the same after
   this time (in seconds)")
(defparameter *captcha-enable* t
  "CAPTCHA is enabled if T")

;; Related to database
(defparameter *db-directory* "~/.stupid-chyat-db/"
  "Directory where database is held")
;; FIXME: must be one value for entire database
(defparameter *start-over-after* #.(* 15 10)
  "Start over with a new file after this number of messages.
   Must be multiple of *SYNC-AFTER*.")
(defparameter *sync-after* 15
  "Synchronize db after this number of messages")
(defparameter *name-prefix* "db")

;; Response generation
(defparameter *show-messages* 10
  "Show no more than this number of mesages")
(defparameter *refresh-after* 35
  "Refresh page after this number of seconds")

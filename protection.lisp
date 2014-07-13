(in-package :stupid-chyat)
(defparameter *not-the-same-after-posts* 30
  "The same messages are not considered the same after
   this number of posts")
(defparameter *not-the-same-after-time* 40
  "The same messages are not considered the same after
   this time (in seconds)")

(defun message-allowed (string)
  "Check if message is allowed (naïve spam protection)"
  (let ((the-same-message
         (find string (db-get-recent *not-the-same-after-posts*)
               :key #'db-entry-message
               :test #'string=)))
    (if the-same-message
        (> (- (get-universal-time)
              (db-entry-time the-same-message)) *not-the-same-after-time*)
        t)))

(defun strip-tags (string &optional (max-depth 100) (depth 0))
  "Strip what looks like HTML tags"
  (let ((open-tag-pos (position #\< string))
        (close-tag-pos (position #\> string)))
    (if (and open-tag-pos close-tag-pos
             (< depth max-depth))
        (strip-tags
         (concatenate 'string
                      (subseq string 0 open-tag-pos)
                      "/tags stripped/"
                      (subseq string (1+ close-tag-pos)))
         max-depth (1+ depth))
      string)))

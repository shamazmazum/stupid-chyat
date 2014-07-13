(in-package :stupid-chyat)

(defun message-allowed (string)
  "Check if message is allowed (spam protection)"
  (string/= "" string))

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

(in-package :stupid-chyat)

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

;; CAPTCHA
(defstruct captcha
  (id        0   :type integer)
  (answer    ""  :type string)
  (time      0   :type integer)
  (processed nil :type boolean))
(defvar *captcha-pool* nil
  "Pool of current (not processed and not expired yet)
   tests")
(defvar *captcha-providers* nil
  "CAPTCHA generators/checkers")
(defvar *captcha-lock*
  (bordeaux-threads:make-lock "captcha lock")
  "Lock protecting *CAPTCHA-POOL*")

(defun register-captcha (gen/check)
  "Register CAPTCHA generator/checker functions"
  (push gen/check *captcha-providers*))

(defun generate-captcha ()
  "Generate CAPTCHA for user and put it in pool"
  (if *captcha-providers*
      (bordeaux-threads:with-lock-held (*captcha-lock*)
        (let* ((last-captcha (car *captcha-pool*))
               (id (if last-captcha
                       (1+ (captcha-id last-captcha))
                       0))
               (gen/check-num (random (length *captcha-providers*))))
          (multiple-value-bind (question answer)
              (funcall (nth gen/check-num *captcha-providers*))
            (push (make-captcha :id id
                                :answer answer
                                :time (get-universal-time))
                  *captcha-pool*)
            (format nil "~d ~a" id question))))))

(defun check-captcha (answer-str)
  "Check CAPTCHA string"
  (if (not *captcha-enable*) (return-from check-captcha t))
  (multiple-value-bind (id rest)
      (parse-integer answer-str :junk-allowed t)
    (if id
        (bordeaux-threads:with-lock-held (*captcha-lock*)
          (let* ((answer (subseq answer-str (1+ rest)))
                 (captcha (find id *captcha-pool*
                                :key #'captcha-id)))
            (when (null captcha)
              (warn "Cannot find still unprocessed captcha (or it is a spam attack)")
              (return-from check-captcha nil))
            (if (and (not (captcha-processed captcha))
                     (string= (captcha-answer captcha) answer))
                (setf (captcha-processed captcha) t)))))))

(defun multiple-reals ()
  (let ((num1 (random 10))
        (num2 (random 10)))
    (values
     (format nil "~d*~d = " num1 num2)
     (format nil "~d" (* num1 num2)))))

(register-captcha #'multiple-reals)

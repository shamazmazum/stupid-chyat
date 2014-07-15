(in-package :stupid-chyat)

(defun message-allowed (string)
  "Check if message is allowed (naïve spam protection)"
  (let ((the-same-message
         (find string (db-get-recent *not-the-same-after-posts*)
               :key #'db-entry-message
               :test #'string-equal)))
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
  (id      0                                     :type integer)
  (time    0                                     :type integer)
  (checker #'(lambda (x) (declare (ignore x)) t) :type function)
  data)

(defvar *captcha-pool* nil
  "Pool of current (not processed and not expired yet)
   tests")
(defvar *captcha-providers* nil
  "CAPTCHA generators/checkers")
(defvar *captcha-lock*
  (bordeaux-threads:make-lock "captcha lock")
  "Lock protecting *CAPTCHA-POOL*")

(defun register-captcha (gen check)
  "Register CAPTCHA generator/checker functions"
  (push (cons gen check) *captcha-providers*))

(defun generate-captcha ()
  "Generate CAPTCHA for user and put it in pool"
  (if *captcha-providers*
      (bordeaux-threads:with-lock-held (*captcha-lock*)
        (if (> (length *captcha-pool*) 200)
            (flet ((captcha-expired (captcha)
                     (> (- (get-universal-time)
                           (captcha-time captcha))
                        *refresh-after*)))
              (setq *captcha-pool*
                    (delete-if #'captcha-expired *captcha-pool*))))

        (let* ((last-captcha (car *captcha-pool*))
               (id (if last-captcha
                       (1+ (captcha-id last-captcha))
                       0))
               (gen/check-num (random (length *captcha-providers*))))

          (multiple-value-bind (data question)
              (funcall (car (nth gen/check-num *captcha-providers*)))

            (push (make-captcha :id id
                                :data data
                                :checker (cdr (nth gen/check-num *captcha-providers*))
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
            (when captcha
              (setq *captcha-pool* (delete captcha *captcha-pool*))
              (funcall (captcha-checker captcha)
                       (captcha-data captcha)
                       answer)))))))

;; Various CAPTCHA generators/checkers

(defun multiply-reals-gen ()
  (let ((num1 (random 10))
        (num2 (random 10)))
    (values
     (* num1 num2)
     (format nil "~d*~d = " num1 num2))))

(defun multiply-reals-check (right-answer answer)
  (let ((answer (parse-integer answer :junk-allowed t)))
    (= right-answer answer)))

(defun pushkin-captcha-gen ()
  (let ((what? (random 3)))
    (values
     what?
     (concatenate 'string
                  "\"Я помню чудное мгновение...\" "
                  (case what?
                    (0 "Фамилия ")
                    (1 "ИМЯ ")
                    (2 "Отчество "))
                  "автора"))))

(defun pushkin-captcha-check (what? answer)
  (string=
   answer
   (case what?
     (0 "Пушкин")
     (1 "Александр")
     (2 "Сергеевич"))))

(eval-when (:load-toplevel)
  (register-captcha #'pushkin-captcha-gen #'pushkin-captcha-check)
  (register-captcha #'multiply-reals-gen #'multiply-reals-check))

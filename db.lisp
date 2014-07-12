(in-package :stupid-chyat)

(defparameter *db-directory* "~/.stupid-chyat-db/"
  "Directory where database is held")
;; FIXME: must be one value for entire database
(defparameter *start-over-after* #.(* 15 10)
  "Start over with a new file after this number of messages.
   Must be multiple of *SYNC-AFTER*.")
(defparameter *sync-after* 15
  "Synchronize db after this number of messages")
(defparameter *name-prefix* "db")

(defvar *current-stream*)
(defvar *messages* nil
  "Messages in memory")
(defvar *db-counter* 0
  "How many db files we have")

(export '(*db-directory*
          *start-over-after*
          *sync-after*
          *name-prefix*))

(defstruct db-entry
  (id   0   :type integer)
  (time 0   :type integer)
  author
  message)

(defun sync-db (&optional (number *sync-after*))
  "Sync DB with disk"
  (loop for msg in (butlast *messages*
                            (- (length *messages*)
                               number)) do
       (write msg :stream *current-stream*)
       (terpri *current-stream*))
  (force-output *current-stream*))

(defun prepare-db-output ()
  "Ensure that all streams is opened/counter set/etc.
   Must not be called directly"
  (when (or
         (= (length *messages*) *start-over-after*)
         (eq *messages* nil))
    (if (boundp '*current-stream*) (close *current-stream*))
    (setq *current-stream*
          (open (format nil "~A~A-~D"
                        *db-directory* *name-prefix*
                        *db-counter*)
                :direction :output
                :if-does-not-exist :create
                :if-exists :append)
          *messages* nil
          *db-counter* (1+ *db-counter*))))

(defun db-output (message)
  "Put a message into DB"
  (declare (type db-entry message))
  (prepare-db-output)
  (push message *messages*)
  (if (= (rem (length *messages*) *sync-after*) 0)
      (sync-db)))

(defun flush-db ()
  "Clear DB, deleting all messages"
  (setq *messages* nil
        *db-counter* 0))

(defun finalize-db ()
  "Sync db and close streams. If you want to work with db later,
   you will need to call reopen-db after this call"
  (sync-db (rem (length *messages*) *sync-after*))
  (close *current-stream*))

(defun reopen-db ()
  "Reads db from disk. Usually, in order to start work with db,
   you must call this function"
  (let* ((db-files (fad:list-directory *db-directory*))
         (last-db-file-num (1- (length db-files)))
         (last-db-file (car (last db-files))))
    (if last-db-file
        (setq *db-counter* last-db-file-num
              *messages*
              (with-open-file (input last-db-file
                                     :direction :input)
                (loop for obj = (read input nil)
                   while obj
                   collect obj))
              *current-stream* (open last-db-file
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :append)))))

(defun db-get-recent (n)
  "Get no more than n messages from db, but maybe less"
  (butlast *messages*
           (max 0
                (- (length *messages*) n))))

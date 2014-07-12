(defsystem :stupid-chyat
    :description "A stupid chat on CL"
    :maintainer "Vasily Postnicov <shamaz.mazum at gmail.com>"
    :version "0.0"
    :depends-on (:cl-fad :cl-who :hunchentoot)
    :components ((:file "package")
                 (:file "db" :depends-on ("package"))
                 (:file "response" :depends-on ("package"))
                 (:file "chyat" :depends-on ("package"))))

(in-package #:pyx.web)

(defvar *config* (u:dict))

(defvar *default-config*
  (u:dict :server :woo
          :port 8000
          :db.type :sqlite
          :db.name "database.db"
          :db.host nil
          :db.port nil
          :db.user nil
          :db.password nil
          :db.debug t
          :task.threads 8
          :task.interval 1
          :smtp.login "user@email.com"
          :smtp.password "secret"
          :smtp.ssl t
          :smtp.host "host.smtp.com"
          :smtp.from "no-reply@host.com"
          :auth.digest :sha512
          :auth.rounds 100000
          :auth.secret "secret"
          :path.log.access "access.log"
          :path.log.error "error.log"
          :path.static "static/"
          :css.local nil
          :css.remote nil
          :js.local nil
          :js.remote nil
          :site.title "Title"
          :site.author "Author"
          :site.description "Description"
          :site.url "http://www.example.com"
          :site.admin "user@email.com"
          :site.menu nil
          :site.year "2019"))

(defun read-config (file)
  (if (uiop:file-exists-p file)
      (u:hash-merge
       *default-config*
       (u:plist->hash
        (u:safe-read-file-form file)
        :test #'eq))
      *default-config*))

(defun load-config ()
  (let ((file (resolve-path "site.lisp")))
    (setf *config* (read-config file))))

(defun cfg (key)
  (u:href *config* key))

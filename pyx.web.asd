(asdf:defsystem #:pyx.web
  :description "Web-based documentation server for the Pyx game engine."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/pyx"
  :source-control (:git "https://github.com/mfiano/pyx.git")
  :bug-tracker "https://github.com/mfiano/pyx/issues"
  :encoding :utf-8
  :depends-on (
               #:alexandria
               #:caveman2
               #:cl-dbi
               #:cl-ppcre
               #:cl-slug
               #:cl-smtp
               #:clack
               #:datafly
               #:djula
               #:golden-utils
               #:ironclad
               #:lack-middleware-accesslog
               #:lack-middleware-csrf
               #:lack-response
               #:local-time
               #:lparallel
               #:sxql
               #:trivial-utf-8
               #:uiop)
  :pathname "web/src"
  :serial t
  :components
  ((:file "package")
   (:file "config")
   (:file "deploy")
   (:file "util")
   (:file "database")
   (:file "model")
   (:file "middleware")
   (:file "authentication")
   (:file "session")
   (:file "task")
   (:file "app")
   (:file "template")
   (:file "message")
   (:file "validate")
   (:file "routes")
   (:file "token/model")
   (:file "token/util")
   (:file "user/model")
   (:file "user/util")
   (:file "user/validate")
   (:file "user/messages")
   (:file "user/routes")))

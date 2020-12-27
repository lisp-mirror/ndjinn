(push :ndjinn.release *features*)
(push :cl-opengl-no-masked-traps *features*)
(push :cl-opengl-no-check-error *features*)

(asdf:defsystem #:ndjinn-deploy
  :description "A system used for deploying a game developed with Ndjinn."
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :source-control (:git "https://git.mfiano.net/mfiano/ndjinn")
  :bug-tracker "https://git.mfiano.net/mfiano/ndjinn/issues"
  :encoding :utf-8
  :depends-on (#:ndjinn)
  :pathname "src/deploy"
  :serial t
  :components
  ((:file "deploy")))

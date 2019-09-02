(in-package #:cl-user)

(defpackage #:pyx
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:log #:verbose)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:q #:origin.quat)
                    (#:m4 #:origin.mat4))
  (:use #:cl)
  (:export #:start
           #:stop))

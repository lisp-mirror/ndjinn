(in-package #:ndjinn)

(defun initialize-audio ()
  (sdl2-mixer:init :flac :ogg)
  (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
  (sdl2-mixer:allocate-channels 16))

(defun deinitialize-audio ()
  (sdl2-mixer:halt-channel -1)
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit))

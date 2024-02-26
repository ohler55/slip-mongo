
(require 'mongo "../slip-mongo")

(defvar mongo-suite (make-instance 'suite-flavor :name "mongo"))

(load "../slip-mongo/test/find-actor.lisp")
(load "../slip-mongo/test/insert-actor.lisp")
(load "../slip-mongo/test/update-actor.lisp")
(load "../slip-mongo/test/replace-actor.lisp")

(defun run-tests ()
  ;; (format t "*** murl: ~A~%" *murl*)
  (send mongo-suite :run)
  (send mongo-suite :report *standard-output*))

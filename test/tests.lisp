
(require 'mongo "../slip-mongo")

(defvar mongo-suite (make-instance 'suite-flavor :name "mongo"))

;; TBD replace with suite for each actor and a test for each variation of the actor :perform methods
(defvar sweet (make-instance 'suite-flavor :name "sweet" :parent mongo-suite))
(defvar toot (make-instance 'test-flavor :name "toot" :parent sweet :forms '((+ 1 (/ 2 3)))))

(defun run-tests ()
  ;; (format t "*** murl: ~A~%" *murl*)
  (send mongo-suite :run)
  (send mongo-suite :report *standard-output*))



(let* ((suite (make-instance 'suite-flavor :name "update-actor" :parent mongo-suite)))
  (make-instance 'test-flavor
                 :name "basic-perform"
                 :parent suite
                 :forms (lambda ()
                          (assert-nil nil "some description of the test in this line"))))

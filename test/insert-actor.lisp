
(let* ((suite (defsuite "insert-actor" mongo-suite)))
  (deftest "basic-perform" suite
    (lambda ()
      (assert-nil nil "some description of the test in this line"))))


(let* ((suite (defsuite "replace-actor" mongo-suite)))
  (deftest "basic-perform" suite
    (lambda ()
      (assert-nil nil "some description of the test in this line"))))

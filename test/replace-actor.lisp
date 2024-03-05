

(let* ((suite (defsuite "replace-actor" mongo-suite)))
  (deftest "static" suite
    (let ((col (send (send mc :database "play-replace") :collection "static"))
          (actor (make-instance 'mongo-replace-actor
                                :filter (make-bag "{a:1}")
                                :replacement (make-bag "{a:1,b:3}")
                                :destination "replaced"
                                :url *murl*
                                :database "play-replace"
                                :collection "static"
                                :timeout 3))
          (box (make-bag "{}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      (assert-equal "{replaced: 1}" (send (cadr response) :write nil))))
  (deftest "funcall" suite
    (let ((col (send (send mc :database "play-replace") :collection "funcall"))
          (actor (make-instance 'mongo-replace-actor
                                :filter (lambda (box) (make-bag "{a:1}"))
                                :replacement (lambda (box) (make-bag "{a:1 b:3}"))
                                :destination "replaced"
                                :url *murl*
                                :database "play-replace"
                                :collection "funcall"
                                :timeout 3))
          (box (make-bag "{}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      (assert-equal "{replaced: 1}" (send (cadr response) :write nil)))))

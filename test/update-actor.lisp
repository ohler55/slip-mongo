
(let* ((suite (defsuite "update-actor" mongo-suite)))
  (deftest "static" suite
    (let ((col (send (send mc :database "play-update") :collection "static"))
          (actor (make-instance 'mongo-update-actor
                                :filter (make-bag "{a:1}")
                                :update (make-bag "{$set:{b:3}}")
                                :destination "updated"
                                :url *murl*
                                :database "play-update"
                                :collection "static"
                                :timeout 3))
          (box (make-bag "{}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      (assert-equal "{updated: 1}" (send (cadr response) :write nil))))
  (deftest "funcall" suite
    (let ((col (send (send mc :database "play-update") :collection "funcall"))
          (actor (make-instance 'mongo-update-actor
                                :filter (lambda (box) (make-bag "{a:1}"))
                                :update (lambda (box) (make-bag "{$set:{b:3}}"))
                                :destination "updated"
                                :url *murl*
                                :database "play-update"
                                :collection "funcall"
                                :timeout 3))
          (box (make-bag "{}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      (assert-equal "{updated: 1}" (send (cadr response) :write nil)))))

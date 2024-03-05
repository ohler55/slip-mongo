
(let* ((suite (defsuite "insert-actor" mongo-suite)))
  (deftest "static" suite
    (let ((col (send (send mc :database "play-insert") :collection "static"))
          (actor (make-instance 'mongo-insert-actor
                                :source "record"
                                :destination "inserted"
                                :url *murl*
                                :database "play-insert"
                                :collection "static"
                                :timeout 3
                                :wrap nil))
          (box (make-bag "{record:{a:1,b:2}}"))
          response)
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      ;; (format t "*** box: ~A ~A~%" (car response) (send (cadr response) :write nil))
      (assert-match "{_id: \"[0-9a-f]+\" a: 1 b: 2}" (send (send (cadr response) :get "record" t) :write nil))
      (assert-match "{_id: \"[0-9a-f]+\" a: 1 b: 2}" (send (send (cadr response) :get "inserted" t) :write nil))
      (assert-equal 2 (cdr (assoc "b" (send col :find-one '((a . 1)) :native t :projection '((b . t))))))))
  (deftest "funcall" suite
    (let ((col (send (send mc :database "play-insert") :collection "funcall"))
          (actor (make-instance 'mongo-insert-actor
                                :source (lambda (box) (make-bag "{a:2 b:4}"))
                                :destination "inserted"
                                :url *murl*
                                :database "play-insert"
                                :collection "funcall"
                                :timeout 3
                                :wrap nil))
          (box (make-bag "{}"))
          response)
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      ;; (format t "*** box: ~A ~A~%" (car response) (send (cadr response) :write nil))
      (assert-match "{_id: \"[0-9a-f]+\" a: 2 b: 4}" (send (send (cadr response) :get "inserted" t) :write nil)))))

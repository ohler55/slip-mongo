
(let ((suite (defsuite "find-actor" mongo-suite)))
  (deftest "static" suite
    (let ((col (send (send mc :database "play-find") :collection "static"))
          (actor (make-instance 'mongo-find-actor
                                :filter (make-bag "{a:1}")
                                :projection (make-bag "{a:true b:true}")
                                :sort (make-bag "{b:1}")
                                :destination "found"
                                :url *murl*
                                :database "play-find"
                                :collection "static"
                                :timeout 3
                                :wrap nil))
          (box (make-bag "{}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      ;; (format t "*** response: ~A ~A~%" (car response) (send (cadr response) :write nil))
      (assert-match "{found: {_id: \"[0-9a-f]+\" a: 1 b: 2}}" (send (cadr response) :write nil))))
  (deftest "lookup" suite
    (let ((col (send (send mc :database "play-find") :collection "lookup"))
          (actor (make-instance 'mongo-find-actor
                                :filter "query"
                                :sort "query"
                                :projection "proj"
                                :destination "found"
                                :url *murl*
                                :database "play-find"
                                :collection (lambda (box) (bag-get box "table"))
                                :timeout 3
                                :wrap nil))
          (box (make-bag "{query: {a: 1} table: lookup proj: {b: 1}}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      ;; (format t "*** response: ~A ~A~%" (car response) (send (cadr response) :write nil))
      (assert-match
       "{found: {_id: \"[0-9a-f]+\" b: 2} proj: {b: 1} query: {a: 1} table: lookup}"
       (send (cadr response) :write nil))))
  (deftest "funcall" suite
    (let ((col (send (send mc :database "play-find") :collection "funcall"))
          (actor (make-instance 'mongo-find-actor
                                :filter (lambda (box) (make-bag "{a:1}"))
                                :projection (lambda (box) (make-bag "{a:true b:true}"))
                                :sort (lambda (box) (make-bag "{b:-1}"))
                                :destination "found"
                                :url *murl*
                                :database "play-find"
                                :collection (lambda (box) "funcall")
                                :timeout 3
                                :wrap nil))
          (box (make-bag "{}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      ;; (format t "*** response: ~A ~A~%" (car response) (send (cadr response) :write nil))
      (assert-match "{found: {_id: \"[0-9a-f]+\" a: 1 b: 2}}" (send (cadr response) :write nil))))
  )


(let ((suite (defsuite "find-actor" mongo-suite
               :setup (lambda () (setq db (send mc :database "play-find"))))))
  (deftest "basic-perform" suite
    (let ((col (send db :collection "basic"))
          (actor (make-instance 'mongo-find-actor
                                :filter "query"
                                :destination "found"
                                :url *murl*
                                :database "play-find"
                                :collection "basic"
                                :timeout 3
                                :wrap nil))
          (box (make-bag "{query: {a: 1}}"))
          response)
      (send col :insert-one '((a . 1) (b . 2)))
      (setq response (send actor :perform box))
      (assert-equal 'ok (car response))
      ;; (format t "*** response: ~A ~A~%" (car response) (send (cadr response) :write nil))
      (assert-match "{found: {_id: \"[0-9a-f]+\" a: 1 b: 2} query: {a: 1}}" (send (cadr response) :write nil)))))

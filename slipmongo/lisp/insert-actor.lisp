
(defflavor mongo-insert-actor (source destination) (mongo-actor)
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables
 (:documentation
  "An actor used to insert one record in a mongo database."))

(defmethod (mongo-insert-actor :perform) (box)
  "Insert a record using mongo-insert-one. The inserted record is return as a _bag_ instance with the id set."
  (let* ((cname (cond ((functionp collection) (apply collection (list box)))
                      (t collection)))
         (rec (bag-from-arg source box))
         (col (send (send client :database database) :collection cname))
         (rid (send col :insert-one rec :wrap wrap)))
    (bag-set rec rid "_id")
    (send box :set rec destination))

  (list 'ok box))


(defflavor mongo-update-actor (filter update destination) (mongo-actor)
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables
 (:documentation
  "An actor used to update one record in a mongo database."))

(defmethod (mongo-update-actor :perform) (box)
  "Using the filter variable call mongo-update-one to identify a matching record
   which is then updated. The number of records updated is returned."
  (let* ((cname (cond ((functionp collection) (apply collection (list box)))
                      (t collection)))
         (f (bag-from-arg filter box))
         (up (bag-from-arg update box))
         (col (send (send client :database database) :collection cname)))

    (send box :set (send col :update-one f up) destination))

  (list 'ok box))

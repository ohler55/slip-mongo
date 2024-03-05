
(defflavor mongo-replace-actor (filter replacement destination) (mongo-actor)
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables
 (:documentation
  "An actor used to replace one record in a mongo database."))

(defmethod (mongo-replace-actor :perform) (box)
  "Using the filter variable call mongo-replace-one to identify a matching record
   which is then replaced. The number of records replaced is returned."
  (let* ((cname (cond ((functionp collection) (apply collection (list box)))
                      (t collection)))
         (f (bag-from-arg filter box))
         (rep (bag-from-arg replacement box))
         (col (send (send client :database database) :collection cname)))

    (send box :set (send col :replace-one f rep) destination))

  (list 'ok box))

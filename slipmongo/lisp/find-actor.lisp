
(defflavor mongo-find-actor (filter projection sort skip destination) (mongo-actor)
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables
 (:documentation
  "An actor used to find one record in a mongo database."))

(defmethod (mongo-find-actor :perform) (box)
  "Using the filter variable call mongo-find-one to retrieve a matching record
   which is then placed in the destination of the box argument using the :set
   method on the box."
  (let* ((cname (cond ((functionp collection) (apply collection (list box)))
                      (t collection)))
         (f (bag-from-arg filter box))
         (proj (bag-from-arg projection box))
         (srt (bag-from-arg sort box))
         (skp (bag-from-arg skip box))
         (col (send (send client :database database) :collection cname)))

    (send box :set (send col :find-one f :projection proj :sort srt :wrap wrap :skip skp) destination))

  (list 'ok box))

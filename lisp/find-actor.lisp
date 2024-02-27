
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
  (let ((col (send (send client :database database) :collection collection))
        (f (cond ((stringp filter) (send box :get filter t))
                 ((functionp filter) (apply filter (list box)))
                 (t nil)))
        (proj (cond ((stringp projection) (send box :get projection t))
                    ((functionp projection) (apply projection (list box)))
                    (t nil)))
        (srt (cond ((stringp sort) (send box :get sort t))
                   ((functionp sort) (apply sort (list box)))
                   (t nil))))

    (send box :set (send col :find-one f :projection proj :sort srt :wrap wrap) destination))

  (list 'ok box))

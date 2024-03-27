
(defflavor mongo-actor (url database collection timeout wrap client) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables
 :abstract-flavor
 (:documentation
  "The common flavor used by all mongo actors."))

(defmethod (mongo-actor :after :init) (plist)
  (setq client (mongo-connect url :timeout timeout)))

(defun bag-from-arg (arg box)
  (cond ((stringp arg) (send box :get arg t))
        ((functionp arg) (funcall arg box))
        (t arg)))


- next

 - flavors
  - mongo-collection
   + :aggregate
   x :bulk-write ;; maybe don't implement yet
   x :clone ;; don't implement
   + :count-documents
   + :database
   + :delete-many
   + :delete-one
   + :distinct
   + :drop
   + :estimated-document-count
   + :find
   + :find-one
   + :find-one-and-delete
   + :find-one-and-replace
   + :find-one-and-update
   + :indexes
   + :insert-many
   + :insert-one
   + :name
   + :replace-one
   - :search-indexes ;;
   + :update-by-id
   + :update-many
   + :update-one
   - :watch ;;

----------
 - actor branch
  - test driver
   - use slip test package
  - defun to get string from value which is string or func
   - need to eval in order or at least have a primary/base
   - (cond string or typep x function or lambda)
    - maybe implement functionp
  - various actors
   - mongo-find-actor (mongo-actor)
    - filter
   - mongo-insert-actor (mongo-actor)
    - func or path to get data to save from box
    - path to put returned id
   - mongo-update-actor (mongo-find-actor)
    - update
    - path to put returned count or nil to ignore
   - mongo-replace-actor (mongo-find-actor)
    - replacement func or path
    - path to put returned count or nil to ignore
   - mongo-actor - abstract
    - mongo url string or func
    - database string or func
    - collection string or func
    - timeout fixnum
    - wrap boolean

----------
 - watch branch
  - collection :watch
  - mongo-change-stream flavor

----------
 - search-indexes branch
  - collection :search-indexes
  - mongo-search-indexes flavor

----------
 - database :create-view
 - how to make read only, maybe just a view?

-----------------------------------------------

  - mongo-search-indexes
   - :create
    - create one, don't bother with create many
   - :drop
    - if with name then drop one else drop all
   - :list
   - :update

  - mongo-change-stream
   - :close
   - :decode
   - :error
   - :identifier ;; id in mongo
   - :next
   - :set-batch-size
   - :resume-token
   - :try-next


 - future sessions managed by clients
  - used for transactions


 - watch with change-stream instance
  - support watch with a channel?
   - set up go routine to call ChangeStream.Next and place on channel
    - or maybe just pass the changestream back and let the user decide
 - no bulk write

- be smart about fixnum conversions and consider the range of int (32bit) vs long (64bit)
- time in bag goes to date

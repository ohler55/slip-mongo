
- next

 - flavors
  - mongo-client
   + mongo-connect (url &key user password timeout)
   + :database (name)
   + :disconnect
   + :databases (filter)
   + :ping
   - :watch
  - mongo-database
   - :aggregate
   - :client
   - :collection (name)
   - :create-view
   - :drop
   - :collections (filter) ;; collection specifications
   + :name
   - :watch
  - mongo-change-stream
   - :close
   - :decode
   - :error
   - :identifier ;; id in mongo
   - :next
   - :set-batch-size
   - :resume-token
   - :try-next
  - mongo-collection
   - :aggregate
   - :bulk-write ;; maybe don't implement yet
   - :clone
   - :count-documents
   - :database
   - :delete-many
   - :delete-one
   - :distinct
   - :drop
   - :estimated-document-count
   - :find
   - :find-one
   - :find-one-and-delete
   - :find-one-and-replace
   - :find-one-and-update
   - :indexes
   - :insert-many
   - :name
   - :replace-one
   - :search-indexes
   - :update-by-id
   - :update-many
   - :update-one
   - :watch

 - future sessions managed by clients
  - used for transactions


 - watch with change-stream instance
  - support watch with a channel?
   - set up go routine to call ChangeStream.Next and place on channel
    - or maybe just pass the changestream back and let the user decide
 - no bulk write

- be smart about fixnum conversions and consider the range of int (32bit) vs long (64bit)
- time in bag goes to date

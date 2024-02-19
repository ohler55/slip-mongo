
- next

 - :insert-many
 - :update-many

 - how to make read only, maybe just a view?

 - database :create-view

 - flavors
  - mongo-collection
   - :aggregate
   - :bulk-write ;; maybe don't implement yet
   - :clone
   - :count-documents
   + :database
   - :delete-many
   - :delete-one
   - :distinct
   + :drop
   - :estimated-document-count
   + :find
   + :find-one
   - :find-one-and-delete
   - :find-one-and-replace
   - :find-one-and-update
   - :indexes ;; return mongo-indexes instance
   - :insert-many
   + :insert-one
   + :name
   - :replace-one
   - :search-indexes
   - :update-by-id
   - :update-many
   + :update-one
   - :watch
  - indexes
   - create
    - create one, don't bother with create many
   - drop
    - if with name then drop one else drop all
   - list

  - mongo-change-stream
   - :close
   - :decode
   - :error
   - :identifier ;; id in mongo
   - :next
   - :set-batch-size
   - :resume-token
   - :try-next

 - actors
  - find-one
  - insert-one
  - update-one
  - generic
   - method, filter, keys

 - future sessions managed by clients
  - used for transactions


 - watch with change-stream instance
  - support watch with a channel?
   - set up go routine to call ChangeStream.Next and place on channel
    - or maybe just pass the changestream back and let the user decide
 - no bulk write

- be smart about fixnum conversions and consider the range of int (32bit) vs long (64bit)
- time in bag goes to date


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

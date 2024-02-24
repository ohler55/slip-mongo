
- next

 - defun and others in a package
  - use-package make other not available
 - maybe allow package cross use/import
 - better, how can it be done in sbcl?

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
   - :indexes ;; return mongo-indexes instance
   + :insert-many
   + :insert-one
   + :name
   + :replace-one
   - :search-indexes
   + :update-by-id
   + :update-many
   + :update-one
   - :watch

----------
 - indexes branch
  - collection :indexes
  - mongo-indexes flavor

----------
 - actor branch
  - load lisp code
  - test driver
  - various actors
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

  - mongo-indexes
   - :create
    - create one, don't bother with create many
   - :drop
    - if with name then drop one else drop all
   - :list

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

 - actors
  - lisp code
   - set slip.CurrentPackage then compile (defer to set back)
   - separate lisp file for each
   - maybe a test driver for actors (copied from one repo to the next)
    - use slip test package
   - use fs to load
  - mongo-find-one-actor
  - mongo-find-actor
  - mongo-insert-actor
  - mongo-update-actor
  - mongo-actor
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

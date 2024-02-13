
- next
 - mongo-connect (url &key user password timeout)
  - return instance of mongo-client
   - should the "-flavor" be left off? for this and others - yes
   -
 - flavors
  - mongo-client
  - mongo-database
  - mongo-change-stream
  - mongo-collection



 - watch with change-stream instance
  - support watch with a channel?
   - set up go routine to call ChangeStream.Next and place on channel
    - or maybe just pass the changestream back and let the user decide
 - no bulk write

- be smart about fixnum conversions and consider the range of int (32bit) vs long (64bit)
- time in bag goes to date

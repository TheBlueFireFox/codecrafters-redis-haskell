module Parse.RDB where

{-
    00000000  52 45 44 49 53 30 30 31  30 fa 09 72 65 64 69 73  |REDIS0010..redis|
    00000010  2d 76 65 72 06 37 2e 30  2e 31 31 fa 0a 72 65 64  |-ver.7.0.11..red|
    00000020  69 73 2d 62 69 74 73 c0  40 fa 05 63 74 69 6d 65  |is-bits.@..ctime|
    00000030  c2 9f 0e 3b 65 fa 08 75  73 65 64 2d 6d 65 6d c2  |...;e..used-mem.|
    00000040  10 20 0e 00 fa 08 61 6f  66 2d 62 61 73 65 c0 00  |. ....aof-base..|
    00000050  fe 00 fb 01 00 00 05 6d  79 6b 65 79 05 6d 79 76  |.......mykey.myv|
    00000060  61 6c ff e5 6a 9a d9 9c  bf 00 83                 |al..j......|
    0000006b

    ----------------------------#
    52 45 44 49 53              # Magic String "REDIS"
    30 30 30 33                 # RDB Version Number as ASCII string. "0003" = 3
    ----------------------------
    FA                          # Auxiliary field
    $string-encoded-key         # May contain arbitrary metadata
    $string-encoded-value       # such as Redis version, creation time, used memory, ...
    ----------------------------
    FE 00                       # Indicates database selector. db number = 00
    FB                          # Indicates a resizedb field
    $length-encoded-int         # Size of the corresponding hash table
    $length-encoded-int         # Size of the corresponding expire hash table
    ----------------------------# Key-Value pair starts
    FD $unsigned-int            # "expiry time in seconds", followed by 4 byte unsigned int
    $value-type                 # 1 byte flag indicating the type of value
    $string-encoded-key         # The key, encoded as a redis string
    $encoded-value              # The value, encoding depends on $value-type
    ----------------------------
    FC $unsigned long           # "expiry time in ms", followed by 8 byte unsigned long
    $value-type                 # 1 byte flag indicating the type of value
    $string-encoded-key         # The key, encoded as a redis string
    $encoded-value              # The value, encoding depends on $value-type
    ----------------------------
    $value-type                 # key-value pair without expiry
    $string-encoded-key
    $encoded-value
    ----------------------------
    FE $length-encoding         # Previous db ends, next db starts.
    ----------------------------
    ...                         # Additional key-value pairs, databases, ...

    FF                          ## End of RDB file indicator
    8-byte-checksum             ## CRC64 checksum of the entire file.
-}
todo = ""

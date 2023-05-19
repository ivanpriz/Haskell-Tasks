data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Error _ = GT
cmp Warning Warning = EQ
cmp Warning Error = LT
cmp Warning _ = GT
cmp Info Info = EQ
cmp Info _ = LT
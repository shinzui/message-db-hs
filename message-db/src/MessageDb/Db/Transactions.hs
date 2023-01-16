module MessageDb.Db.Transactions
  ( writeStreamMessage,
    writeStreamMessages,
  )
where

import Hasql.Transaction
import MessageDb.Db.Statements qualified as Db
import MessageDb.Message (NewMessage, StreamPosition)

writeStreamMessage :: NewMessage -> Transaction StreamPosition
writeStreamMessage evt = statement evt Db.writeStreamMessage

writeStreamMessages :: Traversable t => t NewMessage -> Transaction (t StreamPosition)
writeStreamMessages = traverse writeStreamMessage

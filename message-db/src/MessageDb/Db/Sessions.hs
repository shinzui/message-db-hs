module MessageDb.Db.Sessions
  ( getStreamMessages,
    getCategoryMessages,
    getLastStreamMessage,
    getStreamVersion,
    writeStreamMessage,
    writeStreamMessages,
  )
where

import Data.Vector
import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
import MessageDb.Db.Statements qualified as Db
import MessageDb.Db.Transactions qualified as T
import MessageDb.Message (Message, NewMessage, StreamPosition)
import MessageDb.Stream (Stream)

getStreamMessages :: Db.GetStreamMessagesQuery -> Session (Vector Message)
getStreamMessages query = statement query Db.getStreamMessages

getCategoryMessages :: Db.GetCategoryMessagesQuery -> Session (Vector Message)
getCategoryMessages query = statement query Db.getCategoryMessages

getLastStreamMessage :: Stream -> Session (Maybe Message)
getLastStreamMessage stream = statement stream Db.getLastStreamMessage

getStreamVersion :: Stream -> Session (Maybe StreamPosition)
getStreamVersion stream = statement stream Db.getStreamVersion

writeStreamMessage :: NewMessage -> Session StreamPosition
writeStreamMessage = transaction ReadCommitted Write . T.writeStreamMessage

writeStreamMessages :: Traversable t => t NewMessage -> Session (t StreamPosition)
writeStreamMessages = transaction ReadCommitted Write . T.writeStreamMessages

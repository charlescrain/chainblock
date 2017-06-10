{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module ChainBlock.DB.Interfaces where


import           ChainBlock.DB.Types
import           Servant             (Handler, ServantErr)

-- TODO: Need to complete this interface
data IDataBase m m' =
  IDataBase { queryAllUsers :: m [User]
            , queryUser :: UserId -> m User
            , insertUser :: Username -> m UserId
            , queryWebsite :: UserId -> m [Website]
            , queryWebsiteCredentials :: UserId
                                      -> WebsiteId
                                      -> m [WebsiteCredentials]
            , runDBInterface :: forall a . m a -> m' a
            }



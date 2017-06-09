{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module ChainBlock.Interfaces where


import           ChainBlock.API.Types
import           Servant              (Handler, ServantErr)

data IRouteFunctions m =
  IRouteFunctions { getUsers :: m [User]
                  , postUser :: PostUserBody -> m UserId
                  , getWebsites :: UserId -> m [WebsiteDetails]
                  , getWebsiteCredentials :: UserId
                                          -> WebsiteId
                                          -> PostMasterKey
                                          -> m Website
                  , postWebsites :: UserId -> PostMasterKey -> m WebsiteId
                  , runRouteInterface :: forall a . m a -> Handler a
                  }



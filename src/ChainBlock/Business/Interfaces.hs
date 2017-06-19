{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module ChainBlock.Business.Interfaces where


import           ChainBlock.API.Types
import           Servant              (Handler, ServantErr)

data IBusinessFunctions m m' =
  IBusinessFunctions { getUsers :: m [User]
                     , postUser :: PostUserBody -> m UserId
                     , getWebsites :: UserId -> m [WebsiteDetails]
                     , getWebsiteCredentials :: UserId
                                             -> WebsiteId
                                             -> PostMasterKey
                                             -> m Website
                     , postWebsites :: UserId -> PostMasterKey -> m WebsiteId
                     , runBusinessInterface :: forall a . m a -> m' a
                     }



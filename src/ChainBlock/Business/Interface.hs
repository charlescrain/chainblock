{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module ChainBlock.Business.Interface where


import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (MonadLogger)

import           ChainBlock.Business.Types
import           ChainBlock.Errors         (CBError (..))

class ( MonadError CBError m
      , MonadThrow m
      , MonadIO m
      , MonadCatch m
      , MonadLogger m
      ) => MonadBusiness m

data IBusinessFunctions m m' =
  IBusinessFunctions { -- Users
                       getUsers :: m [User]
                     , postUser :: PostUserBody -> m UserId

                       -- Website
                     , getWebsites  :: UserId -> m [WebsiteDetails]
                     , postWebsites :: UserId -> PostWebsite -> m WebsiteId
                     , getCredentials  :: UserId
                                       -> WebsiteId
                                       -> PostMasterKey
                                       -> m Website
                     , postCredentials :: UserId
                                       -> WebsiteId
                                       -> PostCredentials
                                       -> m ()

                       -- run function
                     , runBusinessInterface :: ( forall a . m a -> m' a)
                     }



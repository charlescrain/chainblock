{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Tholos.Business.Interface where


import           Control.Monad.Catch       (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (MonadLogger)

import           Tholos.Types
import           Tholos.API.Types
import           Tholos.Errors             (CBError (..))
import           Tholos.Monad              (MonadTholos)


data IBusinessFunctions m =
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
                     }



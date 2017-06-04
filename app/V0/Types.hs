module V0.Types where

import           ChainBlock.API.Types

data IRouteFunctions m =
  IRouteFunctions { getUsers :: m [User]
                  , postUser :: PostUserBody -> m UserId
                  , getWebsites :: UserId -> m [WebsiteDetails]
                  , getWebsiteCredentials :: UserId
                                          -> WebsiteId
                                          -> PostMasterKey
                                          -> m Website
                  , postWebsites :: UserId -> PostMasterKey -> m WebsiteId
                  }


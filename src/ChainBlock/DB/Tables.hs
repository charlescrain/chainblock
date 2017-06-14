module ChainBlock.DB.Types where

import           Data.Profunctor.Product (p2, p3, p4)
import           Opaleye                 (Column, PGInt4, PGText, Table (Table),
                                          optional, required, (.<), (.==))


-----------------------------------------------------
-- | User
-----------------------------------------------------

userTable :: Table (Maybe (Column PGInt4), Column PGText)
                     (Column PGInt4, Column PGText)
userTable = Table "userTable" (p2 ( optional "id"
                                   , required "name" ))

-----------------------------------------------------
-- | Website
-----------------------------------------------------

credentialsTable :: Table (Column PGText, Column PGText, Column PGInt4)
                          (Column PGText, Column PGBytea, Column PGInt4)
credentialsTable = Table "credentialsTable" (p3 ( required "username"
                                                , required "encrypted_pass"
                                                , required "website_id" ))

websiteTable :: Table (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
                      (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
websiteTable = Table "credentialsTable" (p4 ( required "id"
                                            , required "website_url"
                                            , required "website_name"
                                            , required "user_id" ))


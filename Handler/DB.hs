{-# LANGUAGE ScopedTypeVariables #-}
module Handler.DB where

import Import
import Permissions
import Data.Aeson.Lens
import Control.Lens ((^?)) -- , (^.))

-- import Database.MongoDB.Query (MongoContext(..))
-- import Data.Aeson.Types (emptyObject)


-- Module dedicated to accessing Database
getUserChannelsR :: ApiReq [YTChannel]
getUserChannelsR =
  TC . catMaybes <$> do
        uid <- getUserIdent
        runDB $ selectList [ChannelMemberUser ==. uid] []
          >>= mapM (\(Entity _ q) -> get $ channelMemberRef q )

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- Insert -- post
postAllUserR :: AppM Value
postAllUserR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> runMaybeT $ do
            email <- liftMaybe (v ^? key "email" . _String )
            MaybeT $ runDB $ getBy (UniqueUser email) >>= \case
                Just (Entity uid _) -> return $ Just uid
                Nothing -> Just <$> insert User
                          { userIdent = email
                          , userName      = Nothing
                          , userFriendly  = Nothing
                          , userSiteAdmin = False
                          }

getAllSiteGroupR      = listsOfAll :: ApiReq [SiteGroup      ]
getAllUserR           = listsOfAll :: ApiReq [User           ]
-- | debug stuff -- mongo admin?
getAllOAuthAccess     = listsOfAll :: ApiReq [OAuthAccess    ]
getAllEmail           = listsOfAll :: ApiReq [Email          ]
getAllYTChannel       = listsOfAll :: ApiReq [YTChannel      ]
getAllYTPlaylist      = listsOfAll :: ApiReq [YTPlaylist     ]
getAllYTVideoPlaylist = listsOfAll :: ApiReq [YTVideoPlaylist]
getAllYTVideo         = listsOfAll :: ApiReq [YTVideo        ]
getAllYTVideoUser     = listsOfAll :: ApiReq [YTVideoUser    ]
getAllChannelMember   = listsOfAll :: ApiReq [ChannelMember  ]
getAllSiteGroupMember = listsOfAll :: ApiReq [SiteGroupMember]
getAllSiteGroup       = listsOfAll :: ApiReq [SiteGroup      ]
getAllVirtualVideo    = listsOfAll :: ApiReq [VirtualVideo   ]
getAllEvent           = listsOfAll :: ApiReq [Event          ]
getAllPlaylistEvent   = listsOfAll :: ApiReq [PlaylistEvent  ]

-- | type magic
--   convert any list of all into a respoce
listsOfAll = do
    guardAllAdmin
    TC . map (\(Entity _ v) -> v) <$> runDB (selectList [] [])

-- | Add some anonymous user, without adding her to any group
putAllUserR :: AppM ()
putAllUserR = guardAllAdmin

{-
postAllUserR :: AppM Value
postAllUserR =
        restPermsM permAllAdmin $ \(v :: Value) -> runMaybeT $ do
            email <- liftMaybe (v ^? key "email" . _String )
            MaybeT $ runDB $ getBy (UniqueUser email) >>= \case
                Just (Entity uid _) -> return $ Just uid
                Nothing -> Just <$> insert User
                          { userIdent = email
                          , userName      = Nothing
                          , userFriendly  = Nothing
                          , userSiteAdmin = False
                          }
-}

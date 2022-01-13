module Pinata.Model.User' where

import Control.Arrow (returnA)
import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import Data.Int (
  Int32,
  Int64,
 )
import Data.Profunctor.Product.Default (Default (..))
import qualified Data.Profunctor.Product.TH as PPTH
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Word (Word64)

import GHC.Generics (Generic)

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types.Internal.AST (OperationType)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Int (Int64)
import Opaleye ((.===))
import qualified Opaleye as DB
import Pinata.Auth.JWT
import Pinata.Auth.Password
import Pinata.Config
import qualified Pinata.DB as DB
import Pinata.Graphql
import qualified Pinata.Graphql

-------------------------------------------------------------------------------
data UserT a b c d = User
  { userId :: a
  , userEmail :: b
  , userPasswordHash :: c
  , userName :: d
  }

$(PPTH.makeAdaptorAndInstanceInferrable "pUser" ''UserT)

type User = DB.TimestampedRow (UserT Int Text Text Text)

type UserWriteField =
  DB.TimestampedWriteField
    ( UserT
        (Maybe (DB.Field DB.PGInt4)) -- use Maybe because we don't need to specify id when inserting
        (DB.Field DB.PGText)
        (DB.Field DB.PGText)
        (DB.Field DB.PGText)
    )

type UserField =
  DB.TimestampedReadField
    ( UserT
        (DB.Field DB.PGInt4)
        (DB.Field DB.PGText)
        (DB.Field DB.PGText)
        (DB.Field DB.PGText)
    )

userTable :: DB.Table UserWriteField UserField
userTable =
  DB.table "users" . DB.pTimestampedRow . DB.withTimestampFields $
    pUser
      User
        { userId = DB.tableField "id"
        , userEmail = DB.tableField "email"
        , userPasswordHash = DB.tableField "password_hash"
        , userName = DB.tableField "name"
        }

-------------------------------------------------------------------------------
type UserID = Int

-------------------------------------------------------------------------------
userSelect :: DB.Select UserField
userSelect = DB.selectTable userTable

-- -------------------------------------------------------------------------------
insertUser :: (Text, Text, Text) -> DB.Insert Int64
insertUser (userEmail, userPasswordHash, userName) =
  DB.Insert
    { iTable = userTable
    , iRows =
        DB.withTimestamp
          [ User
              { userId = Nothing
              , userEmail = DB.toFields userEmail
              , userPasswordHash = DB.toFields userPasswordHash
              , userName = DB.toFields userName
              }
          ]
    , iReturning = DB.rCount
    , iOnConflict = Nothing
    }

-------------------------------------------------------------------------------
findUserByEmail :: Text -> DB.Select UserField
findUserByEmail email =
  proc () -> do
    user <- userSelect -< ()
    let userDetail = DB.record user
    DB.restrict -< userEmail userDetail .=== DB.toFields email
    returnA -< user

-------------------------------------------------------------------------------
findUserByID :: UserID -> DB.Select UserField
findUserByID id =
  proc () -> do
    user <- userSelect -< ()
    let userDetail = DB.record user
    DB.restrict -< userId userDetail .=== DB.toFields id
    returnA -< user

-------------------------------------------------------------------------------
updateUserPassword :: UserID -> Text -> DB.Update Int64
updateUserPassword id newPasswordHash =
  DB.Update
    { uTable = userTable
    , uUpdateWith = DB.updateTimestampedRecord updatePasswordHash
    , uWhere = (.===) (DB.toFields id) . userId . DB.record
    , uReturning = DB.rCount
    }
  where
    updatePasswordHash userData =
      userData{userPasswordHash = DB.toFields newPasswordHash}

--
--
--

-- -----------------------------------------------------------------------------
-- userResolver :: GraphQL o => User -> Value o User
-- userResolver user =
--   let User{userId, userEmail, userName} = record user
--    in return
--         User
--           { id = pure userId
--           , email = pure userEmail
--           , name = pure userName
--           , createdAt = pure . T.pack . show $ recordCreatedAt user
--           , updatedAt = pure . T.pack . show $ recordUpdatedAt user
--           }

-- -------------------------------------------------------------------------------
-- loginResolver :: GraphQL o => LoginArgs -> Value o Session
-- loginResolver LoginArgs{email, password} = do
--   res :: [User] <- runSelect $ findUserByEmail email
--   case res of
--     [user] | validateHashedPassword (DB.userPasswordHash . record $ user) password -> do
--       time <- liftIO getCurrentTime
--       secret <- lift $ asks (jwtSecret . Graphql.config)
--       let jwt = makeJWT time secret (DB.userId . record $ user)
--       return Session{token = pure jwt, user = userResolver user}
--     _ -> fail "Wrong email or password"

-- -------------------------------------------------------------------------------
-- registerResolver :: RegisterArgs -> Value MUTATION Session
-- registerResolver RegisterArgs{email, password, name} = do
--   res :: [User] <- runSelect $ findUserByEmail email
--   case res of
--     _ : _ -> fail "This email is already taken"
--     [] -> do
--       ph <- liftIO $ hashPassword password
--       runInsert $ insertUser (email, ph, name)
--       loginResolver LoginArgs{email, password}

-- -------------------------------------------------------------------------------
-- myUserInfoResolver :: Value QUERY User
-- myUserInfoResolver = do
--   myUserId <- requireAuthorized
--   runSelectOne (findUserByID myUserId) "Invalid user" >>= userResolver

-- -------------------------------------------------------------------------------
-- changePasswordResolver :: ChangePasswordArgs -> Value MUTATION Bool
-- changePasswordResolver ChangePasswordArgs{oldPassword, newPassword} = do
--   myUserId <- requireAuthorized
--   userData :: User <- runSelectOne (findUserByID myUserId) "Invalid user"
--   if validateHashedPassword (DB.userPasswordHash . record $ userData) oldPassword
--     then do
--       ph <- liftIO $ hashPassword newPassword
--       runUpdate $ updateUserPassword myUserId ph
--       return True
--     else fail "Wrong old password"

-- -------------------------------------------------------------------------------
-- allUsersResolver :: GraphQL o => Composed o [] User
-- allUsersResolver = do
--   res :: [User] <- runSelect userSelect
--   traverse userResolver res

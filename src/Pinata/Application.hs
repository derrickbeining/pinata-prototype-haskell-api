module Pinata.Application where

import Pinata.Auth.JWT

import Pinata.Config

import Control.Monad.Trans (
  lift,
  liftIO,
 )
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (runReaderT)

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (find)
import Data.Morpheus (interpreter)
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)

import Database.PostgreSQL.Simple

import Pinata.Error

import qualified Pinata.Graphql
import qualified Pinata.Graphql.Resolver as Resolver

import Network.HTTP.Types.Status (
  mkStatus,
  status200,
  status404,
 )
import Network.Wai.Middleware.Cors

import Web.Scotty

-------------------------------------------------------------------------------
api :: B.ByteString -> Pinata.Graphql.Web B.ByteString
api = interpreter Resolver.root

-------------------------------------------------------------------------------
app :: IO ()
app = do
  init <- runExceptT initialize
  case init of
    Left err -> putStrLn err
    Right res -> webServer res

-------------------------------------------------------------------------------
extractAuthorizationToken :: [(LT.Text, LT.Text)] -> Maybe LT.Text
extractAuthorizationToken
  headers = case find ((== "authorization") . LT.toLower . fst) headers of
    Just (_, authorization) -> case LT.splitOn " " authorization of
      [token] -> Just token
      ["Bearer", token] -> Just token
      ["bearer", token] -> Just token
      _ -> Nothing
    _ -> Nothing

-------------------------------------------------------------------------------
webServer :: (Config, Pool Connection) -> IO ()
webServer (config@Config{port, jwtSecret}, connectionPool) = scotty port $ do
  let myCors =
        cors
          ( const $
              Just
                ( simpleCorsResourcePolicy
                    { corsRequestHeaders = ["Content-Type"]
                    }
                )
          )
  middleware myCors
  post "/api" $ do
    reqBody <- body
    reqHeaders <- headers
    currentTime <- liftIO getCurrentTime
    let currentUserId = case extractAuthorizationToken reqHeaders of
          Just token ->
            verifyJWT currentTime jwtSecret (T.pack . LT.unpack $ token)
          _ -> Nothing
    let env = Pinata.Graphql.Env connectionPool config currentUserId
    response <-
      liftIO
        . flip runReaderT env
        . runExceptT
        . Pinata.Graphql.runWeb
        $ api reqBody
    case response of
      Left error -> do
        status status200
        json $ toErrorResponse error
      Right jsonResponse -> do
        setHeader "Content-Type" "application/json; charset=utf-8"
        status status200
        raw jsonResponse
  get "/graphiql" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file "graphiql.html"
  notFound $ do
    status status404
    text "Not found"

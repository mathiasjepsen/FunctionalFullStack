{-# LANGUAGE OverloadedStrings #-} -- Makes string literals polymorphic over the IsString type class, so you
                                   -- can use String, Text and ByteString in a default delcaration
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Network.Wai.Middleware.Cors
import Web.Scotty
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Prelude hiding (id)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent (newMVar, readMVar, takeMVar, putMVar)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text.Lazy as LazyText
import Network.HTTP.Types.Status


main :: IO ()
main = do
  membersRef <- newMVar $ IntMap.fromList [ (1, Member 1 "Kurt" "kurt@email.com")
                                          , (2, Member 2 "Sonja" "sonja@post.com")
                                          ]
  scotty 9000 $ do
    middleware simpleCors
    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat ["<h1>Hello ", name, " from Scotty!</h1><hr/>"]
    get "/member/count" $ do
      members <- lift $ readMVar membersRef
      json $ IntMap.size members
    get "/member" $ do
      members <- lift $ readMVar membersRef
      json $ IntMap.elems members
    get "/member/:id" $ do
      members <- lift $ readMVar membersRef
      idText <- param "id"
      let id = (read idText) :: Int
      case IntMap.lookup id members of
        Just member ->
          json member
        Nothing ->
          status status400
    post "/member" $ do
      member <- jsonData :: ActionM Member
      beforeMembers <- lift $ takeMVar membersRef
      let (updatedMember, afterMembers) = insertMember member beforeMembers
      lift $ putMVar membersRef afterMembers
      json updatedMember


data Member = Member { id :: Int
                     , name :: String
                     , email :: String
                     } deriving (Show, Generic)


instance ToJSON Member
instance FromJSON Member


insertMember :: Member -> IntMap Member -> (Member, IntMap Member)
insertMember member intmap
  | IntMap.member (id member) intmap == True = (member, IntMap.insert (id member) member intmap)
  | otherwise                                = (member, IntMap.insert mapSizeIncremented newMember intmap)
  where mapSizeIncremented = IntMap.size intmap + 1
        newMember = Member mapSizeIncremented (name member) (email member)

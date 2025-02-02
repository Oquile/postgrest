module Feature.OpenApi.RootSpec where

import Network.HTTP.Types
import Network.Wai        (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude hiding (get)

import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "root spec function" $ do
    it "accepts application/openapi+json" $
      request methodGet "/"
        [("Accept","application/openapi+json")] "" `shouldRespondWith`
        [json|{
           "swagger": "2.0",
           "info": {"title": "PostgREST API", "description": "This is a dynamic API generated by PostgREST"}
          }|]
        { matchHeaders = ["Content-Type" <:> "application/openapi+json; charset=utf-8"] }

    it "accepts application/json" $
      request methodGet "/"
        [("Accept", "application/json")] "" `shouldRespondWith`
        [json| {
            "tableName": "orders_view", "tableSchema": "test",
            "tableDeletable": true, "tableUpdatable": true,
            "tableInsertable": true, "tableDescription": null
          } |]
        { matchHeaders = [matchContentTypeJson] }

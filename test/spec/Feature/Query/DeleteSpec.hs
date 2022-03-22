module Feature.Query.DeleteSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Deleting" $ do
    context "existing record" $ do
      it "succeeds with 204 and deletion count" $
        request methodDelete "/items?id=eq.1"
            []
            ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/*" ]
            }

      it "returns the deleted item and count if requested" $
        request methodDelete "/items?id=eq.2" [("Prefer", "return=representation"), ("Prefer", "count=exact")] ""
          `shouldRespondWith` [json|[{"id":2}]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/1"]
          }

      it "ignores ?select= when return not set or return=minimal" $ do
        request methodDelete "/items?id=eq.3&select=id"
            []
            ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/*" ]
            }
        request methodDelete "/items?id=eq.3&select=id"
            [("Prefer", "return=minimal")]
            ""
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/*" ]
            }

      it "returns the deleted item and shapes the response" $
        request methodDelete "/complex_items?id=eq.2&select=id,name" [("Prefer", "return=representation")] ""
          `shouldRespondWith` [json|[{"id":2,"name":"Two"}]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }

      it "can rename and cast the selected columns" $
        request methodDelete "/complex_items?id=eq.3&select=ciId:id::text,ciName:name" [("Prefer", "return=representation")] ""
          `shouldRespondWith` [json|[{"ciId":"3","ciName":"Three"}]|]

      it "can embed (parent) entities" $
        request methodDelete "/tasks?id=eq.8&select=id,name,project:projects(id)" [("Prefer", "return=representation")] ""
          `shouldRespondWith` [json|[{"id":8,"name":"Code OSX","project":{"id":4}}]|]
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }

    context "known route, no records matched" $
      it "includes [] body if return=rep" $
        request methodDelete "/items?id=eq.101"
          [("Prefer", "return=representation")] ""
          `shouldRespondWith` "[]"
          { matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "*/*"]
          }

    context "totally unknown route" $
      it "fails with 404" $
        request methodDelete "/foozle?id=eq.101" [] "" `shouldRespondWith` 404

    context "table with limited privileges" $ do
      it "fails deleting the row when return=representation and selecting all the columns" $
        request methodDelete "/app_users?id=eq.1" [("Prefer", "return=representation")] mempty
            `shouldRespondWith` 401

      it "succeeds deleting the row when return=representation and selecting only the privileged columns" $
        request methodDelete "/app_users?id=eq.1&select=id,email" [("Prefer", "return=representation")]
          [json| { "password": "passxyz" } |]
            `shouldRespondWith` [json|[ { "id": 1, "email": "test@123.com" } ]|]
            { matchStatus  = 200
            , matchHeaders = ["Content-Range" <:> "*/*"]
            }

      it "suceeds deleting the row with no explicit select when using return=minimal" $
        request methodDelete "/app_users?id=eq.2"
            [("Prefer", "return=minimal")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus = 204
            , matchHeaders = [matchHeaderAbsent hContentType]
            }

      it "suceeds deleting the row with no explicit select by default" $
        request methodDelete "/app_users?id=eq.3"
            []
            mempty
          `shouldRespondWith`
            ""
            { matchStatus = 204
            , matchHeaders = [matchHeaderAbsent hContentType]
            }

    context "limited delete" $ do
      it "works with the limit and offset query params" $
        request methodDelete "/projects?limit=1&offset=4&select=id,name"
            [("Prefer", "return=representation")]
            mempty
          `shouldRespondWith`
            [json|[
              {
                  "id": 5,
                  "name": "Orphan"
              }]|]
            { matchStatus = 200 }

      it "works with the limit query param plus a filter" $
        request methodDelete "/projects?limit=1&id=gt.4&select=id,name"
            [("Prefer", "return=representation")]
            mempty
          `shouldRespondWith`
            [json|[
              {
                  "id": 5,
                  "name": "Orphan"
              }]|]
            { matchStatus = 200 }

      it "works with the limit and offset query params plus a filter" $
        request methodDelete "/projects?limit=1&offset=1&id=gt.3&select=id,name"
            [("Prefer", "return=representation")]
            mempty
          `shouldRespondWith`
            [json|[
              {
                  "id": 5,
                  "name": "Orphan"
              }]|]
            { matchStatus = 200 }

      it "works on a table with a composite pk" $
        request methodDelete "/employees?limit=1&select=first_name,last_name,occupation,salary"
            [("Prefer", "return=representation")]
            mempty
          `shouldRespondWith`
            [json| [
              {
                "first_name": "Daniel B.",
                "last_name": "Lyon",
                "occupation": "Packer",
                "salary": "$36,000.00"
              }]|]
            { matchStatus = 200 }

      it "works with views with an inferred pk" $
        request methodDelete "/projects_view?limit=1&offset=4&select=id,name"
            [("Prefer", "return=representation")]
            mempty
          `shouldRespondWith`
            [json|[
              {
                  "id": 5,
                  "name": "Orphan"
              }]|]
            { matchStatus = 200 }

    it "works on a table without a pk" $
      request methodDelete "/no_pk?limit=1&offset=1"
          [("Prefer", "return=representation")]
          mempty
        `shouldRespondWith`
          [json| [ { "a": "1", "b": "0" } ] |]
          { matchStatus = 200 }

    it "doesn't work on a view without a pk" $
      request methodDelete "/no_pk_view?limit=1&offset=1"
          [("Prefer", "return=representation")]
          mempty
        `shouldRespondWith`
        400

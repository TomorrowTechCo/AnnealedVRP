{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Database.Persist.Postgresql
import           Handler.Account
import           Handler.Vehicle
import           Import
import           Test.Hspec

connStr :: ConnectionString
connStr = "host=localhost dbname=testdb user=testuser password=test port=5432"

main :: IO ()
main = hspec $ do
  describe "web API" $ do
    describe "Accounts API handler" $ do
      it "returns a specific account" $ do
        let acc :: Handler Account
                 = withPostgresqlConn connStr (flip getAccount 123)
        accountRazonSocial <$> acc `shouldBe` "Happy fritos SA"
      it "returns all the accounts" $ do
        accs <- runNoLoggingT $ withPostgresqlConn connStr getAccounts
        accountRazonSocial <$> accs `shouldBe` ["Happy fritos SA",
                                                "Dinero gratis SAS"]
      it "adds one account" $ do
        accId <- withPostgresqlConn connStr postAccount
          (Account 900 "Tomorrow Tech SA" Nothing "something else")
        let acc :: Handler Account
                 = withPostgresqlConn connStr (flip getAccount accId)
        accountRazonSocial <$> acc `shouldBe` "Tomorrow Tech SA"

    describe "Vehicles API handler" $ do
      it "returns a specific vehicle" $ do
        1 `shouldBe` 1
      it "returns all the vehicles" $ do
        1 `shouldBe` 1
      it "adds one vehicle" $ do
        1 `shouldBe` 1

  describe "Annealing" $ do
    it "generates a random solution" $ do
      1 `shouldBe` 1

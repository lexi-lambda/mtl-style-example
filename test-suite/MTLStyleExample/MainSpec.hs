module MTLStyleExample.MainSpec where

import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

import MTLStyleExample.Main
import MTLStyleExample.Test.Stubs

spec :: Spec
spec = describe "main" $ do
  let epoch = posixSecondsToUTCTime 0
      ((), logMessages) = runIdentity $ main
        & runArgumentsT ["sample.txt"]
        & runFileSystemT [("sample.txt", "Alyssa")]
        & runLoggerT
        & runTickingClockT epoch

  it "prints two log messages" $
    length logMessages `shouldBe` 2

  it "prints a greeting as the first message" $
    (logMessages !! 0) `shouldBe` "Hello, Alyssa!"

  it "prints the elapsed time in milliseconds as the second message" $
    (logMessages !! 1) `shouldBe` "1000 milliseconds"

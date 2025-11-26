import Test.Tasty
import ParserTests


main :: IO ()
main = defaultMain $ testGroup "Janus tests" [ParserTests.tests]
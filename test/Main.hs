
import SFPL.Parser
import Test.HUnit

tests :: Test
tests = test ["test" ~: 1 @=? 1]

main :: IO ()
main = runTestTTAndExit tests

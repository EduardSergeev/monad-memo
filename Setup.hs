import Distribution.Simple
import Test.Main


main = defaultMainWithHooks simpleUserHooks { runTests = runt }

runt _ _ _ _ = run
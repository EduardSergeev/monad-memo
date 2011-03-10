import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Process

main = defaultMainWithHooks simpleUserHooks { runTests = run }

run _ _ pd lbi = system testprog >> return ()
    where testprog = (buildDir lbi) ++"/tests/tests"

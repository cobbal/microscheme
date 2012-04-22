module UnsafeLog where

import System.IO.Unsafe

unsafeLog :: Show a => a -> a
unsafeLog x = unsafePerformIO (print x >> return x)

unsafeLog' :: Show a => String -> a -> a
unsafeLog' label x = unsafePerformIO (putStr (label ++ ": ") >> print x >> return x)

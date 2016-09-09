module EulerUtils.IO
    ( timeit
    ) where 

import Data.Time.Clock

timeit :: IO a -> IO a
timeit action = do
    t0 <- getCurrentTime
    value <- action
    t1 <- getCurrentTime
    print (diffUTCTime t1 t0)
    return value
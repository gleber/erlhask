module ErlBifsCommon where

import ErlCore

bif_badarg :: String -> a
bif_badarg str = error ("badarg<" ++ str ++ ">")
-- bif_badarg str = error ("badarg<" ++ (show threadId) ++ "," ++ str ++ ">")

bif_badarg_num :: a
bif_badarg_num = bif_badarg "wrong-number"

bif_badarg_t :: a
bif_badarg_t = bif_badarg "wrong-types"

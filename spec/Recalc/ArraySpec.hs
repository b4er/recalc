{-|
Module      : Recalc.ArraySpec
Description : Tests for Array operations.
-}
module Recalc.ArraySpec where

import Control.Monad (forM_)
import Test.Hspec

import Numeric.LinearAlgebra ()
import Recalc.Array

spec :: Spec
spec =
  describe "tensorContract" $ do
    forM_ (zip [1 ..] examples) $ \(i :: Int, (x, expected)) ->
      it ("passes example-" <> show i) $ x `shouldBe` expected
    it "passes more examples" $ mapM_ (uncurry shouldBe) more

examples, more :: [(Array Double, Array Double)]
examples =
  [
    ( tensorContract [1] [0] (fromList [3, 1] [1 .. 3]) (fromList [1, 3] [1 .. 3])
    , fromList [3, 3] [1, 2, 3, 2, 4, 6, 3, 6, 9]
    )
  ,
    ( tensorContract [1] [0] (fromList [1, 3] [1 .. 3]) (fromList [3, 1] [1 .. 3])
    , fromList [1, 1] [14]
    )
  ,
    ( tensorContract [1] [0] (fromList [1, 1] [2]) (fromList [1, 1] [3])
    , fromList [1, 1] [6]
    )
  ,
    ( tensorContract [1] [0] (fromList [2, 2] [1, 0, 0, 1]) (fromList [2, 2] [1, 0, 0, 1])
    , fromList [2, 2] [1, 0, 0, 1]
    )
  ,
    ( tensorContract [1] [0] (fromList [2, 2] [1, 0, 0, 1]) (fromList [2, 2] [0, 1, 1, 0])
    , fromList [2, 2] [0, 1, 1, 0]
    )
  ,
    ( tensorContract [1] [0] (fromList [2, 2] [1, 0, 0, 1]) (fromList [2, 2] [1, 2, 3, 4])
    , fromList [2, 2] [1, 2, 3, 4]
    )
  ,
    ( tensorContract [1] [0] (fromList [2, 2] [0, 0, 0, 0]) (fromList [2, 2] [1, 2, 3, 4])
    , fromList [2, 2] [0, 0, 0, 0]
    )
  ,
    ( tensorContract [1] [0] (fromList [2, 3] [1 .. 6]) (fromList [3, 2] [7, 8, 9, 10, 11, 12])
    , fromList [2, 2] [58, 64, 139, 154]
    )
  ,
    ( tensorContract [1] [0] (fromList [3, 2] [1, 4, 2, 5, 3, 6]) (fromList [2, 3] [7, 8, 9, 10, 11, 12])
    , fromList [3, 3] [47, 52, 57, 64, 71, 78, 81, 90, 99]
    )
  ,
    ( tensorContract [1] [0] (fromList [3, 3] [1 .. 9]) (fromList [3, 3] [9, 8 .. 1])
    , fromList [3, 3] [30, 24, 18, 84, 69, 54, 138, 114, 90]
    )
  ,
    ( tensorContract [0] [0] (fromList [3, 2] [1 .. 6]) (fromList [3, 2] [1 .. 6])
    , fromList [2, 2] [35, 44, 44, 56]
    )
  ,
    ( tensorContract [2] [0] (fromList [2, 3, 4] [1 .. 24]) (fromList [4, 3] [1 .. 12])
    , fromList
        [2, 3, 3]
        [ 70
        , 80
        , 90
        , 158
        , 184
        , 210
        , 246
        , 288
        , 330
        , 334
        , 392
        , 450
        , 422
        , 496
        , 570
        , 510
        , 600
        , 690
        ]
    )
  ,
    ( tensorContract [1, 2] [1, 0] (fromList [2, 3, 4] [1 .. 24]) (fromList [4, 3, 5] [1 .. 60])
    , fromList
        [2, 5]
        [ 2608
        , 2686
        , 2764
        , 2842
        , 2920
        , 6712
        , 6934
        , 7156
        , 7378
        , 7600
        ]
    )
  ,
    ( tensorContract [0, 1, 2] [0, 1, 2] (fromList [2, 3, 4] [1 .. 24]) (fromList [2, 3, 4] [1 .. 24])
    , fromList [] [4900]
    )
  ,
    ( tensorContract [1] [0] (fromList [3, 4, 2] [1 .. 24]) (fromList [4, 2, 5] [1 .. 40])
    , fromList
        [3, 2, 2, 5]
        [ 356
        , 372
        , 388
        , 404
        , 420
        , 436
        , 452
        , 468
        , 484
        , 500
        , 420
        , 440
        , 460
        , 480
        , 500
        , 520
        , 540
        , 560
        , 580
        , 600
        , 868
        , 916
        , 964
        , 1012
        , 1060
        , 1108
        , 1156
        , 1204
        , 1252
        , 1300
        , 932
        , 984
        , 1036
        , 1088
        , 1140
        , 1192
        , 1244
        , 1296
        , 1348
        , 1400
        , 1380
        , 1460
        , 1540
        , 1620
        , 1700
        , 1780
        , 1860
        , 1940
        , 2020
        , 2100
        , 1444
        , 1528
        , 1612
        , 1696
        , 1780
        , 1864
        , 1948
        , 2032
        , 2116
        , 2200
        ]
    )
  ,
    ( tensorContract [] [] (fromList [2, 3] [1, 2, 3, 4, 5, 6]) (fromList [4] [1, 2, 3, 4])
    , fromList
        [2, 3, 4]
        [ 1
        , 2
        , 3
        , 4
        , 2
        , 4
        , 6
        , 8
        , 3
        , 6
        , 9
        , 12
        , 4
        , 8
        , 12
        , 16
        , 5
        , 10
        , 15
        , 20
        , 6
        , 12
        , 18
        , 24
        ]
    )
  ,
    ( tensorContract [0] [0] (fromList [4] [1, 2, 3, 4]) (fromList [4] [1, 2, 3, 4])
    , fromList [] [30]
    )
  ]
-- scalars
more =
  [ ( tensorContract [] [] (fromList [] [x]) (fromList [] [y])
    , fromList [] [x * y]
    )
  | let zs = [-1.5, -1 .. 5]
  , x <- zs
  , y <- zs
  ]

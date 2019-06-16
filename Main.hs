{-# LANGUAGE DeriveTraversable, LambdaCase, OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as B
import qualified Text.Blaze.Svg.Renderer.Utf8 as S
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

main :: IO ()
main = B.putStr . S.renderSvg . doc $ nth 5

data Segment
  = U
  | R
  | D
  | L
  deriving (Eq, Ord, Show)

rotateCCW :: Segment -> Segment
rotateCCW U = R
rotateCCW R = D
rotateCCW D = L
rotateCCW L = U

data T a
  = Leaf a
  | T a :-: T a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

reverse' :: T Segment -> T Segment
reverse' = foldl1 (flip (:-:)) . fmap Leaf


next :: T Segment -> T Segment
next c = c :-: reverse' (fmap rotateCCW c)

nth :: Int -> T Segment
nth n = iterate next (Leaf L) !! n


renderS :: Int -> Segment -> S.Path
renderS l U = S.vr l
renderS l R = S.hr l
renderS l D = S.vr (-l)
renderS l L = S.hr (-l)

render :: T Segment -> S.AttributeValue
render = S.mkPath . (S.m 500 500 >>) . foldl1 (>>) . fmap (renderS 50)

doc :: T Segment -> S.Svg
doc = (S.docTypeSvg ! A.version "1.1") . (S.path ! A.stroke "black" ! A.strokeWidth "1" ! A.fill "none" !) . A.d . render

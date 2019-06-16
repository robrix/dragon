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

rotateCCW U = R
rotateCCW R = D
rotateCCW D = L
rotateCCW L = U

data T a
  = Leaf a
  | T a :-: T a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

reverse' = foldl1 (flip (:-:)) . fmap Leaf


next c = c :-: reverse' (fmap rotateCCW c)

nth n = iterate next (Leaf L) !! n


renderS U = S.vr 50
renderS R = S.hr 50
renderS D = S.vr (-50)
renderS L = S.hr (-50)

render = S.mkPath . (S.m 500 500 >>) . foldl1 (>>) . fmap renderS

doc = (S.docTypeSvg ! A.version "1.1") . (S.path ! A.stroke "black" ! A.strokeWidth "1" ! A.fill "none" !) . A.d . render

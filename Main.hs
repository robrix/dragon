{-# LANGUAGE DeriveTraversable, LambdaCase, OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as B
import           Options.Applicative
import qualified Text.Blaze.Svg.Renderer.Utf8 as S
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

main :: IO ()
main = execParser opts >>= B.putStr . S.renderSvg
  where opts = info (parser <**> helper)
          (  fullDesc
          <> progDesc "Render the dragon curve to SVG"
          <> header "dragon - the best archipelago-looking fractal around")
        parser = run
          <$> option auto (long "length" <> short 'l' <> showDefault <> value 50 <> metavar "INT" <> help "segment length")
          <*> option auto (long "iteration" <> short 'i' <> showDefault <> value 5 <> metavar "INT" <> help "which iteration of the curve to render")
        run l = doc l . nth

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

render :: Int -> T Segment -> S.AttributeValue
render l = S.mkPath . (S.m (l * 10) (l * 10) >>) . foldl1 (>>) . fmap (renderS l)

doc :: Int -> T Segment -> S.Svg
doc l = (S.docTypeSvg ! A.version "1.1") . (S.path ! A.stroke "black" ! A.strokeWidth "1" ! A.fill "none" !) . A.d . render l

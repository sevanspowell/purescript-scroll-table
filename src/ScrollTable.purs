module ScrollTable where

import Prelude

import CSS.Border as Border
import CSS.Color as Color
import CSS.Display as Display
import CSS.Geometry as Geometry
import CSS.Overflow as Overflow
import CSS.Size as Size
import CSS.TextAlign as TextAlign
import Data.Array ((..), length)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query (Action, action)
import Math ((%))
import Web.Event.Event (target)
import Web.HTML.HTMLElement (HTMLElement, fromEventTarget)

foreign import getScrollTop :: HTMLElement -> Int

type VisibleIndices = Array Int

type State =
  { height         :: Int
  , width          :: Int
  , colWidth       :: Int
  , rowCount       :: Int
  , rowHeight      :: Int
  , visibleIndices :: Array Int
  }

calculateVisibleIndices :: State -> Int -> State
calculateVisibleIndices model scrollTop =
  case model of
    { rowHeight, rowCount, height } -> do
      let firstRow = scrollTop / rowHeight
      let visibleRows = (height + 1) / rowHeight
      let lastRow = firstRow + visibleRows

      model { visibleIndices = firstRow..lastRow }

data Query a
  = UserScroll Int a

tableView :: State -> H.ComponentHTML Query
tableView { rowCount, rowHeight, colWidth, visibleIndices } =
  HH.table
    [ CSS.style do Geometry.height $ Size.px (toNumber (rowCount * rowHeight)) ]
    [ HH.tbody_ $ visibleIndices <#> makeRow ]
  where
    makeRow index = do
      let i = toNumber index
      let key = show (i % (toNumber (length visibleIndices)))

      HH.tr
        [ HP.id_ key
        , CSS.style do
          Display.position Display.absolute
          Geometry.top $ Size.px (i * (toNumber rowHeight))
          Geometry.width $ Size.pct (toNumber 100)
        ]
        [ HH.td
            [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
            [ HH.text $ show i ]
        , HH.td
            [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
            [ HH.text $ show (i * 1.0) ]
        , HH.td
            [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
            [ HH.text $ show (i * 100.0) ]
        ]

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState =
      calculateVisibleIndices
        { height: 600
        , width: 900
        , colWidth: 300
        , rowCount: 10000
        , rowHeight: 30
        , visibleIndices: []
        }
        0

    inputMaybe :: forall f a. (a -> Maybe (Action f)) -> a -> Maybe (f Unit)
    inputMaybe f a = action <$> (f a)

    render :: State -> H.ComponentHTML Query
    render st =
      HH.div_
        [ HH.h1
          [ CSS.style do TextAlign.textAlign TextAlign.center ]
          [ HH.text "Scroll Table!!!" ]
        , HH.div_
          [ HH.div
              [ HP.class_ $ ClassName "container"
              , CSS.style do
                  Display.position Display.relative
                  Geometry.height $ Size.px (toNumber st.height)
                  Geometry.width $ Size.px (toNumber st.width)
                  Overflow.overflowX Overflow.hidden
                  Border.border Border.solid (Size.px 1.0) Color.black
              , HE.onScroll $ inputMaybe (\e -> do
                  target <- target e
                  element <- fromEventTarget target
                  scrollTop <- pure $ getScrollTop element
                  pure $ UserScroll scrollTop)
              ]
              [ tableView st ]
          ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      (UserScroll e next) -> do
        _ <- H.modify $ \s -> calculateVisibleIndices s e
        pure next

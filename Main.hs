import Brick
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import System.Environment (getArgs)
import qualified Graphics.Vty as V

type Name = ()
data Interaction = Interaction
newtype UniEditor = UniEditor String

app :: App UniEditor Interaction Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = \s -> attrMap V.defAttr []
          }

handleEvent :: UniEditor -> BrickEvent Name Interaction -> EventM Name (Next UniEditor)
handleEvent s (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ addstring "    " s
handleEvent s (VtyEvent (V.EvKey (V.KChar c) [])) = continue $ addchar c s
handleEvent s (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = suspendAndResume $ saveToFile s
handleEvent s (VtyEvent (V.EvKey V.KBS [])) = continue $ removeLastChar s
handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = continue $ addchar '\n' s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s _ = continue s

filename = head <$> getArgs

saveToFile (UniEditor content) = do
  filename >>= flip writeFile content
  return $ UniEditor content

addstring :: String -> UniEditor -> UniEditor
addstring s (UniEditor content) = UniEditor (content ++ s)

addchar :: Char -> UniEditor -> UniEditor
addchar c (UniEditor content) = UniEditor (content ++ [c])

lastN n s = drop (length s - n) s

removeLastChar :: UniEditor -> UniEditor
removeLastChar (UniEditor content) | lastN 4 content == "    " =
  UniEditor $ iterate init content !! 4
removeLastChar (UniEditor "") = UniEditor ""
removeLastChar (UniEditor content) = UniEditor (init content)

drawUI :: UniEditor -> [Widget Name]
drawUI (UniEditor content) = return $
  withBorderStyle unicode $
  borderWithLabel (str "UniEditor") $
    strWrap (content ++ "|") <=> fill ' '

initialState = UniEditor <$> (filename >>= readFile)

main = initialState >>= defaultMain app

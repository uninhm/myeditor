import Brick
import Brick.Widgets.Border.Style
import Brick.Widgets.Border
import qualified Graphics.Vty as V

import System.Environment (getArgs)
import System.Directory (doesFileExist)

type Name = ()
data Interaction = Interaction
newtype UniEditor = UniEditor [String]

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
  filename >>= flip writeFile (unlines content)
  return $ UniEditor content

addstring :: String -> UniEditor -> UniEditor
addstring s (UniEditor content) = UniEditor $ init content ++ [last content ++ s]

addchar :: Char -> UniEditor -> UniEditor
addchar c = addstring [c]

lastN n s = drop (length s - n) s

removeLastChar :: UniEditor -> UniEditor
removeLastChar (UniEditor content) | lastN 4 (last content) == "    " =
  UniEditor $ init content ++ [iterate init (last content) !! 4]
removeLastChar (UniEditor [""]) = UniEditor [""]
removeLastChar (UniEditor content)
  | null $ last content = UniEditor $ init content
  | otherwise = UniEditor $ init content ++ [init $ last content]

drawUI :: UniEditor -> [Widget Name]
drawUI s = return $
  withBorderStyle unicode $
  borderWithLabel (str "UniEditor") $
    strWrap (unlines contentWithCursor) <=> fill ' '
      where (UniEditor contentWithCursor) = addchar '|' s

initialState fn = do
  fexists <- doesFileExist fn
  if fexists then
    UniEditor . lines <$> readFile fn
  else
    return $ UniEditor [""]

main = filename >>= initialState >>= defaultMain app

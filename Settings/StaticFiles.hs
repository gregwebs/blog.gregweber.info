{-# LANGUAGE TemplateHaskell #-}
module Settings.StaticFiles where

import Prelude (IO, FilePath)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)
import Settings.Development
import Language.Haskell.TH (Q, Exp, Name)
import Data.Default (def)

$(Static.staticFiles staticDir)

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite = if development then Static.staticDevel staticDir
                            else Static.static      staticDir

combineSettings :: CombineSettings
combineSettings = def

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease bandwidth usage. Sample usage (inside a Widget):
--
-- $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets' development combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts' development combineSettings

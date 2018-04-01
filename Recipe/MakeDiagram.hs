{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude hiding (Time)
import Diagrams.Backend.SVG.CmdLine

import Recipe.Demo
import Recipe.Printer

main = mainWith $ drawDiag cupOfTea
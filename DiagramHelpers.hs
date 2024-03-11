{-# LANGUAGE FlexibleContexts #-}

module DiagramHelpers where

import Diagrams.Prelude
import NotionalMachines.Util.Diagrams

compareAlligators as1 as2 e =
    do d1 <- diagramWithError (as1 e)
       d2 <- diagramWithError (as2 e)
       r [d1, d2]
    where r = return
            . hSepRule 0.1
            . map (sized (mkWidth 1) . centerXY)


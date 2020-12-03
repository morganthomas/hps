module Graphics.PS.Util where

import Control.Monad {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}

type Cmd = (String,[String])

run_cmd :: Cmd -> IO ()
run_cmd (c,a) = void (rawSystem c a)

eps_to_pdf_cmd :: FilePath -> Cmd
eps_to_pdf_cmd fn = ("ps2pdf",["-dEPSCrop",fn,replaceExtension fn ".pdf"])

-- | Run ps2pdf to translate an EPS file to a PDF file.
--
-- > eps_to_pdf "/tmp/t.0.eps"
eps_to_pdf :: FilePath -> IO ()
eps_to_pdf = run_cmd . eps_to_pdf_cmd

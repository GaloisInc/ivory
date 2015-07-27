
module Ivory.Artifact.Template
  ( artifactCabalFileTemplate
  , artifactCabalFileTemplate'
  ) where

import Ivory.Artifact
import Text.StringTemplate
import System.FilePath

artifactCabalFileTemplate :: IO FilePath -> FilePath -> [(String, String)]
                          -> Artifact
artifactCabalFileTemplate datadir templatepath attrs =
  artifactCabalFileTemplate' datadir templatepath outputpath attrs
  where
  templatename = takeFileName templatepath
  outputpath = case splitExtension templatename of
    (a, ext) | ext == ".template" -> a
    _ -> templatename

artifactCabalFileTemplate' :: IO FilePath -> FilePath -> FilePath -> [(String, String)]
                          -> Artifact
artifactCabalFileTemplate' datadir templatepath outputpath attrs =
  artifactTransformErrString applyconf af
  where
  af = artifactFile outputpath (fmap (\f -> f </> templatepath) datadir)
  templatename = takeFileName templatepath

  applyconf s =
    let t  = newSTMP s :: StringTemplate String
        t' = setManyAttrib attrs t
    in case checkTemplate t' of
      (Just e, _, _) -> Left (parseErr e)
      (_, Just e, _) -> Left (missingAttrErr e)
      (_, _, Just e) -> Left (missingTemplate e)
      (_, _, _) -> Right (toString t')
  prefix = "Error in " ++ templatename ++ ": "
  parseErr e = prefix ++ "Failed to parse: \n" ++ e
  missingAttrErr es =  prefix ++ "The following attributes are missing:\n"
                    ++ unlines es
  missingTemplate es =  prefix ++ "Failed to lookup invoked templates: \n"
                     ++ unlines es



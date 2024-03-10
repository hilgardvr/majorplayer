module Templates where
import Text.Mustache (automaticCompile, substitute, Template, compileTemplateWithCache, ToMustache)
import Data.Text (Text)
import Text.Mustache.Compile (TemplateCache, cacheFromList)

searchSpace :: [FilePath]
searchSpace = ["./app/templates"]

index :: FilePath
index = "index.mustache"

compiledTemplates :: IO TemplateCache
compiledTemplates = do
    compiledIndexTemplate <- automaticCompile searchSpace index
    let tmpls = sequence [compiledIndexTemplate]
    case tmpls of
        Left err -> error $ show err
        Right ts -> return $ cacheFromList ts

templateOrError :: String -> IO Template
templateOrError tmpl = do
    tc <- compiledTemplates
    tmpl' <- compileTemplateWithCache searchSpace tc tmpl
    case tmpl' of
        Left err -> error $ show err
        Right t' -> return t'


buildIndex :: (ToMustache a) => a -> IO Text
buildIndex d = do
    compiled <- templateOrError index
    return $ substitute compiled d

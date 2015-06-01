import Data.List
import qualified Control.Monad
import Control.Concurrent
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import System.Environment(getArgs)
import Data.List.Utils
import Text.Regex

--Pour chaque charactere de la string, si il est speciaux, le remplacer, sinon on garde le même charactere
remplacerCaracSpeciaux = 
    let speciaux = Map.fromList [('&', "&amp;"),('>',"&gt;"), ('<',"&lt;"), ('\"', "&quot;")]
    in foldr (\c acc ->(Map.findWithDefault [c] c speciaux)++acc) ""

colorierMotsClef motsClef code = 
    let
        delimitateurs = "(&gt;|&lt;|\\(|\\)|;|[[:space:]])"
        reg = mkRegex $ delimitateurs ++ "("  ++ (intercalate "|" motsClef) ++ ")" ++ delimitateurs
    in subRegex reg code "\\1<span class='motClef'>\\2</span>\\3"
    
colorierCommentaires code = subRegex (mkRegex "(//.*$)") code "<span class='commentaire'>\\0</span>"

colorierPreprocesseur motsClef code = 
    let reg = mkRegex $ "#("  ++ (intercalate "|" motsClef) ++ ")[[:space:]]"
    in subRegex reg code "<span class='preprocesseur'>\\0</span>"

transformerHTML code = "<!DOCTYPE html><html><head><style> .motClef { font-weight: bold; color:blue } .preprocesseur { font-weight: bold; color:red } .commentaire, .commentaire * {font-style: italic; color: brown}</style></head><body><pre>"++code++"</pre></body></html>"
            
pipelineFuncs motsClef preproc = [remplacerCaracSpeciaux, colorierMotsClef motsClef, colorierCommentaires ,colorierPreprocesseur preproc, transformerHTML]

    
lireFichiers = mapM readFile
    
pipelineSeq motsClef preproc fichiers = 
    foldl (\codes f-> map f codes) fichiers $ pipelineFuncs motsClef preproc
    
ecrireFichiers mode a b = do
    mapM_ (\(n,f)-> writeFile n f) $ zipWith (\nom fichier->(nom++"."++mode++".html",fichier))  a b
    

    
main = do
    nomFichiers <- fmap lines $ readFile "fichiers.txt"
    motsClef <- fmap lines $ readFile "motsClef.txt"
    preproc <- fmap lines $ readFile "preprocesseur.txt"
    fichiers <- lireFichiers nomFichiers
    ecrireFichiers "sequentiel" nomFichiers $ pipelineSeq motsClef preproc fichiers

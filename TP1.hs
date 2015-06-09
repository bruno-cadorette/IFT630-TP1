import Data.List
import qualified Control.Monad
import qualified Data.Map as Map
import Text.Regex
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Monad.STM
import Control.Monad

--Premiere étape du pipeline, écrit dans le channel 
fstPipe :: (a -> b) -> TChan b -> MVar () -> [a] -> IO ()
fstPipe f chIn m xs = do
	( mapM_(\x-> atomically $ writeTChan chIn $! f x) xs) >> putMVar m ()
	
--Lis les données du channel entrant et les met dans le channel sortant
pipe :: (a -> b) -> TChan a -> TChan b -> MVar () -> MVar () -> IO()
pipe f chIn chOut mIn mOut = iter 
	where iter = do
		val <- fmap f $ atomically $ readTChan chIn
		atomically $ writeTChan chOut val
		estFini <- pipelineEstFini chIn mIn
		if estFini then
			putMVar mOut ()
		else iter
		
--Vérifi si le pipeline est fini
pipelineEstFini channel mIn = do
	rouleEncore <- isEmptyMVar mIn
	estVide <- atomically $ isEmptyTChan channel
	return $ estVide && not rouleEncore
		
--Derniere etape du pipeline
lastPipe::(Show a) => (a->IO()) -> TChan a -> MVar () -> IO()
lastPipe f chIn mIn = iter 
	where iter = do
		r <- atomically $ readTChan chIn
		f r
		estFini <- pipelineEstFini chIn mIn
		unless estFini $ iter

--Crée un pipeline a partir d'une liste de fonction. La dernière fonction doit être séparé car elle n'est pas du même type
creerPipeAsync::(Show a) => [a -> a] -> (a -> IO()) -> [a] -> IO ()
creerPipeAsync (f:fs) final xs = do
	chIn <- atomically newTChan
	m <- newEmptyMVar 
	th <- async $ fstPipe f chIn m xs
	reste <- lkn fs chIn m
	mapM_ wait (th:reste)
	where 
		--lkn fait le lien entre les fonctions avec les channels
		lkn [] chIn mIn = do
			th <- async $ lastPipe final chIn mIn
			return [th]
			
		lkn (f:fs) chIn mIn = do
			(th, channel, fin) <- linkPipeline f chIn mIn
			reste <- (lkn fs channel fin)
			return (th:reste)

		linkPipeline f chIn mIn = do
			chOut <- atomically newTChan
			mOut <- newEmptyMVar
			a <- async $ pipe f chIn chOut mIn mOut
			return (a, chOut, mOut)


--Pour chaque charactere de la string, si il est speciaux, le remplacer, sinon on garde le même charactere
remplacerCaracSpeciaux (nom, code)= 
    let speciaux = Map.fromList [('&', "&amp;"),('>',"&gt;"), ('<',"&lt;"), ('\"', "&quot;")]
    in foldr (\c acc ->(Map.findWithDefault [c] c speciaux)++acc) "" code

colorierMotsClef motsClef (nom,code) = 
    let
        delimitateurs = "(&gt;|&lt;|\\(|\\)|;|[[:space:]])"
        reg = mkRegex $ delimitateurs ++ "("  ++ (intercalate "|" motsClef) ++ ")" ++ delimitateurs
    in subRegex reg code "\\1<span class='motClef'>\\2</span>\\3"
    
colorierCommentaires (nom,code) = subRegex (mkRegex "(//.*$|/\\*(.|[\r\n])*\\*/)") code "<span class='commentaire'>\\0</span>"

colorierPreprocesseur motsClef (nom,code) = 
    let reg = mkRegex $ "#("  ++ (intercalate "|" motsClef) ++ ")[[:space:]]"
    in subRegex reg code "<span class='preprocesseur'>\\0</span>"

transformerHTML (nom,code) = "<!DOCTYPE html><html><head><style> .motClef { font-weight: bold; color:blue } .preprocesseur { font-weight: bold; color:red } .commentaire, .commentaire * {font-style: italic; color: brown}</style></head><body><pre>"++code++"</pre></body></html>"
            
exporterFichier mode (nom,code) = writeFile (nom++"."++mode++".html") code
			
pipelineFuncs motsClef preproc = map ajoutNom $ [remplacerCaracSpeciaux, colorierMotsClef motsClef, colorierCommentaires ,colorierPreprocesseur preproc, transformerHTML]

lireFichiers = mapM readFile

ajoutNom f (a,b)= (a,f(a,b))    

pipelineSeq funcs fichiers = do
    mapM_ (exporterFichier "seq") $ foldl (\codes f-> map f codes) fichiers $ funcs
    

pipelinePar funcs fichiers =
	creerPipeAsync funcs (exporterFichier "par") fichiers
    
	
main = do
    nomFichiers <- fmap lines $ readFile "fichiers.txt"
    motsClef <- fmap lines $ readFile "motsClef.txt"
    preproc <- fmap lines $ readFile "preprocesseur.txt"
    fichiers <- lireFichiers nomFichiers
    pipelinePar (pipelineFuncs motsClef preproc) $ zip nomFichiers fichiers

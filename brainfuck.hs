{-# LANGUAGE DeriveFunctor #-}

import Data.Monoid
import Data.Word
import Data.Char
import Control.Lens
import Control.Applicative hiding ((<|>), many)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Free
import Control.Monad.Trans.Maybe

-- Parsec is not found on Codewars, so the code below is some necessary wheel reinvention.
newtype Parser a = P { unP :: String -> Maybe (a, String) } deriving (Functor)

instance Applicative Parser where
  pure a = P $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  return = pure
  (P f) >>= g = P $ \s -> case (f s) of
                            Nothing -> Nothing
                            Just (a, rest) -> unP (g a) rest

char :: Char -> Parser Char
char c = P $ \s -> case s of
                     [] -> Nothing
                     (c':cs) -> if (c == c') then Just (c, cs) else Nothing

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \s -> case unP p1 s of
                        Nothing -> unP p2 s
                        Just (a, rest) -> Just (a, rest)
infixr 1 <|>

between :: Parser a -> Parser b -> Parser c -> Parser c
between begin end body = do begin; b <- body; end; return b
                             

parse :: Parser a -> String -> Maybe a
parse p s = case unP p s of
              Just (a, "") -> Just a
              _ -> Nothing

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []

bf :: Parser [Instr]
bf = many instr

instr :: Parser Instr 
instr = ('+' `is` Inc)
    <|> ('-' `is` Dec)
    <|> ('<' `is` SL)
    <|> ('>' `is` SR)
    <|> (',' `is` In)
    <|> ('.' `is` Out)
    <|> Block <$> (between (char '[') (char ']') bf)
  where is :: Char -> Instr -> Parser Instr
        is c i = char c *> pure i



-- Pipes is not found on Codewars, so below is also some wheel reinvention.
type ResponseT a = FreeT ((,) a)
type RequestT r = FreeT ((->) r)

yield :: (Monad m) => a -> ResponseT a m ()
yield a = wrap (a, return ())

await :: (Monad m) => RequestT a m a
await = wrap return

bind :: (Monad m) => ResponseT a m () -> RequestT a m b -> MaybeT m b
bind (FreeT mRes) (FreeT mReq) = 
  do res <- lift mRes
     req <- lift mReq
     case req of
          Pure b -> return b
          Free f -> case res of
                         Pure b -> MaybeT $ return Nothing
                         Free (a, rest) -> bind rest (f a)


produce :: (Monad m) => [a] -> ResponseT a m ()
produce = mapM_ yield 
--produce (x:xs) = wrap (x, produce xs)
--produce [] = return ()

-- Datatypes involved in representing a BF machine

data Stream s = s :- Stream s

data Tape a = Tape (Stream a) a (Stream a)

left :: Tape a -> Tape a
left (Tape (l :- ls) a rs) = Tape ls l (a :- rs)

right :: Tape a -> Tape a
right (Tape ls a (r :- rs)) = Tape (a :- ls) r rs

data Instr = SR | SL | Inc | Dec | In | Out | Block [Instr] deriving (Show)

type BF a = RequestT Char (RWS () (Endo String) (Tape Word8)) a

initial :: Tape Word8
initial = Tape zeros 0 zeros
        where zeros = 0 :- zeros


-- Who wouldn't love some lenses?
cur :: Lens' (Tape a) a
cur = lens getter setter
  where getter (Tape _ a _) = a
        setter (Tape l a r) a' = Tape l a' r

dString :: Iso' String (Endo String)
dString = iso (Endo . (++)) (flip appEndo [])

wordChar :: Iso' Word8 Char
wordChar = iso (chr . fromIntegral) (fromIntegral . ord)



-- Real interpretation
interp :: [Instr] -> BF ()
interp instrs = mapM_ interp' instrs


interp' :: Instr -> BF ()
interp' SR = modify right
interp' SL = modify left
interp' Inc = cur += 1
interp' Dec = cur -= 1
interp' Out = do
   w <- use cur
   tell $ w ^. wordChar . to (:[]) . dString
interp' In = do
   c <- await
   cur .= (c ^. from wordChar)
interp' b@(Block instrs) = do
   w <- use cur
   unless (w == 0) $ interp instrs >> interp' b


executeString :: String -> String -> Maybe String
executeString source input = do ins <- parse bf source
                                let (a, w) = evalRWS (runMaybeT $ produce input `bind` interp ins) () initial
                                a *> (return $ w ^. from dString)





-- examples
helloWorldBF = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+."
numbersBF    = ",>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]"

hw = executeString helloWorldBF "" -- Just "Hello World!"
nb = executeString numbersBF [chr 10] -- Just "1, 1, 2, 3, 5, 8, 13, 21, 33, 55"
    

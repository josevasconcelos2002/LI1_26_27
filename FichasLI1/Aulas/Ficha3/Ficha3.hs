type Matriz a = [[a]]

type Posicao = (Int,Int)

data Direcao = Norte | Este | Sul | Oeste deriving (Eq, Show)

type Dimensao = (Int,Int)



dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz ((x:xs):h) = (1+length h, length (x:xs))
data Direcao = Norte | Sul | Este | Oeste deriving (Show, Eq)

type Ponto = (Double, Double)

move :: Ponto -> Direcao -> Ponto
move (x,y) d
            | d == Norte = (x, y+1)
            | d == Sul = (x, y-1)
            | d == Este = (x+1 ,y)
            | d == Oeste = (x-1,y)
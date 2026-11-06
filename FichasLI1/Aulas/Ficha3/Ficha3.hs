type Matriz a = [[a]]

{-
Uma posição numa matriz é dada como um par (linha,coluna). As coordenadas são dois
números naturais e começam com (0,0) no canto superior esquerdo, com as linhas a incrementar
para baixo e as colunas a incrementar para a direita
-}
type Posicao = (Int,Int)


-- A dimensão de uma matrix é dada como um par (número de linhas, número de colunhas).
type Dimensao = (Int,Int)


-- Uma direção é dada pelos quatro pontos cardeais
data Direcao = Norte | Este | Sul | Oeste deriving (Eq, Show)



-- Funções não-recursivas

-- 2) Defina uma função que calcula a dimensão de uma matriz.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz ((x:xs):h) = (1+length h, length (x:xs))


-- 3) Defina uma função que verifica se uma posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (l,c) matriz 
                                | l<= fst (dimensaoMatriz matriz) && c<= snd (dimensaoMatriz matriz) = True
                                | otherwise = False 


-- 4)  Defina uma função que acrescenta uma linha no início de uma matriz
acrescentaLinhaInicioMatriz :: [a] -> Matriz a -> Matriz a
acrescentaLinhaInicioMatriz [] m = m
acrescentaLinhaInicioMatriz linha (x:h) = (linha:x:h)


-- 5)  Defina uma função que acrescenta uma linha no fim de uma matriz
acrescentaLinhaFimMatriz :: [a] -> Matriz a -> Matriz a
acrescentaLinhaFimMatriz [] m = m
acrescentaLinhaFimMatriz linha (x:h) = (x:h) ++ [linha]


{- 
6) Considere uma janela retangular com origem no canto superior esquerdo definida como
uma matriz. Defina uma função que move uma posição uma unidade numa direcção e
que garanta que o movimento não se desloca para fora de uma janela.. A função recebe
a dimensao da janela.
-}

movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (nlinhas, ncolunas) d (l,c)
                                            | d == Norte && l-1 >= 0 = (l-1,c)
                                            | d == Sul && l+1 <= nlinhas = (l+1,c)
                                            | d == Este && c+1 <= ncolunas = (l,c+1)
                                            | d == Oeste && c-1 >= 0 = (l,c-1) 
                                            | otherwise = (l,c)


-- Funções Recursivas


-- 1) Defina uma função que aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] (l,c) = (l,c)
moveDirecoesPosicao (d:ds) (l,c)
                                | d == Norte = moveDirecoesPosicao ds (l-1,c)
                                | d == Sul = moveDirecoesPosicao ds (l+1,c)
                                | d == Este = moveDirecoesPosicao ds (l,c+1)
                                | d == Oeste = moveDirecoesPosicao ds (l,c-1)


-- 2) Defina uma função que aplica uma mesma movimentação a uma lista de posições.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes d ((l,c):h)
                                | d == Norte = [(l-1,c)] ++ moveDirecaoPosicoes d h
                                | d == Sul = [(l+1,c)] ++ moveDirecaoPosicoes d h
                                | d == Este = [(l,c+1)] ++ moveDirecaoPosicoes d h
                                | d == Oeste = [(l,c-1)] ++ moveDirecaoPosicoes d h
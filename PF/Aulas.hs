dobro :: Float -> Float 
dobro x = (2 * x)

omaior :: (Float, Float) -> Float 
omaior (x,y) = if x>y then x else y

type Coordenada = (Float, Float)

data Cor = Amarelo | Verde | Vermelho | Azul | Cinza
    deriving (Show)

data PontoC = PC Coordenada Cor
    deriving (Show)

distOrigem::PontoC -> Float
distOrigem (PC (x,y) c) = sqrt(x^2 + y^2)

fria :: Cor -> Bool
fria Azul = True
fria Verde = True
fria Cinza = True
fria x = False


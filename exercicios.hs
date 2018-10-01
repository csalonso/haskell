module Exericios where

--Exercicios do livro: Haskell, Uma Introducao a Programacao Funcional - Alexandre Garcia de Oliveira

--2.1 A
listaA :: Int -> [Int]
listaA x = [11 ^ x | x <- [1..x]]

--2.1 B
listaB  :: Int -> [Int]
listaB x = [x |  x <- [1..x], x `mod` 4 /= 0]

--2.1 C
listaC :: Char -> [String]
listaC x = ["A" ++ [x] ++ "B" ++ "b" | x <- ['a'..x]]

--2.2 Verificar se tamanho de String é par. retornar Bool.
verificaTamanho :: String -> Bool
verificaTamanho x = mod (length x) 2 == 0

--2.3 Receber vetor de String e retornar todos os elementos em ordem reversa
retornaReverso :: [String] -> [String]
retornaReverso x = reverse [reverse (x !! i) | i <- [0..length x - 1]]

--2.4 Receber uma lista de Strings e retornar uma lista com o tamanho de cada String. Tamanhos pares não devem ser exibidos
retornaTamanho :: [String] -> [Int]
retornaTamanho x = [length (x !! i) | i <- [0..length x - 1], length (x !! i) `mod` 2 /= 0]

--2.5 Escrever função head como composicao de duas outras
headComposto :: String -> Char
headComposto x = last $ reverse x

--2.6 Receber String, retornar True caso seja palindromo e false caso não seja
verificaPalindromo :: String -> Bool
verificaPalindromo x = x == reverse x

--2.7 Receber um inteiro e retornar Tupla contendo o dobro, o triplo, o quadruplo e o quituplo do numero recebido
retornaTupla :: Int -> (Int, Int, Int, Int)
retornaTupla x = (x*2, x*3, x*4, x*5) 

--3.1 Criar um tipo Pergunta e desenvolver as sguintes funções
data Pergunta = SIM | NAO deriving Show

--Recebe uma Pergunta e retorna 0 para Nao e 1 para Sim
pergNum :: Pergunta -> Int
pergNum NAO = 0
pergNum SIM = 1

--Recebe uma lista de Perguntas e recebe uma lista de 0s e 1s correspondente aos contrutores da lista
listPergs :: [Pergunta] -> [Int]
listPergs x = map pergNum x

-- Funcao AND lógico, SIM = True e NAO = False
and' :: Pergunta -> Pergunta -> Bool
and' SIM SIM = True
and' _ _ = False

-- Funcao OR lógico, SIM = True e NAO = False
or' :: Pergunta -> Pergunta -> Bool
or' SIM _ = True
or' _ SIM = True
or' _ _ = False

-- Funcao NOT lógico, SIM = True e NAO = False
not' :: Pergunta -> Pergunta -> Bool
not' NAO NAO = True
not' _ _ = False

--3.2 Criar um tipo Temperatura com os valores Celsius, Farenheit e Kelvin como construtores e implementar as funções
data Temperatura = Celsius | Kelvin | Farenheit deriving Show

--Recebe um valor double e uma Temperatura e converte para Celsius
converterCelsius :: Double -> Temperatura -> Double
converterCelsius x Farenheit = (x - 32) / 1.8
converterCelsius x Kelvin = x - 273

--Recebe um valor double e uma Temperatura e converte para Farenheit
converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit x Celsius = 1.8 * x + 32
converterFarenheit x Kelvin = (x - 273) * 1.8 + 32

--Recebe um valor double e uma Temperatura e converte para Kelvin
converterKelvin :: Double -> Temperatura -> Double
converterKelvin x Celsius = x + 273
converterKelvin x Farenheit = ((x - 32) / 1.8) + 273

--3.3 Implementar Pedra, Papel e Tesoura
data Jokenpo = Pedra | Papel | Tesoura | Empate deriving Show

playJokenpo :: Jokenpo -> Jokenpo -> Jokenpo
playJokenpo Pedra Pedra = Empate
playJokenpo Pedra Papel = Papel
playJokenpo Pedra Tesoura = Pedra
playJokenpo Papel Pedra = Papel
playJokenpo Papel Papel = Empate
playJokenpo Papel Tesoura = Tesoura
playJokenpo Tesoura Pedra = Pedra
playJokenpo Tesoura Papel = Tesoura
playJokenpo Tesoura Tesoura = Empate

--3.4 Receba uma String e retorne ela sem vogais (Maiusculas ou Minusculas) usando list compreenshion
removeVogal :: String -> String
removeVogal x = [(x !! i) | i <- [0..length x - 1], (x !! i) /= 'a', (x !! i) /= 'e', (x !! i) /= 'i', (x !! i) /= 'o', (x !! i) /= 'u', 
                                                    (x !! i) /= 'A', (x !! i) /= 'E', (x !! i) /= 'I', (x !! i) /= 'O', (x !! i) /= 'U']

--3.5 Criar um tipo Unidade Imperial contendo Inch, Yard e Foot e implementar as funcoes converterMetros e converterImperial
data UnidadeImperial = Inch | Yard | Foot deriving Show

--funcao para converter unidade imperial para metros
converterMetros :: UnidadeImperial -> Double -> Double
converterMetros Inch x = 0.0254 * x
converterMetros Yard x = 0.9144 * x
converterMetros Foot x = 0.3048 * x

--funcao para converter metro em uma unidade imperial
converterImperial x Inch = x / 0.0254
converterImperial x Yard = x / 0.9144
converterImperial x Foot = x / 0.3048

--3.6 Criar um tipo Mes com todos os meses do ano e implementar as funcoes
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving (Show, Enum)

data Hemisferio = Norte | Sul deriving Show
data Estacao = Primavera | Verao | Outono | Inverno deriving Show
--retorna o numero de dias que cada mes possui (Fevereiro = 28)
checaFim :: Mes -> Int
checaFim Janeiro = 31
checaFim Fevereiro = 28
checaFim Marco = 31
checaFim Abril = 30
checaFim Maio = 31
checaFim Junho = 30
checaFim Julho = 31
checaFim Agosto = 31
checaFim Setembro = 30
checaFim Outubro = 31
checaFim Novembro = 30
checaFim Dezembro = 31

--recebe um mes e retorna o proximo
prox :: Mes -> Mes
prox x = succ x

--recebe um mes e um hemisferio e retorna uma estacao
estacao :: Mes -> Hemisferio -> Estacao
estacao Janeiro Sul = Verao
estacao Fevereiro Sul = Verao
estacao Marco Sul = Verao
estacao Abril Sul = Outono
estacao Maio Sul = Outono
estacao Junho Sul = Outono
estacao Julho Sul = Inverno
estacao Agosto Sul = Inverno
estacao Setembro Sul = Inverno
estacao Outubro Sul = Primavera
estacao Novembro Sul = Primavera
estacao Dezembro Sul = Primavera
estacao Janeiro Norte = Inverno
estacao Fevereiro Norte = Inverno
estacao Marco Norte = Inverno
estacao Abril Norte = Primavera
estacao Maio Norte = Primavera
estacao Junho Norte = Primavera
estacao Julho Norte = Verao
estacao Agosto Norte = Verao
estacao Setembro Norte = Verao
estacao Outubro Norte = Outono
estacao Novembro Norte = Outono
estacao Dezembro Norte = Outono

--3.7 Receber String, retornar True caso seja palindromo e false caso não seja
verificaPalindromo2 :: String -> Bool
verificaPalindromo2 x = x == reverse x

--3.8 Receber lista de inteiros, eliminar pares, ímpares multiplos de 7 e negativos. Retornar em ordem reversa
retornaInteirosReverso :: [Int] -> [Int]
retornaInteirosReverso x = [(x !! i) | i <- [0..length x - 1], (x !! i) `mod` 2 /= 0, (x !! i) `mod` 7 /= 0, (x !! i) > 0]

--3.9 Receber 3 Strings e implementar a funcao retornaTupla deve retornar uma Tupla contendo as Strings em sua ordem reversa
retornaTupla2 :: String -> String -> String -> (String, String, String)
retornaTupla2 x y z = (reverse x, reverse y, reverse z)

--3.10 Implemente a funcao revNum, que deve receber uma String s e um Int n, retornando as n primeiras letras da String em ordem reversa e o restante em ordem normal
revNum :: String -> Int -> String
revNum s n = reverse [(s !! i) | i <- [0..n - 1]] ++ [(s !! j) | j <- [n..length s - 1]]

--3.11

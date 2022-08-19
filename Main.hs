{- Henrique Levandoski Richa -}

{- 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um
inteiro uma unidade maior que a entrada. -}
soma1 :: Int -> Int
soma1 x = ((+) x 1)

{- 2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva
sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. -}
sempreQue x = 0

{- 3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com
precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. -}
treco :: Double -> Double -> Double -> Double
treco x y z = ((x + y) * z)

{- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números
inteiros. -}
resto :: Int -> Int -> Int
resto x y = mod x y

{- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores
monetários. -}
precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior w x y z = 
  if w > x && w > y && w > z
    then w
  else if x > y && x > z
    then x
  else if y > z
    then y
  else z

{- 5.5. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva
uma função em Haskell que devolva a soma dos componentes de um par de inteiros. -}
par :: (Int, Int) -> Int
par (numero1, numero2) = ((+)numero1 numero2)
    
{- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto
de dois números inteiros for ímpar. -}
multiplica :: Int -> Int -> Int
multiplica x y = ((*)x y)

impar :: Int -> Int -> Bool
impar numero1 numero2 = 
  if mod (multiplica numero1 numero2) 2 == 0 
    then True 
  else False

{- 7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado
da equação 𝑥^2 + y/2 + z. -}
equacao :: Double -> Double -> Double -> Double
equacao x y z = ((x^2) + (y/2) + z)

{- 8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima
um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link:
Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos
(cuidadospelavida.com.br). Observe que este diagnóstico é meramente estatístico e não
tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas.
Todo e qualquer diagnóstico deve ser feito por um profissional médico.  -}
imc :: Double -> Double -> Double
imc peso altura = ((/) peso (altura^2))

diagnostico :: Double -> Double -> String
diagnostico peso altura = 
  if (imc peso altura) < 17 then "Muito abaixo do peso"
  else if (imc peso altura) >= 17 && (imc peso altura) <= 18.49 then "Abaixo do peso"
  else if (imc peso altura) >= 18.50 && (imc peso altura) <= 24.99 then "Peso Normal"
  else if (imc peso altura) >= 25 && (imc peso altura) <= 29.99 then "Sobrepeso"
  else if (imc peso altura) >= 30 && (imc peso altura) <= 34.99 then "Obesidade Leve"
  else if (imc peso altura) >= 35 && (imc peso altura) <= 39.99 then "Obesidade Severa"
  else "Obesidade Morbida"

{- 9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4
𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100
𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto. -}
bissexto :: Int -> Bool
bissexto ano = 
  if ((mod ano 4) == 0 && not((mod ano 100)==0) && not((mod ano 400)==0))
    then True
  else False

main = do
  let a = 5
  let b = 10

  putStr "Func. 1: entrada:2; resultado: "
  print (soma1 5)

  putStrLn ""

  putStr "Func. 2: entrada:5; resultado: "
  print (sempreQue 5)

  putStrLn ""

  putStr "Func. 3: entrada:2 2 3; resultado: "
  print(treco 2.0 2.0 3.0)

  putStrLn ""

  putStr "Func. 4: entrada:7 4; resultado: "
  print(resto 7 4)
  putStr "Func. 4: entrada:8 4; resultado: "
  print(resto 8 4)

  putStrLn ""

  putStr "Func. 5: entrada 3.5 1.4 6.4 5.2; resultado: "
  print(precoMaior 3.5 1.4 6.4 5.2)

  putStrLn ""

  putStr "Func. 5.5: entrada 5 5; resultado: "
  print(par (5, 5))

  putStrLn ""

  putStr "Func. 6: entrada 4 3; resultado: "
  print(impar 4 3)
  putStr "Func. 6: entrada 5 3; resultado: "
  print(impar 5 3)

  putStrLn ""

  putStr "Func. 7: entrada 3 6 5; resultado: "
  print(equacao 3 6 5)

  putStrLn ""

  putStr "Func.8: entrada 45 1.75; resultado: "
  print(diagnostico 45 1.75)
  putStr "Func.8: entrada 55 1.75; resultado: "
  print(diagnostico 55 1.75)
  putStr "Func.8: entrada 60 1.75; resultado: "
  print(diagnostico 60 1.75)
  putStr "Func.8: entrada 80 1.75; resultado: "
  print(diagnostico 80 1.75)
  putStr "Func.8: entrada 95 1.75; resultado: "
  print(diagnostico 95 1.75)
  putStr "Func.8: entrada 110 1.75; resultado: "
  print(diagnostico 110 1.75)
  putStr "Func.8: entrada 130 1.75; resultado: "
  print(diagnostico 130 1.75)

  putStrLn ""

  putStr "Func. 9: entrada 2000; resultado: "
  print(bissexto 2000)
  putStr "Func. 9: entrada 2022; resultado: "
  print(bissexto 2022)
  putStr "Func. 9: entrada 2024; resultado: "
  print(bissexto 2024)
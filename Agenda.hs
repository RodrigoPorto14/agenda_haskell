data Contato = Contato{ nome :: [Char],telefone :: [Char],endereco :: [Char],relacao :: [Char]} deriving(Show)

type Agenda = [Contato]																						  

toString :: Contato -> [Char]
toString c = nome c ++ "  " ++ telefone c ++ "  " ++ endereco c ++ "  " ++ relacao c ++ "\n"

comparaString :: [Char] -> [Char] -> Bool
comparaString [] [] = True
comparaString a [] = True
comparaString [] b = False
comparaString (prim_a:restante_a) (prim_b:restante_b)
	|	(prim_a == prim_b) = comparaString restante_a restante_b
	|	otherwise = False

insereContato :: Agenda -> Contato -> Agenda
insereContato [] novo_contato = [novo_contato]
insereContato (prim:restante) novo_contato
	|	(nome prim == nome novo_contato) = novo_contato : restante
	|   otherwise = prim : insereContato restante novo_contato

removeContato :: Agenda -> [Char] -> Agenda
removeContato [] n = []
removeContato a n = filter (\c -> (nome c /= n)) a

buscaContato :: Agenda -> [Char] -> Contato
buscaContato [] n = Contato "Contato nao encontrado" " " " " " "
buscaContato (prim:restante) n
	|	(comparaString (nome prim) n) = prim
	|	otherwise = buscaContato restante n

imprimeAgenda :: Agenda -> [Char]
imprimeAgenda [] = ""
imprimeAgenda (prim:restante) = toString prim ++ imprimeAgenda restante

a :: Agenda
a = [Contato "Rodrigo" "97235-2167" "Rio Bonito" "Eu",
     Contato "Eliane" "97148-2326" "Rio Bonito" "Mae"]

main :: IO()
main = do
putStrLn (imprimeAgenda a)
let a2 = insereContato a (Contato "Guilherme" "99780-4344" "Rio Bonito" "Pai") 
putStrLn (imprimeAgenda a2)
let a3 = removeContato a2 "Eliane" 
putStrLn (imprimeAgenda a3)
let c = buscaContato a3 "Rodri"
putStrLn (toString c)
let c = buscaContato a3 "Rodrigol"
putStrLn (toString c)






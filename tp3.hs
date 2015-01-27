
import Graphics.Gloss
  
--Q1
type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
                
motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (s:mot) = (r s)++(motSuivant r mot)


motSuivant' :: Regles -> Mot -> Mot
motSuivant' r mot = concat [r x | x<-mot]


motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' r mot = concatMap r mot

--Q2

floKoch :: Symbole -> Mot
floKoch 'F' = "F-F++F-F"
floKoch '+' = "+"
floKoch '-' = "-"

{-
length (motSuivant floKoch "F")
 8
length (motSuivant floKoch "F-F++F-F") 
 36
length (motSuivant floKoch "F-F++F-F-F-F++F-F++F-F++F-F-F-F++F-F") 
 148

length (motSuivant' floKoch "F") 
 8
length (motSuivant' floKoch "F-F++F-F") 
 36
length (motSuivant' floKoch "F-F++F-F-F-F++F-F++F-F++F-F-F-F++F-F") 
 148

length (motSuivant'' floKoch "F") 
 8
length (motSuivant'' floKoch "F-F++F-F") 
 36
length (motSuivant'' floKoch "F-F++F-F-F-F++F-F++F-F++F-F-F-F++F-F") 
 148
-}

--Q3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r =
  let suiv = motSuivant r a in
  if a == suiv then   
    [a]
  else
    a:(lsysteme suiv r)
    
{-
take 3 (lsysteme "F" floKoch)
 ["F","F-F++F-F","F-F++F-F-F-F++F-F++F-F++F-F-F-F++F-F"]
-}


type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

--Q4

etatInitial :: Config -> EtatTortue
etatInitial (x,_,_,_,_) = x
                          
longueurPas :: Config -> Float
longueurPas (_,x,_,_,_) = x

facteurEchelle :: Config -> Float
facteurEchelle (_,_,x,_,_) = x

angle :: Config -> Float
angle (_,_,_,x,_) = x

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,x) = x

--Q5

avance :: Config -> EtatTortue -> EtatTortue
avance cfg ((x,y), cap) =
  let d = longueurPas cfg in
  ( ( (x + d*cos(cap)),(y + d*sin(cap)) ), cap)
  
{-
test

cfgtest :: Config
cfgtest = (((2.0,3.0), 0.0),1.0,1.0,1.0,['o','k'])

avance cfgtest (etatInitial cfgtest)
 ((3.0,3.0),0.0)
-}

--Q6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche cfg ((x,y), cap) = ((x,y), cap + (angle cfg))


tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite cfg ((x,y), cap) = ((x,y), cap - (angle cfg))

{-
test

tourneAGauche cfgtest (etatInitial cfgtest)
 ((2.0,3.0),-1.0)
tourneADroite cfgtest (etatInitial cfgtest)
 ((2.0,3.0),1.0)
-}

--Q7

filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue _ [] = []
filtreSymbolesTortue cfg (y:ys) =
  if y `elem` symbolesTortue cfg then
    y:(filtreSymbolesTortue cfg ys)
  else
    filtreSymbolesTortue cfg ys
    
{-    
Tests
> filtreSymbolesTortue cfgtest "oh"
 "o"
> filtreSymbolesTortue cfgtest "ok"
 "ok"
> filtreSymbolesTortue cfgtest ""
""
> filtreSymbolesTortue cfgtest "c"
""
-}
    
    
type EtatDessin = (EtatTortue, Path)

--Q8

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole cfg (s, p) 'F' =
  let s2 = avance cfg s in
  (s2, (fst s2):p)    
interpreteSymbole cfg (s, p) '+' =
  let s2 = tourneAGauche cfg s in
  (s2, (fst s2):p)
interpreteSymbole cfg (s, p) '-' =
  let s2 = tourneADroite cfg s in
  (s2, (fst s2):p)
interpreteSymbole _ _ _ = error "symbole non accepté"

--Q9

interpreteMot :: Config -> Mot -> Picture
interpreteMot cfg mot =
  let i = etatInitial cfg in
 line (interpreteMot_rec cfg (i,[fst i]) (filtreSymbolesTortue cfg mot))
  where
    interpreteMot_rec _ _ [] = []
    interpreteMot_rec cfg d (x:xs) =
      if //TODOOOO


dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F"

main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin
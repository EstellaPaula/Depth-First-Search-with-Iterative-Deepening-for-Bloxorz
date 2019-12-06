{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState 

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type SwitchUpdate = (Position, Bool)

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = HardTile | SoftTile | Block | Switch | EmptySpace | WinningTile
    deriving (Eq, Ord)

instance Show Cell where
    show HardTile = [hardTile]
    show SoftTile = [softTile]
    show Block = [block]
    show Switch = [switch]
    show EmptySpace = [emptySpace]
    show WinningTile = [winningTile]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = L { playPosition ::(Position, Position)
                ,up :: Bool
                ,over :: Bool
                ,win :: Bool
                ,coloane :: Int
                ,randuri :: Int
                ,switches :: [(SwitchUpdate, [Position])]
                ,harta :: A.Array Position Cell
                } deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where
    show (L player _ ov w col rand _ h) = "\n" ++
     unlines [foldl (++) "" [if (i,j) == (fst player) || (i,j) == (snd player) then show Block
        else if (w == False && ov == True && i == rand && j == col) then (show $h A.! (i,j)) ++ "\nGame Over"
        else if (w == True && ov == True && i == rand && j == col) then (show $h A.! (i,j)) ++ "\nCongrats! You won!"
        else (show $h A.! (i,j)) | i <-[0..rand]] | j <- [0..col]]

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel coltDreapta pozPlayer = L { playPosition = ((snd pozPlayer, fst pozPlayer), (snd pozPlayer, fst pozPlayer))
                                        ,up = True
                                        ,over = False
                                        ,win = False
                                        ,coloane = fst coltDreapta
                                        ,randuri = snd coltDreapta
                                        ,switches = [] 
                                        ,harta = A.array ((0,0), (snd coltDreapta , fst coltDreapta))
                                        [((i, j), cell) | i <- [0..(snd coltDreapta)], j <- [0..(fst coltDreapta)], let cell = EmptySpace]
                                    }

-- returns type of cell
getTile :: (Position,Position) -> Level -> [Cell]
getTile (x,y) (L _ _ _ _ _ _ _ h) = [h A.! x, h A.! y]

getTileOne :: Position -> Level -> Cell
getTileOne x (L _ _ _ _ _ _ _ h) = h A.! x


{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile 'H' pos (L player u ov w col rand sw h)
    |otherwise = L { playPosition = player
                    ,up = u
                    ,over = ov
                    ,win = w
                    ,coloane = col
                    ,randuri = rand
                    ,switches = sw
                    ,harta = h A.// [((snd pos, fst pos), HardTile)] 
                    }

addTile 'S' pos (L player u ov w col rand sw h)
    |otherwise = L { playPosition = player
                    ,up = u
                    ,over = ov
                    ,win = w
                    ,coloane = col
                    ,randuri = rand
                    ,switches = sw
                    ,harta = h A.// [((snd pos, fst pos), SoftTile)] 
                    }

addTile 'W' pos (L player u ov w col rand sw h)
    |otherwise = L { playPosition = player
                    ,up = u
                    ,over = ov
                    ,win = w
                    ,coloane = col
                    ,randuri = rand
                    ,switches = sw
                    ,harta = h A.// [((snd pos, fst pos), WinningTile)] 
                    }

addTile _ _ _ = undefined

addTileActivate :: Level -> Position -> Level
addTileActivate (L player u ov w col rand sw h) pos = L { playPosition = player
                                                         ,up = u
                                                         ,over = ov
                                                         ,win = w
                                                         ,coloane = col
                                                         ,randuri = rand
                                                         ,switches = sw
                                                         ,harta = h A.// [((snd pos, fst pos), HardTile)] 
                                                    }

addTileDeactivate :: Level -> Position -> Level
addTileDeactivate (L player u ov w col rand sw h) pos = L { playPosition = player
                                                            ,up = u
                                                            ,over = ov
                                                            ,win = w
                                                            ,coloane = col
                                                            ,randuri = rand
                                                            ,switches = sw
                                                            ,harta = h A.// [((snd pos, fst pos), EmptySpace)] 
                                                        }

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos listCells (L player u ov w col rand sw h) = L { playPosition = player
                                                             ,up = u
                                                             ,over = ov
                                                             ,win = w
                                                             ,coloane = col
                                                             ,randuri = rand
                                                             ,switches = (((snd pos, fst pos), False), listCells) : sw 
                                                             ,harta = h A.// [((snd pos, fst pos), Switch)] 
                                                        }

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

-- returns position of switch
getSwitch :: (Position, Position) -> Level -> Position
getSwitch (x, y) level
    |getTileOne x level == Switch = x
    |otherwise = y

getSwitchCells :: Position -> Level -> [Position]
getSwitchCells pos (L _ _ _ _ _ _ sw _) = snd $ head $ [x | x<- sw, (fst (fst x)) == pos]

-- return switch bool value
needOfActivate :: Position -> Level -> Bool
needOfActivate pos (L _ _ _ _ _ _ sw _) = snd $ fst $ head $ [x | x<- sw, (fst (fst x)) == pos]

updateStateSwitch ::Position -> Bool -> Level -> Level
updateStateSwitch pos bool (L player u ov w col rand sw h) = L { playPosition = player
                                                                        ,up = u
                                                                        ,over = ov
                                                                        ,win = w
                                                                        ,coloane = col
                                                                        ,randuri = rand
                                                                        ,switches = [x | x <- sw, (fst (fst x)) /= pos] 
                                                                        ++ [(x,y) | (z, t) <- sw, (fst z) == pos, let x = (pos, bool), let y = t]
                                                                        ,harta = h
                                                                    }


activateSwitchCells :: Position -> Level -> Level
activateSwitchCells pos level = case (needOfActivate pos level) of
    True ->  updateStateSwitch pos False (foldl addTileDeactivate level (getSwitchCells pos level))          --deactivateCells
    False -> updateStateSwitch pos True (foldl addTileActivate level (getSwitchCells pos level))            --activateCells


activate :: Cell -> Level -> Level
activate HardTile level = level
-- soft tile block vertical => loss
activate SoftTile (L player True _ _ col rand sw h) = L { playPosition = player
                                                                        ,up = True
                                                                        ,over = True
                                                                        ,win = False
                                                                        ,coloane = col
                                                                        ,randuri = rand
                                                                        ,switches = sw
                                                                        ,harta = h
                                                                    }
-- soft tile block orizontal => nothing
activate SoftTile level = level
-- winning tile block vertical => win
activate WinningTile (L player True _ _ col rand sw h) = L { playPosition = player
                                                                        ,up = True
                                                                        ,over = True
                                                                        ,win = True
                                                                        ,coloane = col
                                                                        ,randuri = rand
                                                                        ,switches = sw
                                                                        ,harta = h
                                                                    }
-- winning tile block vertical => win
activate WinningTile level = level
-- switch -> check if activated -> activate / deactivate
activate Switch level@(L player _ _ _ _ _ _ _) = activateSwitchCells (getSwitch player level) level

--any other tile won tchange the outcome
activate EmptySpace (L player u _ _ col rand sw h) = L { playPosition = player
                                ,up = u
                                ,over = True
                                ,win = False
                                ,coloane = col
                                ,randuri = rand
                                ,switches = sw
                                ,harta = h
                             }
activate _ level = level

activateHelper :: Level -> Cell -> Level
activateHelper level cell = activate cell level 

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}


{-
    Va returna coordonatele corespunzatoare jucatorului dupa efecturea unei mutari
-}

getStangaDreapta :: (Position, Position) -> (Position, Position)
getStangaDreapta (x,y) = if fst x < fst y then (x,y) else (y, x)

getJosSus :: (Position, Position) -> (Position, Position)
getJosSus (x, y) = if snd x > snd y then (x, y) else (y, x)

moveHelperUp :: Directions -> (Position, Position) -> (Position, Position)
moveHelperUp North (x, y) = ((fst x, snd x - 1) , (fst y, snd y - 2))
moveHelperUp South (x, y) = ((fst x ,snd x + 1), (fst y, snd y + 2))
moveHelperUp West (x, y) = ((fst x - 1, snd x), (fst y - 2, snd y))
moveHelperUp East (x, y) = ((fst x + 1, snd x), (fst y + 2, snd y))


moveHelperH :: Directions -> (Position, Position) -> (Position, Position)
moveHelperH North (x, y) = ((fst x, snd x - 1) , (fst y, snd y - 1))
moveHelperH South (x, y) = ((fst x, snd x + 1) , (fst y, snd y + 1))
moveHelperH West (x, y) = ((fst x - 1, snd x) , (fst y - 2, snd y))
moveHelperH East  (x, y)  = ((fst x + 2, snd x) , (fst y + 1, snd y))

moveHelperV :: Directions -> (Position, Position) -> (Position, Position)
moveHelperV North (x, y) = ((fst x,snd x - 2), (fst y, snd y - 1))
moveHelperV South (x, y) = ((fst x, snd x + 1), (fst y, snd y + 2))
moveHelperV West (x, y) = ((fst x - 1, snd x), (fst y - 1, snd y))
moveHelperV East (x, y) =  ((fst x + 1, snd x), (fst y + 1, snd y))

--return true if block is vertical on board
isV :: (Position, Position) -> Bool
isV (x,y) = if fst x == fst y then True else False

move :: Directions -> Level -> Level
move direction level@(L player u ov w col rand sw h) = case direction of
    -- --up block
    North -> if u == True then  foldl activateHelper ( L { playPosition = moveHelperUp direction player
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperUp direction player) level)
            --vertical block
            else if isV player then foldl activateHelper ( L { playPosition = moveHelperV direction (getJosSus player)
                                                ,up = True
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperV direction (getJosSus player)) level)
                --horizontal block
                else foldl activateHelper ( L { playPosition = moveHelperH direction (getStangaDreapta player)
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperH direction (getStangaDreapta player)) level)
    South -> if u == True then  foldl activateHelper ( L { playPosition = moveHelperUp direction player
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperUp direction player) level)
            --vertical block
            else if isV player then foldl activateHelper ( L { playPosition = moveHelperV direction (getJosSus player)
                                                ,up = True
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperV direction (getJosSus player)) level)
                --horizontal block
                else foldl activateHelper ( L { playPosition = moveHelperH direction (getStangaDreapta player)
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperH direction (getStangaDreapta player)) level)
    West -> if u == True then  foldl activateHelper ( L { playPosition = moveHelperUp direction player
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperUp direction player) level)
            --vertical block
            else if isV player == True then foldl activateHelper ( L { playPosition = moveHelperV direction (getJosSus player)
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperV direction (getJosSus player)) level)
                --horizontal block
                else foldl activateHelper ( L { playPosition = moveHelperH direction (getStangaDreapta player)
                                                ,up = True
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperH direction (getStangaDreapta player)) level)
    East -> if u == True then  foldl activateHelper ( L { playPosition = moveHelperUp direction player
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperUp direction player) level)
            --vertical block
            else if isV player then foldl activateHelper ( L { playPosition = moveHelperV direction (getJosSus player)
                                                ,up = False
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperV direction (getJosSus player)) level)
                --horizontal block
                else foldl activateHelper ( L { playPosition = moveHelperH direction (getStangaDreapta player)
                                                ,up = True
                                                ,over = ov
                                                ,win = w
                                                ,coloane = col
                                                ,randuri = rand
                                                ,switches = sw
                                                 ,harta = h
                                            }) (getTile (moveHelperH direction (getStangaDreapta player)) level)


continueGame :: Level -> Bool
continueGame (L _ _ True _ _ _ _ _) = False
continueGame (L _ _ False _ _ _ _ _) = True

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

-- determines if a pair of direction , lvl is a succesor move
toAdd :: (Directions,Level) -> Bool
toAdd (_ , (L _ _ True True _ _ _ _)) = True
toAdd (_ , (L _ _ True False _ _  _ _)) = False
toAdd ( _, (L _ _ False _ _ _ _ _)) = True


instance ProblemState Level Directions where
    successors lvl
        |isGoal lvl = []
        |otherwise = filter toAdd $[(North, move North lvl), (South, move South lvl),(East, move East lvl),(West, move West lvl)]

    isGoal (L _ _ True True _ _ _ _) = True
    isGoal (L _ _ _ _ _ _ _ _) = False

    -- Doar petru BONUS
    -- heuristic = undefined

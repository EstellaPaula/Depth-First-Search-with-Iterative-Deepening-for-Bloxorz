{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S
import Data.Maybe
import Data.List

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = N {
    state :: s
    ,parinte :: Maybe (Node s a)
    ,direction :: Maybe a
    ,h :: Int
    ,kids :: [(a,s)]
    ,statesToGoThrough :: Int
}

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState node = state node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace initialState = N initialState Nothing Nothing 0 (successors initialState) 0


{-  
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

comparator :: (ProblemState s a, Ord s)
           =>  (a, s) -> (a , s) -> Ordering
comparator (_, s1) (_, s2)
    | (snd (iterativeDeepening (createStateSpace s1))) > (snd (iterativeDeepening (createStateSpace s2)))  = GT
    | otherwise = LT

orderStateSpace :: (ProblemState s a, Ord s) => Node s a -> Node s a
orderStateSpace node = (N (state node) (parinte node) (direction node) (h node) (sortBy comparator (kids node)) (statesToGoThrough node))


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

limitedHelper :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> (S.Set s, [Node s a])  -- accumulator
           -> (S.Set s, [Node s a]) -- result of
limitedHelper node depth result = 
    -- verificam daca starea unui copil a fost vizitata sau daca am ajuns la adancimea dorita
    foldl (\acc x -> if ((S.member (snd x) (fst acc)) || depth == 0) -- verificare daca starea a mai fost vizitata sau daca am ajuns la adancimea maxima
        then acc
        else limitedHelper  (N (snd x) (Just node) (Just (fst x)) ((h node) + 1) (successors (snd x)) 0) (depth - 1) 
        (S.insert (snd x) (fst acc), (N (snd x) (Just node) (Just (fst x)) ((h node) + 1) (successors (snd x)) 0) : (snd acc))
        )
    -- acumulator
    result
    -- succesori nod aka copii lui
    (kids node)


limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs root depth = reverse (snd (limitedHelper root depth ((S.insert (state root) (S.empty)) , [root]))) 

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeHelper :: (ProblemState s a, Ord s)
    => Node s a     
    -> Int             -- deepth reached for searching
    -> Int             -- numar de stari parcurse
    -> [Node s a]      -- adancimea pana la care efectuam cautarea
    -> (Node s a, Int) -- pereche stare finala / numar de stari parcurse
iterativeHelper start_node depth nr [] = iterativeHelper start_node (depth + 1) nr (limitedDfs start_node  (depth + 1))
iterativeHelper start_node depth nr (x:xs) = if (isGoal (state x)) == True    -- daca gasim stare finala returnam perechea
    then  (x, nr) 
    else iterativeHelper start_node  depth (nr + 1) xs


-- next two functions are used to set up the states to go for each node
updateNode :: Node s a -> Int -> Node s a
updateNode n statesTogo = (N (state n) (parinte n) (direction n) (h n) (kids n) statesTogo)

helperPair :: (Node s a, Int) -> (Node s a, Int) 
helperPair (n, states) = (updateNode n states, states)

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening root = helperPair (iterativeHelper root 0 0 (limitedDfs root 0))


----------------------------------------- bonus trials-------------------------

limitedHelperHeuristic :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> (S.Set s, [Node s a])  -- accumulator
           -> (S.Set s, [Node s a]) -- result of
limitedHelperHeuristic node depth result = 
    -- verificam daca starea unui copil a fost vizitata sau daca am ajuns la adancimea dorita
    foldl (\acc x -> if ((S.member (snd x) (fst acc)) || depth == 0) -- verificare daca starea a mai fost vizitata sau daca am ajuns la adancimea maxima
        then acc
        else limitedHelperHeuristic ((N (snd x) (Just node) (Just (fst x)) ((h node) + 1) (successors (snd x)) 0) ) (depth - 1) 
        (S.insert (snd x) (fst acc), ((N (snd x) (Just node) (Just (fst x)) ((h node) + 1) (successors (snd x)) 0) ) : (snd acc))
        )
    -- acumulator
    result
    -- succesori nod aka copii lui
    (kids (orderStateSpace node))


limitedDfsHeuristic :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfsHeuristic root depth = reverse (snd (limitedHelperHeuristic (root) depth ((S.insert (state root) (S.empty)) , [(root)]))) 



iterativeHelperHeuristic :: (ProblemState s a, Ord s)
    => Node s a     
    -> Int             -- deepth reached for searching
    -> Int             -- numar de stari parcurse
    -> [Node s a]      -- adancimea pana la care efectuam cautarea
    -> (Node s a, Int) -- pereche stare finala / numar de stari parcurse
iterativeHelperHeuristic root depth nr [] = iterativeHelperHeuristic ( root) (depth + 1) nr (limitedDfs ( root)  (depth + 1))
iterativeHelperHeuristic root depth nr (x:xs) = if (isGoal (state x)) == True    -- daca gasim stare finala returnam perechea
    then  (x, nr) 
    else iterativeHelperHeuristic (root)  depth (nr + 1) xs

iterativeDeepeningHeuristic :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepeningHeuristic root = helperPair (iterativeHelperHeuristic (orderStateSpace root) 0 0 (limitedDfsHeuristic (orderStateSpace root) 0))

--------------------------------------------------------------------------------

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

nodesPath :: (ProblemState s a, Ord s)
    => Node s a -> [Node s a]
nodesPath node = take (statesToGoThrough node) (iterate (\x -> (fromMaybe (createStateSpace (state x)) (parinte x)) ) node)

notNothing :: Maybe a -> Bool
notNothing d = case d of
    Nothing -> False
    _ -> True

helperP :: (ProblemState s a, Ord s)
    => Node s a -> [(a, s)]
helperP root = [(d, stare) | node <- (nodesPath root), 
                notNothing (direction node),
                let d = (fromJust (direction node)), 
                let stare = (state node)] 

extractPath :: (ProblemState s a, Ord s)
    => Node s a -> [(a, s)]
extractPath node = reverse (helperP node)

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve s False = extractPath (fst (iterativeDeepening (createStateSpace s)))
solve s True = extractPath (fst (iterativeDeepening (orderStateSpace (createStateSpace s))))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
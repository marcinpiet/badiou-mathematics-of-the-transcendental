{-# LANGUAGE GADTs, TypeOperators, MultiParamTypeClasses, KindSignatures, FlexibleInstances, DataKinds #-}

{-    An Analysis of Alain Badiou's "Preliminary Definitions" from 'Mathematics of the Transcendental'
    and an experimental implementation in Haskell.

    Badiou introduces the fundamental concepts of category theory. In plain language, this is a way of
    describing systems of objects and the relationships between them in a very abstract manner.

    The goal is to move away from what objects *are* (their internal properties, like in set theory)
    and focus entirely on how they connect to other objects.

    Key Concepts:
    1. Objects: These are the fundamental entities. Think of them as points, locations, or even types.
       In our code, we'll represent specific objects like `A`, `B`, and `C`.

    2. Arrows (Morphisms): An arrow is a directed connection or transformation from a source object
       to a target object. If you have an arrow `f` from `A` to `B`, it's written as `f: A -> B`.
       Badiou calls this a "directed action". The key idea is that arrows are defined by their
       source and target and their relationships with other arrows, not by any internal content.

    3. Composition: This is the most crucial operation. If you have an arrow `f` from `A` to `B` and
       another arrow `g` from `B` to `C`, you can 'compose' them to create a new arrow that goes
       directly from `A` to `C`. Badiou writes this as `g . f` (read "g after f").
       This is like taking a flight from New York to London, and then another from London to Tokyo.
       The composition is the total journey from New York to Tokyo.

    4. Identity Arrow: For every object, there is a special "identity" arrow that starts and ends
       at that same object (e.g., `idA: A -> A`). Badiou calls this a "null action". It represents
       doing nothing or staying put. When composed with any other arrow, it doesn't change it.
       (e.g., flying from London to Tokyo, and then taking the "identity" flight from Tokyo, is the
       same as just flying from London to Tokyo).

    The entire structure is governed by a few simple laws, the most important being that composition
    is associative: `h . (g . f)` is the same as `(h . g) . f`.
-}

-- We define the abstract notion of a Category using a typeclass.
-- The `cat` variable represents the type of the arrows.
-- The `k` is the kind of the objects, which are represented as types.
class Category (cat :: k -> k -> *) where
  -- The identity arrow for an object `a`.
  identity :: cat a a
  -- The composition of two arrows.
  compose :: cat b c -> cat a b -> cat a c

-- To experiment, let's define a simple, concrete category.
-- First, we define its objects as types using a GADT.
data SimpleObject = A | B | C

-- Now, we define the arrows for our SimpleCategory.
-- This GADT explicitly lists every arrow that exists in our category.
data SimpleCategory a b where
  -- Identity arrows
  IdA :: SimpleCategory A A
  IdB :: SimpleCategory B B
  IdC :: SimpleCategory C C
  -- Other arrows
  F :: SimpleCategory A B
  G :: SimpleCategory B C
  H :: SimpleCategory A C -- This could be the composition of F and G

-- To make our arrows printable and comparable
instance Show (SimpleCategory a b) where
  show IdA = "IdA"
  show IdB = "IdB"
  show IdC = "IdC"
  show F = "F"
  show G = "G"
  show H = "H"

instance Eq (SimpleCategory a b) where
  IdA == IdA = True
  IdB == IdB = True
  IdC == IdC = True
  F == F = True
  G == G = True
  H == H = True
  _ == _ = False

-- Now we declare that our SimpleCategory is an instance of the Category typeclass.
-- We must provide implementations for `identity` and `compose`.
instance Category SimpleCategory where
  identity = IdA -- Note: A more robust implementation would handle IdB, IdC etc.
  
  -- Composition is defined by pattern matching on the valid compositions.
  compose IdC G = G
  compose G IdB = G
  compose IdB F = F
  compose F IdA = F
  compose G F = H -- Here we explicitly define that H is the composition of G after F.
  -- Any other composition is undefined, effectively not existing in the category.

-- Main function to run our experiments
main :: IO ()
main = do
  putStrLn "--- Badiou Category Theory Experiments ---"

  -- 1. Experiment with Identity Law
  putStrLn "\n1. Testing Identity Law:"
  let g_composed_with_id = compose g idB
  putStrLn $ "  g . idB = " ++ show g_composed_with_id
  putStrLn $ "  Is g . idB == g? " ++ show (g_composed_with_id == g)

  -- 2. Experiment with Composition
  putStrLn "\n2. Testing Composition:"
  let h_from_composition = compose g f
  putStrLn $ "  g . f = " ++ show h_from_composition
  
  -- 3. Badiou discusses commutative diagrams. This is a way of asking:
  --    Is the direct path `h` the same as the composed path `g . f`?
  putStrLn "\n3. Testing Commutative Diagram (h == g . f):"
  putStrLn $ "  Is the explicit arrow H the same as the composed arrow (G . F)?"
  putStrLn $ "  Result: " ++ show (h == h_from_composition)
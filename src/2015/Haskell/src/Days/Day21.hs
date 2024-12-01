{-# LANGUAGE InstanceSigs #-}
module Days.Day21 (solve) where
import AoCUtils.Days (Solver)

solve :: Solver
solve input = undefined

class Equipment a where
  stats :: a -> (Int, Int, Int)

data Stat = Cost | Damage | Armor

data Player = Player Weapon Armor Rings

data Weapon = Dagger | Shortsword | Warhammer | Longsword | Greataxe

data Armor = Bare | Leather | Chainmail | Splintmail | Bandedmail | Platemail

data Rings = None | One Ring | Two Ring Ring

data Ring = Damage1 | Damage2 | Damage3 | Defense1 | Defense2 | Defense3

instance Equipment Weapon where
  stats :: Weapon -> (Int, Int, Int)
  stats Dagger = (8, 4, 0)
  stats Shortsword = (10, 5, 0)
  stats Warhammer = (25, 6, 0)
  stats Longsword = (40, 7, 0)
  stats Greataxe = (74, 8, 0)

instance Equipment Armor where
  stats :: Armor -> (Int, Int, Int)
  stats Bare = (0, 0, 0)
  stats Leather = (13, 0, 1)
  stats Chainmail = (31, 0, 2)
  stats Splintmail = (53, 0, 3)
  stats Bandedmail = (75, 0, 4)
  stats Platemail = (102, 0, 5)

instance Equipment Ring where
  stats :: Ring -> (Int, Int, Int)
  stats Damage1 = (25, 1, 0)
  stats Damage2 = (50, 2, 0)
  stats Damage3 = (100, 3, 0)
  stats Defense1 = (20, 0, 1)
  stats Defense2 = (40, 0, 2)
  stats Defense3 = (80, 0, 3)

instance Equipment Rings where
  stats :: Rings -> (Int, Int, Int)
  stats None = (0, 0, 0)
  stats 

getStat :: Equipment a => Stat -> a -> Int
getStat stat equipment = case stat of
  Cost -> cost
  Damage -> damage
  Armor -> armor
  where
    (cost, damage, armor) = stats equipment

totalStat :: Stat -> Player -> Int
totalStat stat (Player weapon armor rings) = 
  getStat stat weapon + 
  getStat stat armor + 
  getStat stat rings
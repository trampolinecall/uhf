module UHF.Util.InfList where

data InfList a = a ::: InfList a

from_list :: a -> [a] -> InfList a
from_list end (x:more) = x ::: from_list end more
from_list end [] =
    let x = end ::: x
    in x

head :: InfList a -> a
head (x ::: _) = x

drop1 :: InfList a -> InfList a
drop1 (_ ::: xs) = xs

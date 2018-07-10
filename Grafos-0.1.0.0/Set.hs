module Set (
    Set (Set),
    eset,
    set, 
    add,
    addr,
    remove,
    union,
    inter,
    dif,
    isEmpty,
    exists
) where 

data Set a = Set [a]

instance (Show a)=>Show (Set a) where
    show (Set a)= show a

set::a->Set a
set x = Set [x]

eset::Set a
eset = Set []

addl::a->Set a->Set a
addl x (Set []) = Set [x]
addl x (Set (y:ys)) = Set ([x]++(y:ys))

addr::a->Set a->Set a
addr x (Set []) = Set [x]
addr x (Set (y:ys)) = Set ((y:ys)++[x])

add::(Eq a, Ord a)=>a->Set a->Set a
add x (Set []) = Set [x]
add y (Set (x:xs)) = if x==y then Set (x:xs) else if x<y then addl x (add y (Set xs)) else Set ([y]++(x:xs))

union::(Eq a, Ord a)=>Set a->Set a->Set a
union (Set x) (Set []) = Set x
union (Set []) (Set y) = Set y
union (Set (x:xs)) (Set (y:ys)) = if x==y then union (Set (x:xs)) (Set ys) else if x<y then addl x (union (Set xs) (Set (y:ys))) else addl y (union (Set (x:xs)) (Set ys))


remove::(Eq a)=>a->Set a->Set a
remove x (Set []) = Set []
remove y (Set (x:xs)) = if x==y then Set xs else addl x (remove y (Set xs))

isEmpty::Set a->Bool
isEmpty (Set []) = True
isEmpty (Set (x:xs)) = False

exists::(Eq a)=>a->Set a->Bool
exists x (Set []) = False
exists x (Set (y:ys)) = if x==y then True else exists x (Set ys)

inter::(Eq a, Ord a)=>Set a->Set a->Set a
inter (Set []) x = Set []
inter x (Set []) = Set []
inter (Set (x:xs)) (Set (y:ys)) = if x==y then addl x (inter (Set xs) (Set ys)) else if x<y then inter (Set xs) (Set (y:ys)) else inter (Set (x:xs)) (Set ys)

dif::(Eq a, Ord a)=>Set a->Set a->Set a
dif (Set x) (Set []) = Set x
dif (Set []) x = Set []
dif (Set (x:xs)) (Set (y:ys)) = if x==y then dif (Set xs) (Set ys) else if x<y then addl x (dif (Set xs) (Set (y:ys))) else addl x (dif (Set (x:xs)) (Set ys))

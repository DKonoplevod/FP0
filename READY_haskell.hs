-- 7
-- Удалить из исходного списка все повторные вхождения элементов

removeDuplicates = foldl (\res x -> if x `elem` res
                                      then res
                                      else res ++ [x]) []

---------------------------------------------------------------------------
-- 17
-- Определите функцию МНОЖЕСТВО, преобразующую список в множество.

makeSet = removeDuplicates

---------------------------------------------------------------------------

-- 18
-- Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух мно-
-- жеств (независимо от порядка следования элементов).

isSameSet set1 set2 = null (filter (\x -> x `notElem` set2) set1) 
                    && null (filter (\x -> x `notElem` set1) set2)

---------------------------------------------------------------------------

-- 19
-- Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно мно-
-- жество подмножеством другого. Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО

isSubset subset set = null (filter (\x -> x `notElem` set) subset)

isSelfSubset subset set = isSubset subset set 
                        && not (null subset)
                        && (length subset) < (length set)

---------------------------------------------------------------------------

-- 24
-- Определите функцию РАЗНОСТЬ, формирующую разность двух множеств, т.е.
-- удаляющую из первого множества все общие со вторым множеством элементы.

diff set1 set2 = filter (\x -> x `notElem` set2) set1
                        
---------------------------------------------------------------------------

main = do
    putStrLn "Tests for task 7"
    putStrLn "Test 1"
    let list = [3,1,2,3,4,2,3,3,3]
    print (removeDuplicates list)

    putStrLn "Tests for task 17"
    putStrLn "Test 1"
    let list = [1,2,3,4,2,3]
    print (makeSet list)
    putStrLn "Test 2"
    let list = [1,2,3,4,2,3,1,7,8,1,2,4]
    print (makeSet list)

    putStrLn "Tests for task 18"
    putStrLn "Test 1"
    let set1 = [1,2,3]
    let set2 = [2,1,3]
    print (isSameSet set1 set2)
    putStrLn "Test 2"
    let set1 = [1,2,3]
    let set2 = [1,3,2,4]
    print (isSameSet set1 set2)
    putStrLn "Test 3"
    let set1 = [1,2,3,5]
    let set2 = [1,3,2,4]
    print (isSameSet set1 set2)
    putStrLn "Test 4"
    let set1 = [1,2,3,5]
    let set2 = [1,2,5]
    print (isSameSet set1 set2)

    putStrLn "Tests for task 19"
    putStrLn "Test 1"
    let set1 = [1,2]
    let set2 = [1,2,5]
    print (isSubset set1 set2)
    print (isSelfSubset set1 set2)
    putStrLn "Test 2"
    let set1 = [1,2,3]
    let set2 = [1,2,5]
    print (isSubset set1 set2)
    print (isSelfSubset set1 set2)
    putStrLn "Test 3"
    let set1 = [1,2,5,3,4]
    let set2 = [1,2,5]
    print (isSubset set1 set2)
    print (isSelfSubset set1 set2)
    putStrLn "Test 4"
    let set1 = []
    let set2 = [1,2,5]
    print (isSubset set1 set2)
    print (isSelfSubset set1 set2)
    putStrLn "Test 5"
    let set1 = [1,2,3]
    let set2 = [1,2,3]
    print (isSubset set1 set2)
    print (isSelfSubset set1 set2)

    putStrLn "Tests for task 24"
    putStrLn "Test 1"
    let set1 = [1,2,3,4,5]
    let set2 = [4]
    print (diff set1 set2)
    putStrLn "Test 2"
    let set1 = [1,2,3,4,5]
    let set2 = [1,3,5]
    print (diff set1 set2)
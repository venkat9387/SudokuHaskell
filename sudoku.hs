{-------------------------------------------------------------------------------
    Question Number : Task 1 (Haskell Programming)
    Description     : To find a solution for 4x4 sudoku problem.
    Author          : Venkatesan Manivasagam
    Student Number  : 2007095
    Date Created    : 06/06/2021
    References      : Hutton G, 2021,
		      Source : https://www.youtube.com/watch?v=glog9DZh8G0&t=21s
-------------------------------------------------------------------------------}

-- Declarations of variables. 
subMatrixSize  = 2
type Array     = Board Value
type Board a  = [Row a]
type Row a     = [a]
type Value     = Int
values        :: [Value]
values         = [1,2,3,4]
type Population   = [Value]


-- Function 1 :- Creating a blank 4x4 Board with 0 as placeholder value. 

blank :: Array
blank  =  take n $ repeat (take n ( repeat 0))
           where n = subMatrixSize ^ 2



-- Function 2 :- Declaring row values from the List of List (input). 
--               Each list is considered as a row. 

rows :: Board a -> [Row a]
rows m = m



-- Function 3 :- Declaring column values. List of List is taken and Transpose 
--               methodology used to convert the rows as columns. 

columnTranspose :: Board a -> [Row a]
columnTranspose m = colTrans m []
  where
    colTrans [] a = a
    colTrans ([]:xss) a = colTrans xss a
    colTrans ((x:xs):xss) a = colTrans a ((x:map head xss): colTrans (xs:map tail xss) a)


-- Function 4 :- Given an input of type Board, extracts subMatrix as rows.

subMatrix :: Board a -> [Row a]
subMatrix = unpack . map columnTranspose . pack
       where
        pack   = split . map split
        split  = chop subMatrixSize
        unpack = map concat . concat


-- Function 5 :- Given an type Board it is splited into equal parts. 

chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)



-- Function 6 :- Given an solution, check if the solution is correct by checking 
--               if has duplicates and returns true or false. 

validMatrix                 ::  Array -> Bool
validMatrix g               =   all checkDuplicate (rows g) &&
                                all checkDuplicate (columnTranspose g) &&
                                all checkDuplicate (subMatrix g)

-- Function 7 :- Works as the subfunction for the above function. 
checkDuplicate                :: Eq a => [a] -> Bool
checkDuplicate []             =  True
checkDuplicate (x:xs)         =  not (elem x xs) && checkDuplicate xs



-- Function 8 :- Maps the 0 value with the values [1,2,3,4]. 

population               :: Array -> Board Population
population               =  map (map choice)
                         where
                            choice v = if v==0 then values else [v]


-- Function 9:- After mapping the values, they are cartesianed to create a list 
--              of list.

cartesianProduct                    :: [[a]] -> [[a]]
cartesianProduct []                 =  [[]]
cartesianProduct (xs:xss)           =  [y:ys | y <- xs, ys <- cartesianProduct xss]

-- Function 10 :- The cartesianed lists are grouped together form possible
--                rows and colums.

collapse              :: Board [a] -> [Board a]
collapse              =  cartesianProduct . map cartesianProduct

-- Function 11 :- Function 8 , 10 and 6 are called together to derive an valid output.
solveAsGrid                 :: Array -> [Array]
solveAsGrid                 =  filter  validMatrix . collapse . population

-- Function 12 :- The selected solution is reduced into list of list as described 
---------         in the portfolio. 

solution :: Array -> Array
solution x = concat $ solveAsGrid x 


{- --------- Below are the test cases used to test the implementation----------
--main :: IO ()
--main = do
-- print blank
--print $ columnTranspose [[3,4,0,0],[2,0,3,0],[0,3,0,2],[0,0,1,3]]
--print $ subMatrix [[3,4,0,0],[2,0,3,0],[0,3,0,2],[0,0,1,3]]
--print $ validMatrix  [[3,4,2,1],[2,1,3,4],[1,3,4,2],[4,2,1,3]]
--print $ population [[3,4,0,0],[2,0,3,0],[0,3,0,2],[0,0,1,3]]
--print $ cartesianProduct [[3,4,2,1],[2,1,3,4],[1,3,4,2],[4,2,1,3]]
--print $ population [[3,4,0,0],[2,0,3,0],[0,3,0,2],[0,0,1,3]]
--print $ collapse [[[3],[4],[1,2,3,4],[1,2,3,4]]]
--print $ solveAsGrid [[3,4,0,0],[2,0,3,0],[0,3,0,2],[0,0,1,3]]
--print $ solution [[3,4,0,0],[2,0,3,0],[0,3,0,2],[0,0,1,3]]
-- ------------------------------------------------------------------------------}
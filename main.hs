finalGrade :: [Int] -> [Int] -> Int
finalGrade l1 l2
         |(length(l1) == 0) = 0
	 |(length(l2) == 0) = 0
	 |otherwise = sum1(l1) * sum1(l2) 'div' sum1(l2)
         	 
	
sum1 :: [Int] -> Int
sum1 l1
    | (length(l1) == 0) = 0
    | otherwise = head(l1)+ sum1(tail(l1))

weightAvg :: [Int] -> [Int] -> Int
weightAvg l1 l2
    | (length(l1) == 0) = 0
    | (length(l2) == 0) = 0
    | otherwise = head(l1) * head(l2) + (weightAvg tail l1 (tail(l2)))

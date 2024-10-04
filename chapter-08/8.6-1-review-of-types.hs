-- 8.6-review-of-types.hs

-- 1. What is the type of `[[True, False], [True, True], [False, True]]`?
--   (d) `[[Bool]]`

-- 2. Which of the following has the same type as 
--    `[[True, False], [True, True], [False, True]]`?
--   (b) `[[3 == 3], [6 > 5], [3 < 4]]`

-- 3. For the function below, which of the following statements are true?
--  func :: [a] -> [a] -> [a]
--  func x y = x ++ y
--  (d) all of the above

-- 4. For the `func` code above, which is a valid application of `func` to both of its 
--    arguments?
--  (b) `func "Hello" "World"`

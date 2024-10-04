-- 8.6-reviewing-currying.hs

module ReviewingCurrying where

-- Given the following definitions, tell us what value results from further 
-- applications:

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. What is the value of `appedCatty "woohoo!"`? Try to determine the answer for 
-- yourself, then test it in the REPL.
-- `appedCatty "woohoo!" == cattyConny "woops" "woohoo!" == "whoops mrow wohoo!"

-- 2. `frappe "1"`
-- frappe "1" = flippy "haha" "1" = (flip cattyConny) "haha" "1" = cattyConny "1" "haha"
-- = "1 mrow haha"

-- 3. frappe (appedCatty "2")
-- frappe (appedCatty "2") = frappe (cattyConny "whoops" "2") =
--  frappe ("whoops mrow 2") = flippy "haha" "whoops mrow 2" = 
--  (flip cattyConny) "haha" "whoops mrow 2" = cattyConny "whoops mrow 2" "haha"
--  = "whoops mrow 2 mrow haha"

-- 4. appedCatty (frappe "blue") = appedCatty (flippy "haha" "blue") = 
--      appedCatty (cattyConny "blue" "haha") = cattyConny "whoops" "blue mrow haha"
--      = "whoops mrow blue mrow haha"

-- 5. cattyConny (frappe "pink")
--               (cattyConny "green"
--                 (appedCatty "blue"))
-- cattyConny (frappe "pink")
--               (cattyConny "green"
--                "whoops mrow blue")
-- = cattyConny (frappe "pink") "green mrow whoops mrow blue"
-- = cattyConny (flippy "haha" "pink") "green mrow whoops mrow blue"
-- = cattyConny (cattyConny "pink" "haha") "green mrow whoops mrow blue"
-- = cattyConny "pink mrow haha" "green mrow whoops mrow blue"
-- = "pink mrow haha mrow green mrow whoops mrow blue"

-- 6. cattyConny (flippy "Pugs" "are") "awesome"
-- = cattyConny (cattyConny "are" "Pugs") "awesome"
-- = cattyConny "are mrow Pugs" "awesome"
-- = "are mrow Pugs mrow awesome"
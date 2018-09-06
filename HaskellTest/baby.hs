doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
conanO'Brien = "It's a-me, Conan O'Brien!"
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
--[ [ x | x <- xs, even x ] | xs <- xxs]
--= [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
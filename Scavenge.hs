module Scavenge where
import Dictionaries
import Data.List
import Debug.Trace

--                                          Type Aliases
-- These type aliases help to abstract our code. 
-- 
type Hand = [Char]
type Move = String
type Play = [Move]
type Dictionary = [String] 

-- You don't need to use buildDict currently. However, if you change the type of Dictionary
-- to be more efficient than a straight list, you must change buildDict accordingly.

buildDict :: [String] -> Dictionary
buildDict dct = dct

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the --test flag) even if some functions are left undefined.
--
--                                       Milestone
--

-- Score takes a move and returns the sum of the letter values, according to the Scrabble scoring
-- system: https://scrabble.hasbro.com/en-us/faq
-- A helper function may be useful.
score :: Move -> Integer
score word = sum [helpScore letter | letter <- word]

--helpScore takes a character and returns it's point value, according to the Scrabble scoring system
helpScore :: Char -> Integer
helpScore x | x `elem`         "QZ" = 10
            | x `elem`         "JX" = 8
            | x `elem`          "K" = 5
            | x `elem`      "FHVWY" = 4
            | x `elem`       "BCMP" = 3
            | x `elem`         "DG" = 2
            | x `elem` "AEIOULNSTR" = 1
            | otherwise = error "A non-letter character was part of the move."

-- scorePlay takes a play and returns the total score of all words.
scorePlay :: Play -> Integer
scorePlay listOfWords= sum [score word | word <- listOfWords]

-- remove takes an element and a list, and returns the list with one copy of that element removed.
-- You should not assume the list is sorted. If there are multiple copies of the element in the list,
-- only remove one of them. If the element doesn't occur, you should throw an error.
remove :: Eq a => a -> [a] -> [a]
remove a [] = error "element is not in the list"
remove y (x:xs) = if(y `elem` (x:xs)) then if x == y then xs else x : remove y xs
                                      else error "element is not in list"
-- remove 7 [7,3,1,7,5] = [3,1,7,5]
-- The order here doesn't matter, if you remove the second 7 it is okay.

-- updateHand should take a hand (a list of characters), and a move (a string), and return the hand
-- that remains after that move is played.
updateHand :: Hand -> Move -> Hand
updateHand hand move = if(length move == 1) then remove (head move) hand
                                           else updateHand (remove (head move) hand) (tail move)
-- updateHand "HELLO" "LO" = "HEL"

-- canMake takes a hand and a move, and tells you if that move can be made with that hand. Be sure to
-- consider the possibility that a letter may occur more times in the move than it does in the hand.
canMake :: Hand -> Move -> Bool
canMake hand [] = True
canMake hand move = canMakeSort sortH sortM
    where sortH = sort hand
          sortM = sort move
canMakeSort :: Hand -> Move -> Bool
canMakeSort hand [] = True
canMakeSort [] move = False
canMakeSort (h:hs) (m:ms) = if(h == m) 
                            then canMakeSort hs ms
                            else canMakeSort hs (m:ms)

--canMake [] [] = True
--canMake a []  = True
--canMake hand (y:ys) | length hand < length hand = False
--                    | y `elem` hand = canMake (updateHand hand [y]) ys
--                    | otherwise = False

-- "DNAHTSET" `canMake` "HAND" = True 
-- "DNAHTSET" `canMake` "HAAND" = False
-- For full credit, this must run in near-linear time (n log n is sufficient)

-- isValidMove tests if a move is valid with respect to a dictionary and hand: 
-- the move must be a word in the dictionary and a move that can be made with the hand.
isValidMove :: Dictionary -> Hand -> Move -> Bool
isValidMove dict hand move = if (canMake hand move) then if (move `elem` dict) 
                                                         then True
                                                         else False
                                                    else False
-- isValidMove tinyDict "MKEKIOCHAUX" "MAKE" = TRUE
-- isValidMove tinyDict "MKEKIOCHAUX" "MAXKE" = FALSE
-- isValidMove tinyDict "MKEKIOCHAUX" "FAKE" = FALSE

-- isValidPlay checks if a play is valid. Each move in the play must be a word in the dictionary, and
-- must be playable using whatever remains of the hand after all previous moves have been made.
isValidPlay :: Dictionary -> Hand -> Play -> Bool
isValidPlay dict hand [] = True
isValidPlay dict hand play | not (canMake hand (head play)) = False
                           | isValidMove dict hand (head play) = isValidPlay dict (updateHand hand (head play)) (tail play)
                           | otherwise = False
-- isValidPlay tinyDict "TMAKE" ["TAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["MAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["TAKE","MAKE"] = False
    
-- validMoves: takes a dictionary and a hand, and returns all words that can be
-- created by letters in the hand. Order does not matter.
validMoves :: Dictionary -> Hand -> [Move]
--validMoves dict [] = []
--validMoves dict hand = if(isValidMove dict hand)
validMoves dict hand = [word | word <- dict, canMake hand word]
-- validMoves shortDict "PEMDOVZIJM" = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]


--                                  End of Milestone!

--                                  Final Project 

-- Greedy Algorithm
 
-- maximumMove: takes a list of moves, and returns the one with the highest
-- score. Return an error for the empty list.
maximumMove:: [Move] -> Move
maximumMove [] = error "empty list"
maximumMove [a] = a
maximumMove allMove = res
    where maxLst = [(score move, move) | move <- allMove]
          (scr, res) = maximum maxLst
--maximumMove ["OR", "OK"]  = "OK"

-- greedyPlay: choose the best move you can at any given point in time, then check to see what
-- other moves you can make.
greedyPlay :: Dictionary -> Hand -> Play
greedyPlay dict [] = []
greedyPlay dict hand = if(validMoves dict uhb == []) 
                           then [bestMove] 
                           else [bestMove] ++ (greedyPlay vms uhb)
    where bestMove = (maximumMove vms)
          uhb = updateHand hand bestMove
          vms = validMoves dict hand
--by passing in vms, we are no longer pruning the dictionary for valid moves each recursive call
-- greedyPlay shortDict "CLOSEFLOOR" = ["FORCE", "SO"] 

-- Brute Force Algorithms
-- You are going to search for the best play, instead of just choosing the best move one at a time.
-- To do so, you will consider every possible play, by considering every possible combination of
-- words. You will implement two variants. 

-- maximumPlay takes a list of plays and returns the highest scoring play.
maximumPlay :: [Play] -> Play
maximumPlay [] = error "no plays, empty list"
maximumPlay [p] = p
maximumPlay (p:ps) = if(scorePlay p > maximum [scorePlay x | x <- ps]) 
                         then p
                         else maximumPlay ps
--maximumPlay [["HI","THERE"], ["HI"], ["QUASAR"]] = ["HI", "THERE"]

-- powerset: return the powerset of the input, i.e. the list of all sub-lists.
-- You may assume the list has no duplicates. 
-- The output should not have duplicates, up to sorting.
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset [x] = [[x],[]]
--put head onto the list recursive then and that again recursive
powerset (x:xs) = [x:rest | rest <- rec] ++ rec
    where rec = powerset xs
-- powerset [1,2] = [[],[1],[1,2],[2]]
-- It is acceptable to have [2,1] INSTEAD of [1,2], but not in addition.
-- length (powerset "abcde") = 32

-- The Naive Brute Force (naiveBrutePlay) takes every combination of moves in
-- the dictionary: .the powerset of the dictionary. It will only work with very small
-- dictionaries, like tenWords.  You will then choose the best valid play out of that list.
naiveBrutePlay :: Dictionary -> Hand -> Play
naiveBrutePlay dict hand = maximumPlay vps
    where vps = [mv | mv <- pd, isValidPlay dict hand mv]
          pd = powerset dict

-- The Smart Brute Force approach realizes that we can restrict the size of the dictionary
-- before we compute the powerset. There are probably MANY moves in the dictionary that we can't
-- create at all! So, first find all the moves that can be made with this hand. Then, take the
-- powerset of that restricted list to create a list of all plays made up of those moves. Then
-- find the best valid play from that list.
smartBrutePlay :: Dictionary -> Hand -> Play
smartBrutePlay dict hand = maximumPlay vPls
    where vPls = [play | play <- plays, isValidPlay dict hand play]
          plays = powerset validDict
          validDict = validMoves dict hand


-- Recursive Game-Tree Algorithm

-- Finally we will try a recursive strategy to find the best play. Even the smart brute force
-- fails for large hands: I can make a lot of moves, but not a lot of moves simultaniously. Thus
-- the list of all possible plays is large, but the list of all valid plays is still likely
-- small. 
-- For this algorithm, start with the list of valid moves. Then, for each move find the
-- best play for the remaining hand. Select the hand that leads to the best overall score, counting both
-- the score of the move and the score of the best remaining play.
recBestPlay:: Dictionary -> Hand -> Play
recBestPlay dict [] = []
recBestPlay dict hand = if (plays /= []) then maximumPlay plays
                                         else []
    where plays = [m:(recBestPlay dict h) | (m, h) <- remHandLst]
          lstValidMoves = validMoves dict hand
          remHandLst = [(move, (updateHand hand move)) | move <- lstValidMoves]














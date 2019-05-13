module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

-- In module Statment, T is visible outside => Statement.T
newtype T = Program [Statement.T]
instance Parse T where
  parse = makeStates
  toString = makeString


--Reads a string in coreparser with fromString, calls parse in Program. 
-- Which calls makeStates then parse every line with iter.
-- Makes a list of statements. Which then run through exec from TestProgram.

-- Same pattern as parse functions in statement. 
makeStates:: Parser T    
makeStates = iter Statement.parse >-> makeProgram
makeProgram = Program 

makeString :: T -> String
makeString (Program []) = []
makeString (Program state) = Statement.makeString (head state) "" ++ makeString (Program (tail state))

--Program.exec p [3,16]
-- Parsed program p => fromString(string)
-- Statement.exec [statements] dict input
exec:: T -> [Integer] -> [Integer]
exec (Program statements) xs = Statement.exec statements  Dictionary.empty xs  




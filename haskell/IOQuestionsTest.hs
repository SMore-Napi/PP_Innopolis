-- Lab 7
-- Roman Soldatov B19-SD-01
-- r.soldatov@innopolis.university

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}


module Main where

import Text.Read (readMaybe)
main = runTest ["age?", "weight?"]

-- Exercise 6.1.
sumOfTwoInputs :: IO ()
sumOfTwoInputs = do
    input1 <- getLine
    input2 <- getLine
    let number1 = (read input1 :: Int)
    let number2 = (read input2 :: Int)
    print (number1 + number2)

-- Exercise 6.2.
sumOfManyInputs :: IO ()
sumOfManyInputs = do
    input <- getLine
    let number = (read input :: Int)
    readInputs number 0
    where
        readInputs :: Int -> Int -> IO ()
        readInputs 0 res = print res
        readInputs amount res = do
            input <- getLine
            let number = (read input :: Int)
            readInputs (amount - 1) (res + number)

-- Exercise 6.3.
type Question = String
type Answer = Int

runQuestion :: Question -> IO (Maybe Answer)
runQuestion q = do
    putStrLn q
    input <- getLine
    case readMaybe input :: Maybe Answer of
        Nothing -> return Nothing
        Just x -> return (Just x)

-- Exercise 6.4.
runQuestions :: [Question] -> IO [Maybe Answer]
runQuestions = mapM runQuestion

-- Exercise 6.5.
type Test = [Question]
type TestAnswers = [Maybe Answer]

runTest :: Test -> IO TestAnswers
runTest = runQuestions

-- Exercise 6.6. - 6.8.
data QuestionType = TrueFalseQuestion | MultipleChoiceQuestion | FreeTextQuestion
data AnswerType input = TrueFalseAnswer input | MultipleChoiceAnswer [input] | FreeTextAnswer input

type Question2 = (QuestionType, String)
type Answer2 = (QuestionType, AnswerType String)

-- type Test2 = [Question2]
type TestAnswers2 = [Maybe Answer2]

-- Exercise 6.9.
type Checker = AnswerType String
type TestChecker = [Checker]

-- Exercise 6.10.
gradeTest :: TestAnswers2 -> TestChecker -> [Bool]
gradeTest _ [] = []
gradeTest [] _ = []
gradeTest (a:answers) (c:checkers) = checkAnswer a c : gradeTest answers checkers
    where
        checkAnswer :: Maybe Answer2 -> Checker -> Bool
        checkAnswer Nothing _ = False
        checkAnswer (Just answer) chkr = compareAnswers answer chkr
        compareAnswers :: Answer2 -> Checker -> Bool
        compareAnswers (_, FreeTextAnswer _a) (FreeTextAnswer _c) = _a == _c
        compareAnswers (_, FreeTextAnswer _a) _ = False
        compareAnswers (_, TrueFalseAnswer _a) (TrueFalseAnswer _c) = _a == _c
        compareAnswers (_, TrueFalseAnswer _a) _ = False
        compareAnswers (_, MultipleChoiceAnswer _a) (MultipleChoiceAnswer _c) = _a == _c
        compareAnswers (_, MultipleChoiceAnswer _a) _ = False

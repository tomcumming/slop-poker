module Main where

import System.Console.ANSI

main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
  putStr "♠ Slop Poker "
  setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
  putStrLn "♥"
  setSGR [Reset]
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "Hello from terminal Hold'em."
  setSGR [Reset]

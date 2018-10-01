import Control.Monad.State
import Debug.Trace

rotate :: String -> String
rotate string = let 
    (l,r,_) = foldr (\c (f,s,fl) -> if fl then (f,c:s,False) else (c:f,s,True)) ([],[],False) string
    in r ++ l
rotateSt :: State String String
rotateSt = do
    s <- gets rotate
    put s
    return s

rotateSt1 :: State String String
rotateSt1 = do 
    modify rotate
    s <- get
    return s

rotateSt2 :: State String String
rotateSt2 = modify rotate >> get

-- rotateStringPFull times string = foldr (<$>) id (replicate times rotate) string
rotateString = foldr (<$>) id . flip replicate rotate



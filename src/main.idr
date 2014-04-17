module Main

import parse

partial -- no error checking!
readHandle : File -> IO String
readHandle h = readHandle' ""
  where
    partial
    readHandle' : String -> IO String
    readHandle' contents =
      do x <- feof h
         if not x
            then do l <- fread h
                    readHandle' $ contents ++ l
            else return contents

elimWhitespace : List Char -> List Char
elimWhitespace = List.filter (not . flip List.elem [' ', '\n'])

optimise : String -> String
optimise input = case parse . elimWhitespace $ unpack input of
                      Left  err         => "ERROR: " ++ err
                      Right (_ ** prog) => show prog

main : IO ()
main = do
  --frontend <- popen "ao abcRaw fibonacci" Read
  --input <- readHandle frontend
  --pclose frontend -- Silently ignores errors. Hopefully will actually return the code in the future.
  let input = "lrlrlrlr"
  putStrLn $ "Received input:\n" ++ input
  let output = optimise input
  putStrLn $ "Optimised form:\n" ++ output
  putStrLn "Passing to backend"
  backend <- popen "abcc-backend" Write
  fwrite backend output
  pclose backend

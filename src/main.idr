module Main

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

optimise : List Char -> List Char
optimise = List.filter (not . flip List.elem [' ', '\n'])

main : IO ()
main = do
  frontend <- popen "ao abcRaw fibonacci" Read
  input <- readHandle frontend
  pclose frontend -- Silently ignores errors. Hopefully will actually return the code in the future.
  putStrLn $ "Received input:\n" ++ input
  let output = pack . optimise . unpack $  input
  putStrLn $ "Optimised form:\n" ++ output
  putStrLn "Passing to backend"
  backend <- popen "abcc-backend" Write
  fwrite backend output
  pclose backend

module Main

main : IO ()
main = do
  p <- popen "abcc-backend" Write
  fwrite p "Hello world from Idris!\n"
  pclose p

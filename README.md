Haskell library for OpenBSD's pledge(2)
=======================================

Example Haskell program:

    import System.Pledge
    import System.IO

    main :: IO ()
    main = do
      pledge [Stdio] []
      print "Pledge works"
      withFile "/dev/urandom" ReadMode $ \f ->
        hGetContents f >>= putStr

pledge(2) kills this when withFile tries to open file.

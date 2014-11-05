better-life
===========

Creating a better life

This repository exists as a complete library that can be used to create various representations of the "Game of Life" originally created by Graham Hutton.

It also serves as a working example for the HERMIT GHC Plugin.  

View the examples directory for examples of how to set up a "Game of Life" program using the Life libraries.  

  -- start GHCi with "ghci examples/HuttonConsole.hs" from the better-life directory, then type "main" to see the orignal "Game of Life" running with a "glider" formation as the start.
  
  -- for a compiled version that uses blank-canvas, you can use the make file, type "make HuttonCanvas" from the better-life directory then "./examples/HuttonCanvas" to run the program.
      The game can then be viewed in a browser at localhost:3000.  

View the sandbox directory for the HERMIT conversion example.  

  -- With GHC > 7.8 and the latest build of HERMIT installed,
      From the better-life directory type "make HermitSetCanvas" (or one of the other examples in the make file)
      This will produce a binary that has been transformed by HERMIT
      The transformation takes the original list-based implementation and replaces it with another implementation.  
      

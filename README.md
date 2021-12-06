# Str8ts
This is an attempt to solve the game [Str8ts](https://www.janko.at/Raetsel/Straights/index.htm).
It uses backtracking to solve the problem.
## Requirements
- [ghc 8.4.4](https://www.haskell.org/ghc/download_ghc_8_8_4.html)
## How to run
- There is a script to run. In the project root's folder, run ./scripts/run.sh
- You can also run it from the terminal following the commands:
    - ghc --make main.hs -o program
    - ./program
## Informations
- The table must be set statically via code. It accepts any table of size bigger than 0
- The static cells are represented by -1
- The empty cells are represented by 0
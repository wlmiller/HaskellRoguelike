Haskell Roguelike
===

This is the beginning of a roguelike programmed in Haskell.  It is intended as a challeng/learning exercise, and will likely be perpetually incomplete.

![Haskell Roguelike](roguelike.png)

## Playing
Download an install a Haskell compiler (like [GHC](https://www.haskell.org/ghc/)).  The game can be run either interactively from the GHCi prompt, or compiled.

### GHCi
From the command line, launch `ghci`.  At the prompt, type:
```Haskell
Prelude> :load RogueLike.hs
*Main> main
```

### Compilation
From the command line, execute:
```Bash
ghc RogueLike.hs
```
Launch the resulting compiled file.

### Game display
Below are listed the characters which are displayed on the screen and what they represent:
- `@`: player character
- `>`: level entrance
- `<`: level exit (a new level is generated when the player is placed on this character)
- `.`: empty floor
- `#`: wall

### Controls
The controls are as follows:
- `W`: move up
- `A`: move left
- `S`: move down
- `D`: move right
- `P`: exit

__Note__: GHCi does not support buffer-free input in Windows; currently, you must press `Enter` to have the game interpret your input.  Entering a string of characters and then pressing `Enter` will result in the character executing each action in the string in sequence!

## TODO:
- [x] Movement of the player character
- [x] Line-of-sight calculation and indication on the level map
- [x] Automatic generation of random level maps
- [ ] Player statistics; e.g. health
- [ ] Non-player characters
- [ ] Battle
- [ ] Items


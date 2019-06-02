# haskoban
The classic game of Sokoban (倉庫番 sōko-ban, "warehouse keeper") written in Haskell.

### Installation
Currently suppported platforms are Linux and MacOS. Installation requires the Haskell Stack build tool. To 
install Stack:
```
curl -sSL https://get.haskellstack.org/ | sh
```
To install Haskoban:
```
git clone https://github.com/peeley/haskoban.git
cd haskoban
stack build
stack install
```
If an error regarding file permissions is returned by the build or install command, pass the 
`--allow-different-user` flag or run as root.

Within the haskoban directory, simply run `haskoban` to start playing!

### How to Play
As a warehouse keeper ('@'), your job is to organize the warehouse by sliding crates ('o') into their 
respective slots ('v'). Move your character using WASD, and push boulders by walking 
into them while their path is clear. The game is won when all the slots are filled.

### Options & Custom Levels
All levels are stored in plaintext in the user's home directory at `~/.haskoban/levels`. Custom levels can be
written in the following format:
```
First line: Width of level in # tiles
Second line: Height of level in # tiles
Rest of file: level written out where each tile is represented as:
    '|' or '-' : wall
    '0' : crate
    '^' : slot
    '@' : player start position
```
To play a specific level, pass the `--level` or `-l` with the desired level filename as its argument.
Currently, the levels are largely borrowed from the ASCII roguelike [Nethack](https://www.nethack.org/),
which features a number of Sokoban-style puzzle levels. Contributions of custom level designs are greatly welcomed,
and level sharing features are planned for the future.

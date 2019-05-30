# haskoban
The classic game of Sokoban (倉庫番 sōko-ban, "warehouse keeper") written in Haskell.

### Installation
Currently suppported platforms are Linux and MacOS. Installation requires the Haskell Stack build tool. To install Stack:
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
If an error regarding file permissions is returned by the build or install command, pass the `--allow-different-user`
flag or run as root.

Within the haskoban directory, simply run `haskoban` to start playing!

### How to Play
As a warehouse keeper, your job is to organize the warehouse by  sliding crates into their respective slots
in the warehouse. Control the your worker ('@') using WASD, and push boulders by walking into them while their 
path is clear. The game is won when there are no slots remaining.

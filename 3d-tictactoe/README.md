# 3D TicTacToe

3D TicTacToe is a 3-dimensional version of the classic tic-tac-toe game. It involves three classic tic-tac-toe board stacked on top of each other and additional three-in-a-rows that span those three boards. It also determines the winner of the game not by who reaches a three-in-a-row first, but who has the most three-in-a-rows by the time all the cells in the 3-dimensional board are filled. 

This game is a Cornell University CS 3110 final project.

## Installation

This game requires OCaml and opam, OCaml Graphics and CamlImages. If OCaml and opam are not installed, please click this link and follow the listed instructions: http://www.cs.cornell.edu/courses/cs3110/2018sp/install.html

X11/XQuartz are also needed for Graphics support. If homebrew is installed, it can be installed by running

```brew install Caskroom/cask/xquartz```

```brew reinstall ocaml --with-x11```

Then, map opam to use the system installation by running

`opam switch sys`
 
Then run 

`eval opam config env`

GTK will also be needed for CamlImages. To install GTK, run

`sudo apt-get install gtk2.0`

In order to install the Graphics module and CamlImages, run

`opam install graphics`

`opam install camlimages`

## Running the game

### Starting the game

Run `make play` in order to launch the GUI and begin playing the game. The first screen will detail the rules of the game. Click anywhere in order to continue and a welcome screen should appear.

![Rules Screen](imgs/rules_screen.png "Rules Screen")

If you wish to only compile, run `make compile`.

### Welcome screen

One the welcome screen there will be option for three things:
* Level
* Mode
* Number of players

![Welcome Screen](imgs/welcome_screen.png "Welcome Screen")

If the number of players is specified to be Single, then the mode will determine the AI that will be the player's opponent.
There are two modes: normal and krazy. Normal functions as a classic tic-tac-toe game while krazy mode involves special features such as bomb, switching planes, etc.

### Testing, Clean
Run `make test` to run the tests included in the project.
Run `make clean` to remove build files.

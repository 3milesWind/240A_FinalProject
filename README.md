CSE230 Project Proposal

# How to run this game
In the root of this project <br />
stack build <br />
stack run <br />
# Demo

<div align=center><img width="150" height="150" src="https://github.com/3milesWind/CSE230_FinalProject/blob/main/demo/demo.png"/></div>

# Descriptions: 

You are an adventurer. You need to set off from the castle to save the kidnapped princess. Because the situation is very urgent, you have to get there in a certain number of steps. You might meet monsters and rocks on the way. You can kill monsters and move rocks(rocks can't be destroyed), but these operations will also consume a few steps. If you fail to save the princess before using up all the steps, then you lose, otherwise you win. 

In the game, there will be four elements: adventurer(you), princess, monster and rocks. 

### Player:
When the game starts, the adventurer can start to move. Each time, it can only move one step. 

### Princess:
When the game starts, the princess will be some place in the game. It will never move. All it can do is to wait for adventurer


### Monster, Rock:
As for Monster and Rock, when they are not by the wall, the adventurer can push them forward one square with the cost of one step. When they are by the wall, the wall will prevent the adventurer from continuing to push them forward. However, in this kind of situation, the adventurer can kill the Monster with the cost of one step(the Rock can not be destroyed). 

### Trap:
The trap may be hidden under other elements(rock, monster...). If the adventurer step onto it,  the step will be decreased by 2





# Possible libraries: 

Control Monad library <br />
Data library    <br />
Brick library   <br />
System Random library <br />

# Team members (Name, Email, Github_id): 

Shanchuan You, shy228@ucsd.edu, shy228 <br />
Guoyi Li, gul008@ucsd.edu , 3milesWind  <br />
Mingyang Chen, mic016@ucsd.edu, MY-Chen2000                          <br />
Jiaen Yu, jiy037@ucsd.edu, yujiaen1999  <br />

# Github Repository Link: 
https://github.com/3milesWind/CSE230_FinalProject


# Milestone 2:

### What is the architecture of your application (the key components)?
Firstly, we initialized a player (adventurer) in the boundary map, which can move in four directions (MyNorth; MySouth; MyEast; MyWest) inside the map, And the location of the player will be modified via I/O. Each time the player move, we check the status of [_win] [_gameOver] to check if the game is live or dead. We used [_stepsRemain] to record the remaining steps restriction of each game, and each step of movement, hit the monster or push the rock/monster will consume a step.

Also, we initialized the location of the princess as the goal position of our game. If the player arrives the position of the princess before run out of all the steps, we win.

Secondly, We initialized the outrange of the board with [_unwalkable] parameter in MyGame.hs to fit each level of the game to guarantee the game is playability and chanllenging.

As for the Rock and Monsters, we set the location of the them by [rockLocation] and [monsterLocation] in MyGame.hs. In each step of movement, it will also check if there is a rock/monster exist in the next position (by [moves]) and if there is movable (by [movable]), if that’s all true, the player will push the rock to move forward for one step without move itself.

We added a function to quit or restart the game in UI.hs. If we input “q”, it will decide to quite the game, and if it input a “r”, the game will return to the initialized status. And we also added Level.hs to select the level of the game to play.

### What challenges (if any) did you have so far and how did you solve them?
Challenge: the game just has the most simple UI now, which is just made up of squares of different colors. We still need to spend some time to find a proper way to embellish the game’s UI design.

Solution: We are trying to use some png picture to beautify our UI, such as proper png picture for the elements in the game.

### Do you expect to meet your goals until the deadline?
Yes, we expect to meet all the description in the previous project proposal, including the movement of adventurer, map restriction, and functions of rock/monster, etc.

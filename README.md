CSE230 Project Proposal

# Descriptions: 

You are an adventurer. You need to set off from the castle to save the kidnapped princess. Because the situation is very urgent, you have to get there in a certain number of steps. You might meet monsters and rocks on the way. You can kill monsters and move rocks(rocks can't be destroyed), but these operations will also consume a few steps. If you fail to save the princess before using up all the steps, then you lose, otherwise you win. 

In the game, there will be four elements: adventurer(you), princess, monster and rocks. 

### Adventurer:
When the game starts, the adventurer can start to move. Each time, it can only move one step. 

### Princess:
When the game starts, the princess will be some place in the game. It will never move. All it can do is to wait for adventurer

### Monster and Rock:
As for Monster and Rock, when they are not by the wall, the adventurer can push them forward one square with the cost of one step. When they are by the wall, the wall will prevent the adventurer from continuing to push them forward. However, in this kind of situation, the adventurer can kill the Monster(the Rock can not be destroyed).



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

# Game design:
Game {
    _steps_remain :: Int
    _princess :: Coord
    _monster :: Coord
    _rock :: Rock
    _dir :: Direction
    _dead :: Bool
    _player :: Coord
    
}


# Milestone 2:

### What is the architecture of your application (the key components)?
Firstly, we initialized a player (adventurer) in the board with borders, which can move in four directions (MyNorth; MySouth; MyEast; MyWest) inside the board, And the location of the player will be modified via I/O. Each time the player move, we check the status of [_win] [_gameOver] to check if the game is live or dead. We used [_stepsRemain] to record the remaining steps restriction of each game, and each step of movement, hit the monster or push the rock will consume a step.

Also, we initialized the location of the princess as the goal position of our game.

Secondly, We initialized the outrange of the board with [_unwalkable] parameter in MyGame.hs to fit each level of the game to guarantee the game is playability and fun.

As for the Rock and Monsters, we set the location of the them by [rockLocation] and [monsterLocation] in MyGame.hs. In each step of movement, it will also check if there is a rock/monster exist in the next position (by [moves]) and if there is movable (by [movable]), if that’s all true, the player will push the rock to move forward for one step without move itself.

We added a function to quit or restart the game in UI.hs. If we input “q”, it will decide to quite the game, and if it input a “r”, the game will return to the initialized status. And we also added Level.hs to select the level of the game to play.

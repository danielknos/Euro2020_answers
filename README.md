# Euro 2020 answers

A shinyapp to calculate the scores based on predictions from my other app (https://github.com/danielknos/Euro2020_tip).
The app needs to have the following files in the subfolder Data.
- One csv file per competitor
- results.csv - One csv file called results (identical to the competitors' files). This file should be updated with the actual scores as the tournaments is played. Unplayed games re not included so their scores are irrelevant. It's probably easiest to not add rows as the torunament is played but rather just keep all games 0-0 and then update the results as you go. Keep the topscorr cell blank until here is an actual winner.
- topscorers.csv - A csv file called topscorers with columns Player, Goals. The code matches the player column with the competitors guesses so it's important that they're identical. It probably needs some cleanssing of the files work perfect
- scoring.csv - A file that defines he number of points for different metrics, it can be changed here if you want to have other scores than my predefined ones.

I have not managed to find a good API to have the results live feed into the app, so currently I'm updating the results file daily (or whenever there has been games). Will have a more detailed look for an automatic feed which would be helpful
In the server file the variable 'today' is defined as a date. This should normally be today's date but can also be other dates. The scoring only calculates the scores for the matches that has been played prior to this date (and time).

The menus pretty much speak for themselves, you can filter between users etc. The only one that needs some description is the Simulator (Group matches -> Simulator). This one allows users to manually add results in the unplayed games (grey cells) to see the scoring outcome given the manually added scores. This only works for the group stage. To reset you can change 'Simulate days forward' to 0 and reclick Simulate, or jut refresh the window

To quickly run and have a look please run the following r code

```
library(shiny)
runGitHub('danielknos/Euro2020_answers')
```
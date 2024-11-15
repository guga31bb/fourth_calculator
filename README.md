Fourth down calculator
================

This is the repository for the [Shiny
app](https://rbsdm.com/stats/fourth_calculator) and [4th down
bot](http://twitter.com/ben_bot_baldwin) introduced in [this piece on
The
Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/).

The code that powers the Twitter fourth down bot [is in this folder
here](https://github.com/guga31bb/fourth_calculator/tree/main/bot).

The website and bot use [`nfl4th`](https://www.nfl4th.com/) to calculate
the probabilities. Research using the package [can be found
here](https://www.nfl4th.com/articles/articles/4th-down-research.html).

## Models

Here is the code that generates the various models:

  - [Going for 4th down and going for 2
    points](https://github.com/guga31bb/fourth_calculator/blob/main/R/_go_for_it_model.R)
  - [Punting and kicking a field
    goal](https://github.com/guga31bb/fourth_calculator/blob/main/R/punts.R)

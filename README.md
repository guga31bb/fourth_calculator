# Fourth down calculator

This is the repository for the [fourth down calculator](https://rbsdm.com/stats/fourth_calculator) introduced in [this piece on The Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/). Here are the main files of interest:

* [Model for yards gained on fourth down go decision](https://github.com/guga31bb/fourth_calculator/blob/main/R/_go_for_it_model.R)
* [Modeling punt and field goal outcomes](https://github.com/guga31bb/fourth_calculator/blob/main/R/punts.R)
* [Code for generating the figures and tables in The Athletic piece](https://github.com/guga31bb/fourth_calculator/blob/main/R/_the_athletic_post.R)
* [The logic of the actual Shiny app](https://github.com/guga31bb/fourth_calculator/blob/main/app.R)
* [The actual functions that do the win probability calculations](https://github.com/guga31bb/fourth_calculator/blob/main/R/helpers.R)

The code that powers the Twitter fourth down bot [is in this folder here](https://github.com/guga31bb/fourth_calculator/tree/main/bot).

## Features

* The **go for it** model gives probabilities for possibilities of yards gained and includes the possibility of earning a first down via defensive penalty
* The **punt** model includes the possibility for getting blocked, returned for a touchdown, or fumbled on the return
* The **field goal** model is a simple model of field goal % by distance and roof type

## Current limitations

There are some edge cases that are not accounted for. These should only make a marginal difference to the recommendations as they are largely edge cases (e.g. the possibility for a field goal to be blocked and returned).

* The **go for it** model does not allow for the possibility of a turnover return. However, long returns are extremely rare: For example, in 2018 and 2019 there were only four defensive touchdowns on plays where teams went for fourth downs out of 1,236 plays, and all of these happened when the game was well in hand for the other team.
* The **punt** model doesn’t account for the punter or returner, ignores penalties on returns and ignores the potential for blocked punts to be returned for touchdowns
* The **field goal** model doesn’t account for who the kicker is, what the weather is (only relevant for outdoor games), or the possibility of a kick being blocked and returned for a touchdown
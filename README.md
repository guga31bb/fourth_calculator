# Fourth down calculator

This is the repository for the [fourth down calculator](https://rbsdm.com/stats/fourth_calculator) introduced in [this piece on The Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/). Here are the main files of interest:

* [Model for yards gained on fourth down go decision](https://github.com/guga31bb/fourth_calculator/blob/main/R/_go_for_it_model.R)
* [Modeling punt and field goal outcomes](https://github.com/guga31bb/fourth_calculator/blob/main/R/punts.R)
* [Code for generating the figures and tables in The Athletic piece](https://github.com/guga31bb/fourth_calculator/blob/main/R/_the_athletic_post.R)
* [The logic of the actual Shiny app](https://github.com/guga31bb/fourth_calculator/blob/main/app.R)
* [The actual functions that do the win probability calculations](https://github.com/guga31bb/fourth_calculator/blob/main/R/helpers.R)

The code that powers the Twitter fourth down bot [is in this folder here](https://github.com/guga31bb/fourth_calculator/tree/main/bot).
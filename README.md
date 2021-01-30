# Fourth down calculator

This is the repository for the [fourth down calculator](https://rbsdm.com/stats/fourth_calculator) introduced in [this piece on The Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/). Here are the main files of interest:

* [Model for yards gained on fourth down go decision](https://github.com/guga31bb/fourth_calculator/blob/main/R/_go_for_it_model.R)
* [Modeling punt and field goal outcomes](https://github.com/guga31bb/fourth_calculator/blob/main/R/punts.R)
* [Code for generating the figures and tables in The Athletic piece](https://github.com/guga31bb/fourth_calculator/blob/main/R/_the_athletic_post.R)
* [The logic of the Shiny app](https://github.com/guga31bb/fourth_calculator/blob/main/app.R) which is located [here](https://rbsdm.com/stats/fourth_calculator/)
* [The functions that do the win probability calculations](https://github.com/guga31bb/fourth_calculator/blob/main/R/helpers.R)

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

## Example usage 1: user input

Here is the code that can look up one play. This is the controversial field goal attempt that the Packers attempted at the end of the 2020 NFC Championship Game.

``` r
source('R/helpers.R')

# get the situation from user input
df <- 
    tibble::tibble(
      
      # reg or post
      "type" = "post",
      
      "qtr" = 4,
      
      # since user input is mins and seconds
      "time" = 60 * as.integer(2) + as.integer(9),
      
      'posteam' = as.character("GB"),
      'away_team' = as.character("TB"),
      'home_team' = as.character("GB"),
      'yardline_100' = as.integer(8),
      'ydstogo' = as.integer(8),
      'posteam_timeouts_remaining' = as.integer(3),
      'defteam_timeouts_remaining' = as.integer(3),
      
      # did home team receive opening kickoff?
      'home_opening_kickoff' = as.integer(0),
      
      'score_differential' = as.integer(-8),
      
      # user input for additional runoff in seconds after successful 4th down conversion
      'runoff' = as.integer(0),
      
      # season that the game happened in
      'yr' = as.integer(2020)
    ) %>%
      # helper function to look up point spread and roof type for given game
      prepare_df(games)

# main function in R/helpers.R
# punt_df is automatically loaded by the source line above and contains the distribution of punts

make_table_data(df, punt_df)

# A tibble: 3 x 5
  choice             choice_prob success_prob fail_wp success_wp
  <chr>                    <dbl>        <dbl>   <dbl>      <dbl>
1 Go for it                12.7          32.7    3.53      31.5 
2 Field goal attempt        8.90         97.5    2.99       9.05
3 Punt                     NA            NA     NA         NA 
```

## Example usage 2 : from an nflfastR play

Typing in all the situation variables is a pain. Here's how to get the table from a play that already exists in nflfastR data:

``` r
# helper function that prepares data and most importantly creates indicator for 2nd half kickoff team
source("R/season_numbers_functions.R")
source('R/helpers.R')

# get the play (I cheated and already looked up play ID)
pbp <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")) %>%
  prepare_data() %>%
  filter(week == 20, posteam == "GB", play_id == 3728) %>%
  prepare_df(games) %>%
  # this is a bug where not doing this causes weird behavior. idk
  select(names(df))

make_table_data(pbp, punt_df)

# A tibble: 3 x 5
  choice             choice_prob success_prob fail_wp success_wp
  <chr>                    <dbl>        <dbl>   <dbl>      <dbl>
1 Go for it                12.7          32.7    3.53      31.5 
2 Field goal attempt        8.90         97.5    2.99       9.05
3 Punt                     NA            NA     NA         NA   
```
## Example usage 3 : make the table

Here's how to get the actual table on the shiny app and tweeted out by the bot.
```
make_table(make_table_data(pbp, punt_df), pbp)
 
```
![https://pbs.twimg.com/media/Ess2ZxrXAAUgRtZ?format=png&name=small](https://pbs.twimg.com/media/Ess2ZxrXAAUgRtZ?format=png&name=small)
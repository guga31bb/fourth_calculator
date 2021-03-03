Fourth down calculator
================

This is the repository for the [fourth down
calculator](https://rbsdm.com/stats/fourth_calculator) introduced in
[this piece on The
Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/).
Here are the main files of interest:

  - [Model for yards gained on fourth down go
    decision](https://github.com/guga31bb/fourth_calculator/blob/main/R/_go_for_it_model.R)
  - [Modeling punt and field goal
    outcomes](https://github.com/guga31bb/fourth_calculator/blob/main/R/punts.R)
  - [Code for generating the figures and tables in The Athletic
    piece](https://github.com/guga31bb/fourth_calculator/blob/main/R/_the_athletic_post.R)
  - [The logic of the Shiny
    app](https://github.com/guga31bb/fourth_calculator/blob/main/app.R)
    which is located [here](https://rbsdm.com/stats/fourth_calculator/)
  - [The functions that do the win probability
    calculations](https://github.com/guga31bb/fourth_calculator/blob/main/R/helpers.R)

The code that powers the Twitter fourth down bot [is in this folder
here](https://github.com/guga31bb/fourth_calculator/tree/main/bot).

To get starting using the code, I would recommend **cloning this
repository** (which gets the dataframes with 4th down calculations
already applied) and then going through [my file analyzing the
data](https://github.com/guga31bb/fourth_calculator/blob/main/R/season_numbers.R).

## Features

  - The **go for it** model gives probabilities for possibilities of
    yards gained and includes the possibility of earning a first down
    via defensive penalty
  - The **punt** model includes the possibility for getting blocked,
    returned for a touchdown, or fumbled on the return
  - The **field goal** model is a simple model of field goal % by
    distance and roof type

## Current limitations

There are some edge cases that are not accounted for. These should only
make a marginal difference to the recommendations as they are largely
edge cases (e.g. the possibility for a field goal to be blocked and
returned).

  - The **go for it** model does not allow for the possibility of a
    turnover return. However, long returns are extremely rare: For
    example, in 2018 and 2019 there were only four defensive touchdowns
    on plays where teams went for fourth downs out of 1,236 plays, and
    all of these happened when the game was well in hand for the other
    team.
  - The **punt** model doesn’t account for the punter or returner,
    ignores penalties on returns and ignores the potential for blocked
    punts to be returned for touchdowns
  - The **field goal** model doesn’t account for who the kicker is, what
    the weather is (only relevant for outdoor games), or the possibility
    of a kick being blocked and returned for a touchdown

## Example usage 1: from nflfastR data

Here is the code that can look up one play. This is the controversial
field goal attempt that the Packers attempted at the end of the 2020 NFC
Championship Game.

``` r
source('https://raw.githubusercontent.com/guga31bb/fourth_calculator/main/R/helpers.R')
# get the play
pbp <- nflfastR::load_pbp(2020) %>%
  prepare_nflfastr_data() %>%
  filter(week == 20, posteam == "GB", qtr == 4, ydstogo == 8) %>%
  prepare_df()

make_table_data(pbp) %>%
  knitr::kable(digits = 1)
```

| choice             | choice\_prob | success\_prob | fail\_wp | success\_wp |
| :----------------- | -----------: | ------------: | -------: | ----------: |
| Go for it          |         12.7 |          32.7 |      3.5 |        31.5 |
| Field goal attempt |          8.9 |          97.5 |      3.0 |         9.0 |
| Punt               |           NA |            NA |       NA |          NA |

## Example usage 2 : make the table

Here’s how to get the actual table on the shiny app and tweeted out by
the bot.

``` r
make_table(make_table_data(pbp), pbp)
```

![<https://pbs.twimg.com/media/Ess2ZxrXAAUgRtZ?format=png&name=small>](https://pbs.twimg.com/media/Ess2ZxrXAAUgRtZ?format=png&name=small)

## Example usage 3 : get results for a bunch of plays

The first two functions do the cleaning steps and then `add_probs` is a
wrapper that adds all the probabilities associated with each choice,
with the added columns shown below:

``` r
source('https://raw.githubusercontent.com/guga31bb/fourth_calculator/main/R/helpers.R')
nflfastR::load_pbp(2020) %>%
      prepare_nflfastr_data() %>%
      prepare_df() %>%
      add_probs() %>%
  dplyr::slice(1:10) %>%
  select(
    posteam, ydstogo, yardline_100, posteam, first_down_prob, wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, fg_wp, punt_wp
  ) %>%
  knitr::kable(digits = 2)
```

| posteam | ydstogo | yardline\_100 | first\_down\_prob | wp\_fail | wp\_succeed | go\_wp | fg\_make\_prob | miss\_fg\_wp | make\_fg\_wp | fg\_wp | punt\_wp |
| :------ | ------: | ------------: | ----------------: | -------: | ----------: | -----: | -------------: | -----------: | -----------: | -----: | -------: |
| SF      |       3 |            34 |              0.55 |     0.69 |        0.78 |   0.74 |           0.62 |         0.69 |         0.75 |   0.73 |     0.72 |
| ARI     |      10 |            65 |              0.27 |     0.18 |        0.28 |   0.21 |           0.00 |         0.18 |         0.29 |   0.18 |     0.24 |
| ARI     |       7 |            72 |              0.38 |     0.07 |        0.14 |   0.10 |           0.00 |         0.07 |         0.15 |   0.07 |     0.10 |
| SF      |       5 |            64 |              0.48 |     0.84 |        0.92 |   0.88 |           0.00 |         0.83 |         0.92 |   0.83 |     0.87 |
| SF      |       3 |            68 |              0.55 |     0.65 |        0.81 |   0.74 |           0.00 |         0.62 |         0.80 |   0.62 |     0.72 |
| ARI     |       9 |            77 |              0.30 |     0.16 |        0.28 |   0.19 |           0.00 |         0.15 |         0.28 |   0.15 |     0.20 |
| SF      |       1 |             1 |              0.63 |     0.78 |        0.89 |   0.85 |           0.99 |         0.76 |         0.82 |   0.82 |       NA |
| ARI     |       5 |            34 |              0.45 |     0.19 |        0.37 |   0.27 |           0.62 |         0.19 |         0.31 |   0.27 |     0.23 |
| SF      |       9 |            36 |              0.32 |     0.72 |        0.84 |   0.76 |           0.57 |         0.71 |         0.80 |   0.76 |     0.76 |
| SF      |       2 |             6 |              0.54 |     0.77 |        0.89 |   0.83 |           0.98 |         0.75 |         0.81 |   0.81 |       NA |

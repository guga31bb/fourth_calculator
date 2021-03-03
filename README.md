Readme
================

# Fourth down calculator

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
source('R/helpers.R')
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

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ujyxidckyv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: black;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: white;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ujyxidckyv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ujyxidckyv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ujyxidckyv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ujyxidckyv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ujyxidckyv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ujyxidckyv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ujyxidckyv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ujyxidckyv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ujyxidckyv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ujyxidckyv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ujyxidckyv .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: black;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ujyxidckyv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: black;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  vertical-align: middle;
}

#ujyxidckyv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ujyxidckyv .gt_from_md > :first-child {
  margin-top: 0;
}

#ujyxidckyv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ujyxidckyv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: white;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ujyxidckyv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ujyxidckyv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ujyxidckyv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ujyxidckyv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ujyxidckyv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ujyxidckyv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ujyxidckyv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ujyxidckyv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ujyxidckyv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ujyxidckyv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ujyxidckyv .gt_left {
  text-align: left;
}

#ujyxidckyv .gt_center {
  text-align: center;
}

#ujyxidckyv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ujyxidckyv .gt_font_normal {
  font-weight: normal;
}

#ujyxidckyv .gt_font_bold {
  font-weight: bold;
}

#ujyxidckyv .gt_font_italic {
  font-style: italic;
}

#ujyxidckyv .gt_super {
  font-size: 65%;
}

#ujyxidckyv .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="ujyxidckyv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="5" class="gt_heading gt_title gt_font_normal" style>

Down 8, 4th & 8, 8 yards from opponent end zone

</th>

</tr>

<tr>

<th colspan="5" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

Qtr 4, 02:09 | Timeouts: Off 3, Def 3

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1" style="color: black; font-weight: bold;">

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1" style="color: black; font-weight: bold;">

Win %

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1" style="color: black; font-weight: bold;">

Success %<sup class="gt_footnote_marks">1</sup>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Win % if</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: black; font-weight: bold;">

Fail

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: black; font-weight: bold;">

Succeed

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left" style="font-weight: bold;">

Go for it

</td>

<td class="gt_row gt_center" style="color: red; font-weight: bold;">

13

</td>

<td class="gt_row gt_center">

33

</td>

<td class="gt_row gt_center">

4

</td>

<td class="gt_row gt_center">

31

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="font-weight: bold;">

Field goal attempt

</td>

<td class="gt_row gt_center" style="color: red; font-weight: bold;">

9

</td>

<td class="gt_row gt_center">

98

</td>

<td class="gt_row gt_center">

3

</td>

<td class="gt_row gt_center">

9

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="font-weight: bold;">

Punt

</td>

<td class="gt_row gt_center" style="color: red; font-weight: bold;">

NA

</td>

<td class="gt_row gt_center">

NA

</td>

<td class="gt_row gt_center">

NA

</td>

<td class="gt_row gt_center">

NA

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="5">

<strong>Source</strong>: @ben\_bot\_baldwin

</td>

</tr>

</tfoot>

<tfoot>

<tr class="gt_footnotes">

<td colspan="5">

<p class="gt_footnote">

<sup class="gt_footnote_marks"> <em>1</em> </sup>

Likelihood of converting on 4th down or of making field goal <br />

</p>

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

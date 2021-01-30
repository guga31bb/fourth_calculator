# Fourth down bot

This is the code that powers the [fourth down bot](https://twitter.com/ben_bot_baldwin). Here are the main files of interest:
  
* [High level bot code](https://github.com/guga31bb/fourth_calculator/blob/main/bot/bot.R). This runs every minute on my server
    * Checks for live games
    * Maintains list of already-tweeted plays
    * Tweets out new plays
* [Helper functions for bot](https://github.com/guga31bb/fourth_calculator/blob/main/bot/bot_functions.R). 
    * Function to get play-by-play from the ESPN API 
    * Function to construct the tweet to be sent out



library(rtweet)

client <- rtweet::client_as("/srv/shiny-server/box_scores/Baldwin bot.rds")
oauth2 <- rtweet::auth_as("/srv/shiny-server/box_scores/oauth2.rds")

rtweet::auth_save(oauth2, "my_oauth2") 
rtweet::client_save(client)


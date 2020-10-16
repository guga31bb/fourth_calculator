# to-do

# fg can't be inside 20
# https://twitter.com/fbgchase/status/1316157100834381826


source('R/helpers.R')
library(shiny)
library(shinythemes)

# get teams and logos
teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

# team abbreviations for dropdown menus
ids <- teams %>%
  pull(team_abbr)
  

#####################################################################################
######## Define UI for viewer app ###########################################
#####################################################################################

ui <- function(request) {
  
  fluidPage(
    
    theme = shinytheme("cerulean"),
    # theme = shinytheme("sandstone"),
    # shinythemes::themeSelector(),
    
    # App title ----
    titlePanel("Fourth Down Decision Calculator"),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      navbarPage("rbsdm.com/stats",
                 tabPanel("Result",

                          fluidRow(
                            column(12, align = "center",
                                   bookmarkButton()
                            )),
                          fluidRow(
                            column(4, align = "center",
                                   
                                   tags$h3("Game"),

                                   selectInput(
                                     inputId =  "season", 
                                     label = "Season:", 
                                     choices = 2018:2020,
                                     selected = 2020
                                   ),

                                   selectInput("posteam",
                                               "Offense:",
                                               c(sort(unique(as.character(ids)))), selected = "TB"),
                                   
                                   selectInput("away",
                                               "Away team:",
                                               c(sort(unique(as.character(ids)))), selected = "TB"),
                                   
                                   selectInput("home",
                                               "Home team:",
                                               c(sort(unique(as.character(ids)))), selected = "CHI"),
                                   
                                   radioButtons("home_ko",
                                                label = "Did home team get opening kickoff?:",
                                                choices = list("0" = 0, "1" = 1),
                                                inline = T,
                                                selected = "0")
                                   
                                   
                                   ),
                            column(4, align = "center",
                                   
                                   tags$h3("Time"),
                                   radioButtons("qtr",
                                                label = "Quarter:",
                                                choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4),
                                                inline = T,
                                                selected = "4"),
                                   
                                   sliderInput(
                                     inputId =  "mins", 
                                     label = "Minutes left in quarter:", 
                                     min = 0, max = 15,
                                     value = 4
                                   ),
                                   
                                   sliderInput(
                                     inputId =  "secs", 
                                     label = "Seconds left in quarter:", 
                                     min = 0, max = 59,
                                     value = 52
                                   ),

                                   radioButtons("posteam_to",
                                                label = "Offense timeouts:",
                                                choices = list("0" = 0, "1" = 1, "2" = 2, "3" = 3),
                                                inline = T,
                                                selected = "3"),
                                   
                                   radioButtons("defteam_to",
                                                label = "Defense timeouts:",
                                                choices = list("0" = 0, "1" = 1, "2" = 2, "3" = 3),
                                                inline = T,
                                                selected = "3"),
                                   
                                   
                                   
                                   
                                   
                            ),
                            
                            column(4, align = "center",
                                   
                                   tags$h3("Situation"),

                                   sliderInput(
                                     inputId =  "ydstogo", 
                                     label = "Yards to go:", 
                                     min = 1, max = 30,
                                     value = 1
                                   ),
                                   
                                   sliderInput(
                                     inputId =  "yardline", 
                                     label = "Yards from opponent EZ:", 
                                     min = 1, max = 90,
                                     value = 7
                                   ),
                                   
                                   sliderInput(
                                     inputId =  "score_diff", 
                                     label = "Score differential:", 
                                     min = -28, max = 28,
                                     value = -2
                                   ),
                                   
                                   sliderInput(
                                     inputId =  "runoff", 
                                     label = "Advanced: additional seconds to run off clock after successful conversion", 
                                     min = 0, max = 40,
                                     value = 0
                                   )
                                   
                                   
                                   )

                          ),
                          
                          fluidRow(
                            column(12, align = "center",
                                   actionButton("update", "Update", width = '50%')
                            )),

                          fluidRow(
                            column(12, align = "center",
                                   tags$br(),
                                   tags$br(),
                                   
                            htmlOutput("picture"),
                            htmlOutput("some_text")
                            
                                   )
                          ),
                          
                          fluidRow(
                            
                            column(12, align="center", 
                                   gt_output(outputId = "view1") %>% 
                                     shinycssloaders::withSpinner(type = 6,# types see https://projects.lukehaas.me/css-loaders/ 
                                                                  color = "#414141",
                                                                  color.background = "#FFFFFF")
                            )
                          ),
                          
                          tags$br(),
                          
                          tags$p("Notes: these are ESTIMATES; please use accordingly. On 4th and 1, the model cannot know whether it's a long 1 or a short 1. Shorter distance would favor going. Do not use this in overtime.")
                          

                          
                          
                 ),
                 
                 tabPanel("About",
                          p("Notes: Data from @nflfastR. A thank you to Lee Sharpe (@LeeSharpeNFL) for hosting an updating source for game results. Website by Ben Baldwin (@benbbaldwin) and Sebastian Carl (@mrcaseb).")
                          
                 )
                 
      )
      
      
      
      
    )
    
    
  )
  
}

#####################################################################################
######## Define server app ###########################################
#####################################################################################
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button


  # input <- NULL
  # input$qtr <- 4
  # input$mins <- 2
  # input$secs <- 0
  # input$posteam <- "MIN"
  # input$away <- "MIN"
  # input$home <- "SEA"
  # input$yardline <- 6
  # input$ydstogo <- 1
  # input$posteam_to <- 2
  # input$defteam_to <- 1
  # input$home_ko <- 0
  # input$score_diff <- 5
  # 
  #nflscrapr: full data
  fullInput <- eventReactive(
    input$update,
    {
 
      tibble::tibble(
        "qtr" = as.integer(input$qtr),
        "time" = 60 * as.integer(input$mins) + as.integer(input$secs),
        'posteam' = as.character(input$posteam),
        'away_team' = as.character(input$away),
        'home_team' = as.character(input$home),
        'yardline_100' = as.integer(input$yardline),
        'ydstogo' = as.integer(input$ydstogo),
        'posteam_timeouts_remaining' = as.integer(input$posteam_to),
        'defteam_timeouts_remaining' = as.integer(input$defteam_to),
        'home_opening_kickoff' = as.integer(input$home_ko),
        'score_differential' = as.integer(input$score_diff),
        'runoff' = as.integer(input$runoff),
        'yr' = as.integer(input$season)
      ) %>%
        prepare_df()
      
    } , ignoreNULL = FALSE
  )
  
  tableData <- eventReactive(
    input$update,
    {
      
      make_table_data(fullInput(), punt_df)
      
    } , ignoreNULL = FALSE
  )
  
  
  #home team summary table
  output$view1 <- render_gt(
      expr = make_table(tableData(), fullInput())
  )
  
  output$picture <-
    renderText({
      c(
        '<img width="150" src="',
        glue::glue("{teams %>% filter(team_abbr == input$posteam) %>% pull(team_logo_espn)}"),
        '">'
      )
    })
  
  output$some_text <- renderText({ 
    

    return(glue::glue("<font size='+2'>Correct choice: <span style='color:red'>{tableData() %>% arrange(-choice_prob) %>% dplyr::slice(1) %>% pull(choice)}</span> (difference: 
      <span style='color:green'> <strong> {round(tableData() %>% arrange(-choice_prob) %>% dplyr::slice(1) %>% pull(choice_prob) - tableData() %>% arrange(-choice_prob) %>% dplyr::slice(2) %>% pull(choice_prob), 1)}%</strong></span>)</font>"))

  })
  
  
}


# Create Shiny app ----
shinyApp(ui, server, enableBookmarking = "url")


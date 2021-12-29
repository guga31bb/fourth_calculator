source('scripts/helpers.R')
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
                                     choices = 2014:2021,
                                     selected = 2020
                                   ),

                                   radioButtons("type",
                                                label = "Type:",
                                                choices = list("Regular" = "reg","Postseason" = "post"),
                                                selected = "reg"),

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
                                     value = -1
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

                          ############ start panel

                          tabsetPanel(type = "tabs", id = "inTabset",
                                      tabPanel("4th down",
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

                                               tags$p("Notes: these are ESTIMATES; please use accordingly. On 4th and 1, the model cannot know whether it's a long 1 or a short 1. Shorter distance would favor going. Do not use this in overtime. Use with EXTREME CAUTION in the final minute of a game as the model is not good with end of game clock scenarios.")


                                               ### end panel




                                      ),
                                      tabPanel("2 point conversions", value = "two_pt",

                                               fluidRow(
                                                 column(12, align = "center",
                                                        tags$br(),
                                                        tags$br(),

                                                        htmlOutput("picture2"),
                                                        htmlOutput("some_text2")

                                                 )
                                               ),
                                               fluidRow(

                                                 column(12, align="center",
                                                        gt_output(outputId = "view2") %>%
                                                          shinycssloaders::withSpinner(type = 6,# types see https://projects.lukehaas.me/css-loaders/
                                                                                       color = "#414141",
                                                                                       color.background = "#FFFFFF")
                                                 )
                                               )

                                               )

                                      )


                          ),
                 tabPanel("About",
                          p("Notes: Data from @nflfastR. A thank you to Lee Sharpe (@LeeSharpeNFL) for hosting an updating source for game results and to Thomas Mock (@thomas_mock) for code to make the table look better. Website by Ben Baldwin (@benbbaldwin). Model writeup coming at some point...")


                                      )



      )




    )


  )

}

#####################################################################################
######## Define server app ###########################################
#####################################################################################
server <- function(session, input, output) {

  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button

  # uncomment only for testing
  # input <- NULL
  # input$qtr <- 4
  # input$mins <- 4
  # input$secs <- 52
  # input$posteam <- "TB"
  # input$away <- "TB"
  # input$home <- "CHI"
  # input$yardline <- 7
  # input$ydstogo <- 1
  # input$posteam_to <- 3
  # input$defteam_to <- 3
  # input$home_ko <- 0
  # input$score_diff <- -1
  # input$type <- "reg"
  # input$runoff <- 0
  # input$season <- 2020


  # get the situation from user input
  fullInput <- eventReactive(
    input$update,
    {

      tibble::tibble(
        "type" = as.character(input$type),
        "qtr" = as.integer(input$qtr),
        "quarter_seconds_remaining" = 60 * as.integer(input$mins) + as.integer(input$secs),
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
        'season' = as.integer(input$season)
      )

    } , ignoreNULL = FALSE
  )

  # get the data that goes in the table
  tableData <- eventReactive(
    input$update,
    {

      fullInput() %>%
        nfl4th::add_4th_probs() %>%
        nfl4th::make_table_data()

    } , ignoreNULL = FALSE
  )

  # make the table
  output$view1 <- render_gt(
      expr = make_table(tableData(), fullInput())
  )

  # get the data that goes in the 2pt table
  tableData2 <- eventReactive(
    input$update,
    {

     fullInput() %>%
        nfl4th::add_2pt_probs() %>%
        nfl4th::make_2pt_table_data()

    } , ignoreNULL = FALSE
  )

  # make the 2pt table
  output$view2 <- render_gt(
    expr = make_table_2pt(tableData2(), fullInput())
  )

  # for the team logo above decision
  output$picture <-
    renderText({
      c(
        '<img width="150" src="',
        glue::glue("{teams %>% filter(team_abbr == input$posteam) %>% pull(team_logo_espn)}"),
        '">'
      )
    })

  output$picture2 <-
    renderText({
      c(
        '<img width="150" src="',
        glue::glue("{teams %>% filter(team_abbr == input$posteam) %>% pull(team_logo_espn)}"),
        '">'
      )
    })


  # say what the right decision is
  output$some_text <- renderText({

    return(glue::glue("<font size='+2'><span style='color:red'>{tableData() %>% arrange(-choice_prob) %>% dplyr::slice(1) %>% pull(choice)}</span> (+
      <span style='color:green'> <strong> {round(tableData() %>% arrange(-choice_prob) %>% dplyr::slice(1) %>% pull(choice_prob) - tableData() %>% arrange(-choice_prob) %>% dplyr::slice(2) %>% pull(choice_prob), 1)}% WP</strong></span>)</font>"))

  })

  output$some_text2 <- renderText({

    return(glue::glue("<font size='+2'><span style='color:red'>{tableData2() %>% arrange(-choice_prob) %>% dplyr::slice(1) %>% pull(choice)}</span> (+
      <span style='color:green'> <strong> {round(tableData2() %>% arrange(-choice_prob) %>% dplyr::slice(1) %>% pull(choice_prob) - tableData2() %>% arrange(-choice_prob) %>% dplyr::slice(2) %>% pull(choice_prob), 1)}% WP</strong></span>)</font>"))

  })


  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    # print(query1)
    # print(substr(query1, (nchar(query1) - 4), (nchar(query1))))
    if(substr(query1, (nchar(query1) - 4), (nchar(query1))) == "2pt=1"){
      updateTabsetPanel(session, "inTabset", selected = "two_pt")
    }
  })


}

# Create Shiny app ----
shinyApp(ui, server, enableBookmarking = "url")


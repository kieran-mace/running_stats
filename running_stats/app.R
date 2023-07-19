#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(googlesheets4)
gs4_deauth()
out_data = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1JUy4_glKBHca1Y6Ia7kBpBYpxc4cGucpQI9WM71Wki0/edit?pli=1#gid=1235039595')

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Running Stats"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_of_interest",
                  "Training Variable of Interest",
                  choices = c('Workouts Per Week', 'Hours of Exercising out Per Week')),
      h6('This data is being read live from this link: https://docs.google.com/spreadsheets/d/1JUy4_glKBHca1Y6Ia7kBpBYpxc4cGucpQI9WM71Wki0/edit?pli=1#gid=1235039595')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("effortPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$effortPlot <- renderPlot({
    # generate bins based on input$bins from ui.R

    if(input$variable_of_interest == 'Workouts Per Week'){
      ggplot(out_data,
             aes(x = weeknum,
                 group = name,
                 color = name,
                 fill = name)) +
        geom_col(aes(y = weekNumberOfWorkouts)) +
        geom_line(aes(y = mile_time)) +
        facet_wrap(vars(name), ncol = 1) +
        labs(x = 'Week of Year',
             y = 'Number of Workouts Per Week (bars), and Mile time (mins)') +
        theme_minimal()
    }
    else
      if(input$variable_of_interest == 'Hours of Exercising out Per Week'){
        ggplot(out_data,
               aes(x = weeknum,
                   group = name,
                   color = name,
                   fill = name)) +
          geom_col(aes(y = weekTimeExercising / (60*60))) +
          geom_line(aes(y = mile_time)) +
          facet_wrap(vars(name), ncol = 1) +
          labs(x = 'Week of Year',
               y = 'Hours of Exercising (bars), and Mile time (mins)') +
          theme_minimal()
      }

  })

}

# Run the application
shinyApp(ui = ui, server = server)

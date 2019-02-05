#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("hydrationCalculator.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Levain details calculator."),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         numericInput("seed_mass",
                     label = "Mature starter seed (g):",
                     min = 0,
                     max = 100,
                     value = 10
                   ),
         numericInput("seed_hydration",
                      label = "Mature starter seed hydration:",
                   min = 0,
                   max = 150,
                   value = 100
         ),
         numericInput("starter_mass",
                      label = "Total mass of new flour and water (g):",
                   min = 0,
                   max = 1000,
                   value = 200
         ),
         numericInput("starter_hydration",
                   label = "Hydration of new flour and water:",
                   min = 0,
                   max = 150,
                   value = 100
         ),
         numericInput("levain_used",
                   label = "Levain used in final dough (g):",
                   min = 0,
                   max = 100,
                   value = 10
         )
      ),

      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("levainInfo")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$levainInfo <- renderTable(
     levainHydration(
       input$seed_mass,
       input$seed_hydration,
       input$starter_mass,
       input$starter_hydration,
       input$levain_used
     )
   )
}

# Run the application
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(ggvis)

source("~/stat133/stat133-hws-fall17/hw04/code/functions.R")

# import data, reorder grades
dt <- read.csv("~/stat133/stat133-hws-fall17/hw04/data/cleandata/cleandata.csv")
mytable <- data.frame(table(dt$grade))
colnames(mytable)[1] <- "Grade"
mytable$prop <- prop.table(table(dt$grade))
target <- c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "F")
mytable <- mytable[match(target, mytable$Grade),]


# create a vector of dataframe names for histograms 
names <- colnames(dt)[2:22]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Grade Visualizer"),
   
   # Sidebar with different widgets depending on selected tab
   sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition = "input.tabselected == 1",
                         h3("Grades Distribution"),
                         tableOutput('table')),
        conditionalPanel(condition = "input.tabselected == 2",
                         selectInput("var2", "X-axis variable", names,
                                     selected = "HW1"),
                         sliderInput("width",
                                     "Bin Width",
                                     min = 1, 
                                     max = 10,
                                     value = 10)),
        conditionalPanel(condition = "input.tabselected == 3",
                         selectInput("var3", "X-axis variable", names,
                                     selected = "Test1"),
                         selectInput("var4", "Y-axis variable", names,
                                     selected = "overall"),
                         sliderInput("opacity", "Opacity",
                                     min = 0.0, max = 1, value = 0.5, step = 0.1),
                         radioButtons("line", "Show line",
                                      c("none", "lm", "loess")))
        ),
      
      # Show plots based on tabular selection
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Barchart", value = 1,
                             ggvisOutput("barchart")),
                    tabPanel("Histogram", value = 2,
                             ggvisOutput("histogram"),
                             h3("Summary Statistics"),
                             textOutput("summary")),
                    tabPanel("Scatterplot", value = 3, 
                             ggvisOutput("scatterplot"),
                             h3("Correlation"),
                             textOutput("text")),
                    id = "tabselected")
   )
  )
)



server <- function(input, output) {
  # Table for first tab sidebar
  output$table <- renderTable(mytable)
  
  # Reorder table
 mytable$Grade <- factor(mytable$Grade, levels = c("A+", "A", "A-", "B+", "B", 
                                                   "B-", "C+", "C", "C-", "D", "F"))
  # Barchart for first tab
  vis_bchart <- { 
    mytable %>%
      ggvis(~Grade, ~Freq) %>%
      layer_bars(fill := "blue")
    
  }
  
  vis_bchart %>% bind_shiny("barchart")
  # hist for second tab
  vis_hist <- reactive({
    var2 <- prop("x", as.symbol(input$var2))
    
    dt %>%
      ggvis(x = var2) %>%
      layer_histograms(width = input$width)
  })
  
  vis_hist %>% bind_shiny("histogram")
  
  # summary_stats for second tab
  output$summary <- renderText({
    print_stats(summary_stats(dt[,input$var2]))
  })
  
  # code for scatterplot
  vis_scatter <- reactive({
    var3 <- prop("x", as.symbol(input$var3))
    var4 <- prop("y", as.symbol(input$var4))
    
    if (input$line == "none") {
     dt %>%
      ggvis(x = var3, y = var4, opacity := input$opacity) %>%
      layer_points()
    } else if (input$line == "lm") {
     dt %>%
        ggvis(x = var3, y = var4, opacity := input$opacity) %>%
        layer_points() %>%
        layer_model_predictions(model = "lm")
    } else {
      dt %>%
        ggvis(x = var3, y = var4, opacity := input$opacity) %>%
        layer_points() %>%
        layer_model_predictions(model = "loess")
    }
    
  })
  
  vis_scatter %>% bind_shiny("scatterplot")
  
  # correlation for third tab
  output$text = renderText ({
    cor(dt[,input$var3], dt[,input$var4])
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)


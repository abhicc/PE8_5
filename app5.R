library(tidyverse)   
library(shiny)

gpmdata <- read_csv("Gapminder_data.csv")

# user interface
ui5 <- fluidPage(
  
  # application title
  titlePanel("Gapminder Summaries"),
  
  fluidRow(
    
    # input for year
    column(3, 
           sliderInput(inputId = "year",
                       label = "Year:",
                       min = 1800,
                       max = 2020,
                       value = 1800)), 
    
    # input to select x-axis variable
    column(3, 
           varSelectInput(inputId = "xvar", 
                          label = "Select Variable to Display on x-axis", 
                          data = gpmdata %>% dplyr::select(lifeExp, pop, gdpPercap))),
    
    # input to select y-axis variable
    column(3, 
           varSelectInput(inputId = "yvar", 
                          label = "Select Variable to Display to y-axis", 
                          data = gpmdata %>% dplyr::select(lifeExp, pop, gdpPercap))),
    
    # input to select 'size' variable
    column(3, 
           varSelectInput(inputId = "sizevar", 
                          label = "Select Variable to Display by Size", 
                          data = gpmdata %>% dplyr::select(lifeExp, pop, gdpPercap)))
  ),
  
  # show plot and table
  fluidRow(
    column(8, plotOutput("plot")),
    
    column(4, tableOutput("table"))
  )
)


# server logic
server5 <- function(input, output) {
  
  output$plot <- renderPlot({
    
    # create plot
    ggplot(data = gpmdata %>% filter(year == input$year)) + 
      geom_point(aes(x = !!input$xvar, y = !!input$yvar, color = continent, size = !!input$sizevar)) +
      scale_x_continuous(trans = 'log2')
  })
  
  output$table <- renderTable({
    
    # create table
    gpmdata %>% 
      filter(year == input$year) %>% 
      group_by(continent) %>% 
      dplyr::summarize(Mean_GDP = mean(gdpPercap, na.rm = TRUE), 
                Mean_LifeExp = mean(lifeExp, na.rm = TRUE),
                Mean_Population = mean(pop, na.rm = TRUE))
  })
}


# run the application 
shinyApp(ui = ui5, server = server5)
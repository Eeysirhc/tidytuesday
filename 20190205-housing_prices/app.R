
# Author: https://twitter.com/Eeysirhc
# Data project for tidytuesday week of 2/5/2019
# Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-02-05

# LOAD PACKAGES
library(tidyverse)
library(scales)
library(shiny)

# PARSE DATA
state_hpi_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")
state_hpi <- state_hpi_raw %>%
  group_by(state, year) %>%
  summarize(us_avg = mean(us_avg),
            price_index = mean(price_index)) %>%
  mutate(pct_diff = (price_index / us_avg) - 1,
         segment = ifelse(pct_diff > 0, 'above', 'below'),
         segment = str_to_title(segment))



# UI 
ui <- fluidPage(
  "Housing Price Index: US Average vs State",
  selectInput(inputId = "select_state",
              label = "Choose a state",
              c(state.abb)),
  plotOutput("hpi1"),
  plotOutput("hpi2")
)



# SERVER
server <- function(input, output, session) {
  
  output$hpi1 <- renderPlot({
    state_hpi %>%
      filter(state == input$select_state) %>%
      group_by(year, state) %>%
      summarize(price_index = mean(price_index),
                us_avg = mean(us_avg)) %>% 
      ggplot() +
      geom_line(aes(year, price_index), size = 2, color = 'steelblue') +
      geom_col(aes(year, us_avg), alpha = 0.3, fill = 'grey54') +
      theme_bw() +
      labs(x = NULL,
           y = "Housing Price Index") + 
      theme_bw(base_size = 15) + 
      scale_y_continuous(limits = c(0,300)) 
  })
  
  output$hpi2 <- renderPlot({
    state_hpi %>%
      filter(state == input$select_state) %>%
      ggplot() + 
      geom_col(aes(year, pct_diff, fill = segment), alpha = 0.8) +
      geom_hline(yintercept = 0, lty = 'dashed') +
      scale_fill_brewer(palette = 'Set1', direction = -1) +
      scale_y_continuous(labels = percent_format(round(1))) +
      theme_bw(base_size = 15) +
      theme(legend.position = 'top') +
      labs(x = NULL,
           y = "Difference to US Average",
           fill = NULL,
           caption = "\n Source: Freddie Mac House Price Index\n Author: eeysirhc")
    })  
}



# APP
shinyApp(ui, server)

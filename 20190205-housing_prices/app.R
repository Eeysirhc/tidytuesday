
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
  mutate(diff = price_index - us_avg) %>% 
  group_by(year, state) %>%
  summarize(us_avg = mean(us_avg),
            price_index = mean(price_index)) %>%
  mutate(pct_diff = (price_index / us_avg) - 1,
         segment = ifelse(pct_diff > 0, 'above', 'below'),
         segment = str_to_title(segment))

# UI 
ui <- fluidPage(

                selectInput(inputId = "select_state",
                            label = "Choose a state",
                            c(state.abb)),
                plotOutput("hpi")
                
)

# SERVER
server <- function(input, output, session) {

    
    output$hpi <- renderPlot({
      state_hpi %>%
        filter(state == input$select_state) %>%
        ggplot() + 
        geom_col(aes(year, pct_diff, fill = segment)) +
        geom_hline(yintercept = 0, lty = 'dashed') +
        scale_fill_brewer(palette = 'Set1', direction = -1) +
        scale_y_continuous(labels = percent_format(round(1))) +
        theme(legend.position = 'none') +
        theme_minimal(base_size = 15) +
        labs(x = NULL,
             y = "Difference to US Average",
             fill = "Segment",
             caption = "\n Source: Freddie Mac House Price Index\n Author: eeysirhc")
    })  

}


# APP
shinyApp(ui, server)




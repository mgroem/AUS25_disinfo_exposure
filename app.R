
rm(list=ls())
options(scipen=999)


# Load/install packages
required_packages <- c("shiny", "ggplot2", "dplyr", "ggthemes", "plotly", "tidyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(plotly)
library(tidyr)

# Load your data
df_counts <- read.csv("df_counts.csv")

# UI
ui <- fluidPage(
  plotlyOutput("exposurePlot")
)

# Server
server <- function(input, output, session) {
  
  output$exposurePlot <- renderPlotly({
    # Responsive dimensions based on screen width
    screen_width <- session$clientData$output_exposurePlot_width
    
    # Set base width and aspect ratio
    width <- screen_width
    height <- if (!is.null(width)) round(width * 1.1) else 600  # 4:3 ratio
    
    df_counts <- df_counts %>%
      mutate(response_label = case_when(
        value == 1 ~ "No, never",
        value == 2 ~ "Yes, but rarely",
        value == 3 ~ "Yes, sometimes",
        value == 4 ~ "Yes, often",
        value == 5 ~ "Unsure",
        TRUE ~ as.character(value)
      ))
    
    p <- ggplot(df_counts, aes(
      x = factor(value),
      y = pct,
      fill = variable,
      text = paste0(response_label, ": ", sprintf("%.1f%%", pct))
    )) +
      geom_col(color = "white", width = 0.7) +
      geom_text(aes(y = pct + 3, label = sprintf("%.1f%%", pct)), size = 3, color = "black") +
     facet_wrap(~variable, ncol = 1, scales = "fixed") +
      scale_x_discrete(
        labels = c(
          "1" = "No, never",
          "2" = "Yes, but rarely",
          "3" = "Yes, sometimes",
          "4" = "Yes, often",
          "5" = "Unsure"
        )
      ) +
      scale_fill_economist() +
      labs(
        y = "Percent", x = NULL,
        title = "Perceived exposure to mis/disinformation about..."
      ) +
      coord_cartesian(ylim = c(0, 50)) +
      theme_economist() +
      theme(
        legend.position = "none",
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(margin = margin(b = 10))
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        autosize = FALSE,
        height = height,
        width = width
      )
  })
}

# Run app
shinyApp(ui, server)



# rsconnect::setAccountInfo(
#  name = "yourname",
#  token = "XXXXXX",
#  secret = "YYYYYY")

# rsconnect::deployApp(".")


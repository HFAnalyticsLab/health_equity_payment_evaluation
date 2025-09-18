## ========================================================================== ##
# Project: Health Equity Payment

# Team: Improvement Analytics Unit (IAU) at the Health Foundation

# Script: app.R

# Corresponding author: Emma Whitfield (emma.whitfield@health.org.uk)

# Description:
# Illustrates SII dependencies

# Dependencies:

# Inputs:

# Outputs:

# Notes:
## ========================================================================== ##


library(shiny)
library(bslib)
library(tidyverse)
library(patchwork)
library(DT)
library(PHEindicatormethods)

q1 <- 20
q2 <- 20
q3 <- 20
q4 <- 20
q5 <- 20

mn1 <- 2
mn2 <- 2.5
mn3 <- 3
mn4 <- 3.5
mn5 <- 4

mx1 <- 2.5
mx2 <- 3
mx3 <- 3.5
mx4 <- 4
mx5 <- 4.5

practices <- tibble()
grouped_practices <- tibble()
sii <- lm(1 ~ 1)

desctext <- htmltools::HTML("Use this interactive tool to explore how the slope index of inequality (SII) for a linear outcome depends on the outcome and population characteristic distributions. <br><br>We assume the population characteristic has five ranked groups of need, from highest need (G1) to lowest need (G5). For each group (G1 - G5) enter a number of units between 5 and 50 to see how a change in the distribution of the population characteristic affects the SII.<br><br>For each group (G1 - G5) enter the minimum and maximum value between 0 and 5 of the outcome that a unit in that group can take. Unit's values are randomly sampled from a uniform distribution between the minimum and maximum for the relevant group. The SII is calculated for the generated sample.")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone", 
                          base_fnt = "Messina Sans"),
  br(),
  titlePanel("Slope index of inequality tool"),
  br(),
  fluidRow(
    column(5,
           wellPanel(
             h5(desctext)
           ),
           br(),
           wellPanel(
             h5("Number of units:"),
             fluidRow(
               column(2, numericInput(inputId = "q1", label = "G1", min = 5, max = 50, value = 20)),
               column(2, numericInput(inputId = "q2", label = "G2", min = 5, max = 50, value = 20)),
               column(2, numericInput(inputId = "q3", label = "G3", min = 5, max = 50, value = 20)),
               column(2, numericInput(inputId = "q4", label = "G4", min = 5, max = 50, value = 20)),
               column(2, numericInput(inputId = "q5", label = "G5", min = 5, max = 50, value = 20))
               ),
             h5("Minimum outcome:"),
             fluidRow(
               column(2, numericInput(inputId = "mn1", label = "G1", min = 0, max = 5, value = 0.5)),
               column(2, numericInput(inputId = "mn2", label = "G2", min = 0, max = 5, value = 0.5)),
               column(2, numericInput(inputId = "mn3", label = "G3", min = 0, max = 5, value = 0.5)),
               column(2, numericInput(inputId = "mn4", label = "G4", min = 0, max = 5, value = 0.5)),
               column(2, numericInput(inputId = "mn5", label = "G5", min = 0, max = 5, value = 0.5))
               ),
             h5("Maximum outcome:"),
             fluidRow(
               column(2, numericInput(inputId = "mx1", label = "G1", min = 0, max = 5, value = 1.5)),
               column(2, numericInput(inputId = "mx2", label = "G2", min = 0, max = 5, value = 1.5)),
               column(2, numericInput(inputId = "mx3", label = "G3", min = 0, max = 5, value = 1.5)),
               column(2, numericInput(inputId = "mx4", label = "G4", min = 0, max = 5, value = 1.5)),
               column(2, numericInput(inputId = "mx5", label = "G5", min = 0, max = 5, value = 1.5))
             )
           )
           ),
    column(7,
           wellPanel(
               plotOutput(outputId = "scatterplot"), 
               br(),
               plotOutput(outputId = "slopeplot")
           )
        )
    ), 
  br(),
  tags$footer("This app is optimised for desktop. For queries please contact freya.tracey@health.org.uk. This tool uses the PHEindicatormethods package to construct the SII shown here. For more information about this package see ",
              tags$a("https://cran.r-project.org/web/packages/PHEindicatormethods/index.html", 
                     href = "https://cran.r-project.org/web/packages/PHEindicatormethods/index.html")
              ),
  tags$head(tags$style(HTML('* {font-family: "Messina Sans"};')))
)

server <- function(input, output, session) {
  total <- reactive({input$q1 + input$q2 + input$q3 + input$q4 + input$q5})
  
  practices <- reactive({tibble(id = seq(1, total()), 
                             quintile = c(rep("1 - highest need", input$q1), rep("2", input$q2), rep("3", input$q3), rep("4", input$q4), rep("5 - lowest need", input$q5))) %>%
    mutate(minval = case_match(quintile, 
                          "1 - highest need" ~ input$mn1, 
                          "2" ~ input$mn2, 
                          "3" ~ input$mn3, 
                          "4" ~ input$mn4, 
                          "5 - lowest need" ~ input$mn5), 
           maxval = case_match(quintile, 
                               "1 - highest need" ~ input$mx1, 
                               "2" ~ input$mx2, 
                               "3" ~ input$mx3, 
                               "4" ~ input$mx4, 
                               "5 - lowest need" ~ input$mx5),  
           outcome = runif(total(), minval, maxval))
  })
    
  grouped_practices <- reactive({practices() %>%
    group_by(quintile) %>%
    summarise(mean.outcome = mean(outcome),
              sd.outcome = sd(outcome),
              n.pracs = n()) %>%
    mutate(lci = mean.outcome - (qt(1 - 0.025, n.pracs - 1)*sd.outcome/sqrt(n.pracs)),
           uci = mean.outcome + (qt(1 - 0.025, n.pracs - 1)*sd.outcome/sqrt(n.pracs)),
           cum.pracs = cumsum(n.pracs),
           ridit = (cum.pracs - (n.pracs/2))/total(), 
           se.outcome = sd.outcome/sqrt(n.pracs))
  })
  
  mytheme <- function() {
    theme_bw() %+replace%
    theme(
      
      plot.title = element_text(
        family = "Messina Sans", 
        face = 'bold', 
        size = 23, 
        hjust = 0, 
        vjust = 1, 
        colour = '#222222', 
        margin = margin(b = 5, t = 10)), 
      plot.subtitle = element_text(
        family = "Messina Sans",
        size = 14, 
        hjust = 0, 
        vjust = 1, 
        colour = '#222222', 
        margin = margin(b = 5, t = 5)),
      
      axis.title = element_text(
        family = "Messina Sans", 
        face = "bold", 
        size = 16, 
        colour = '#4C4C4C'), 
      # axis.title.y = element_text(
      #   vjust = 1.2, 
      #   hjust = 1,
      #   margin = margin(r = -25, l = 20)
      # ),
      axis.text = element_text(
        family = "Messina Sans", 
        size = 16, 
        colour = '#4C4C4C'),
      
      legend.text = element_text(
        family = "Messina Sans", 
        size = 14, 
        hjust = 0, 
        colour = '#222222'),
      legend.title = element_text(
        family = "Messina Sans", 
        size = 16, 
        hjust = 0, 
        colour = '#222222'),
      legend.position = "top", 
      legend.justification = "left", 
      legend.direction = "horizontal",
      legend.box.margin = margin(b = 5), 
      
      panel.grid = element_line(
        colour = "#E5E5E5"
      ),
      axis.line.y.left = element_line(
        colour = "#666666"
      ),
      axis.line.x.bottom = element_line(
        colour = "#666666"
      ),
      panel.border = element_blank(),
      axis.ticks = element_line(
        colour = "#666666"
      ),
      plot.margin = margin(t = 10)
      
    )
  }
  
  thf_cols <- c("#EB003B", "#189EDC", "#7B1FA2", "#FF9800", "#009687")
  
  plot1 <- reactive({ggplot(data = practices(), aes(id, outcome, col = quintile)) + 
      geom_point() + 
      labs(title = "Scatterplot of unit outcomes",
           x = "Unit ID", 
           y = "Outcome", 
           colour = "Need group") +
      scale_colour_manual(values = thf_cols) + 
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
      mytheme()})
  
  makestate <- function(k) {
    if (k == 0) {
      "This indicates that there is no evidence of inequality between the theoretical least and most in need units."
    } else if (k > 0) {
      str_glue("This indicates that units with the lowest need have an excess of the outcome. \nThe difference in outcome between the theoretical least and most in need units is {k}")
    } else {
      str_glue("This indicates that units with the highest need have an excess of the outcome. \nThe difference in outcome between the theoretical least and most in need units is {k}")
    }
  }
  
  plot2 <- reactive({
    gps <- grouped_practices()
    sii <- gps %>% phe_sii(quantile = quintile, population = n.pracs, value = mean.outcome, value_type = 0, se = se.outcome, intercept = T)
    subt <- str_glue("The SII is {round(sii$sii, digits = 3)}. \n{makestate(round(sii$sii, digits = 3))}")
    ggplot(data = gps, aes(ridit, mean.outcome, col = quintile)) +
      geom_point() +
      geom_errorbar(aes(ymin = lci, ymax = uci)) + 
      geom_abline(intercept = sii$intercept, slope = sii$sii, color = "black", linetype = "dashed") + 
      scale_colour_manual(values = thf_cols) + 
      scale_x_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), expand = c(0.01, 0.01)) + 
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
      labs(title = "Illustration of SII calculation", 
           subtitle = subt, 
           x = "Need group midpoint", 
           y = "Mean outcome", 
           colour = "Need group") + 
      mytheme()})
  
  output$scatterplot <- renderPlot({plot1()})
  output$slopeplot <- renderPlot({plot2()})
  output$table <- renderDataTable(grouped_practices())
  
}

shinyApp(ui = ui, server = server)

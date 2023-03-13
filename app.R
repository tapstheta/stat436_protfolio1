library(shiny)
library(ggplot2)
library(plotly)

babies = read.csv("https://uwmadison.box.com/shared/static/7os6119jv2z2auql9tilkp4xv8vff2u1.csv")

#output text for ui
maintext = p("This dataset is from between 1960 and 1967 among women in the San Francisco East Bay area. It encompasses informationb about their pregnancy as well as stats about babies immediatelty after birth. Specifically, age represent the age of the mother at the time of birth. bwt is the weight of the baby at birth. Parity is whether or not the baby is the first child. Smoke is whether the mother is a smoker. gestation represent the gestation period for a specific baby.")


# Define the UI
ui <- fluidPage(
  titlePanel("Baby Metrics Based On Mother's Age: Is it really better to have them younger?"),
  mainPanel(maintext),
  sidebarLayout(
    sidebarPanel(
      selectInput("y_var", "Y-axis variable:",
                  choices = c("bwt", "gestation")),
      checkboxInput("smoke", "Eliminate non-smoking mothers")
    ),
    # Output plot
    mainPanel(
      plotlyOutput("dotplot"),
      dataTableOutput("datatable")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Load the dataset
  
  # Define the dotplot function
  dotplot <- function(df, y_var, smoke) {
    if (smoke) {
      # Plot smoking data
      df_sub <- subset(df, smoke == 1)
      p <- ggplot(df_sub, aes(x = age, y = !!sym(y_var), color = parity)) +
        geom_point() +
        labs(x = "Age", y = y_var, color = "Parity") +
        theme_bw()
    } else {
      # Plot non-smoking data
      df_sub <- subset(df, smoke == 0)
      p <- ggplot(df_sub, aes(x = age, y = !!sym(y_var), color = parity)) +
        geom_point() +
        labs(x = "Age", y = y_var, color = "Parity") +
        theme_bw()
    }
    ggplotly(p, tooltip = "text") %>%
      style(hoveron = "fill")
  }
  
  
  # Render the dotplot output
  output$dotplot <- renderPlotly({
    dotplot(babies, input$y_var, input$smoke)
  })
  
  output$datatable = renderDataTable(babies)
}



# Run the app
shinyApp(ui = ui, server = server)


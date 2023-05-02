#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(maps)
library(scales)
library(tsibble)
library(dplyr)
library(shinythemes)
library(usmap)
library(tidycensus)
library(plotly)
library(tidymodels)
library(readr)
library(caret)
library(DALEX)

#load and clean data
wage <- read.csv("https://uwmadison.box.com/shared/static/qk9mbulhomf72yzolt8dkz7tqan3ihqf.csv")
college <- read.csv("https://uwmadison.box.com/shared/static/u0cjn8tytes6oemp06ec74bgkijzg324.csv")
wage <- subset(wage, Year >= 2013 & Year <= 2020)
college <- college[college$Expense == "Fees/Tuition",]
wage <- wage %>% rename(state = State)
college <- college %>% rename(state = State)
years <- 2013:2019
names(years) <- years

#acs data tidycensus package
acs_data <- map_dfr(years, ~{
  get_acs(
    geography = "state",
    variables = c("B01002_001", "B19001_001", "B15003_001", "B27001_001"), #age, median income, educational attainment, insurance 
    year = .x,
    survey = "acs1"
  )
}, .id = "year")
vars <- c("B01002_001", "B19001_001", "B15003_001", "B27001_001")

#widening ACS to state data for each year
acs_data_wide <- acs_data %>% 
  #select(year, NAME, vars) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  group_by(year, NAME) %>%
  summarize(across(all_of(vars), sum, na.rm = TRUE))

acs_data_wide$year = as.numeric(acs_data_wide$year)
names(acs_data_wide)[names(acs_data_wide) %in% c("B01002_001", "B19001_001", "B15003_001", "B27001_001")] <- c("age", "income", "educ", "insurance")

#us heatmap function
usmap <- function(df, vals) {
  plot_usmap(data = df, values = vals,labels = FALSE) +
    scale_fill_continuous(low = "white",high = "blue",name = "Production", 
                          label = scales::comma) +
    theme(legend.position = "bottom",
          legend.key.width = unit(2.5, "cm")) +
    scale_color_gradient2(limits = c(10000, 40000000))
}

paribus_helper <- function(user_year){
  acs_data_wide_2019 <- subset(acs_data_wide, year == 2019, select = -c(year, NAME))
} 

#ceteris paribus graph
cetparibus <- function(user_year){
  acs_data_wide_year <- subset(acs_data_wide, year == user_year, select = -c(year, NAME))
  x <- select(acs_data_wide_year, -income)
  hyper <- data.frame(n.trees = 100, interaction.depth = 8, shrinkage = 0.1, n.minobsinnode = 10)
  fit <- train(x = x, y = acs_data_wide_year$income, method = "gbm", tuneGrid = hyper, verbose = F)
  
  explanation <- explain(model = fit, data = x, y = acs_data_wide_year$income)
  
  profile <- model_profile(explainer = explanation)
  plot <- plot(profile, geom = "profiles", variables = "educ") 
  return(plot)
}


### definition of app
ui <- fluidPage(
  tags$h1("Looking at the Costs and Payoffs of Higher Education"),
  tags$hr(),
  tags$p("The below three plots are to help you understand the accessibility of education and the payoff to higher education. To start, select a year and a type of higher ed institution. The first plot will show tuition by state. The second shows the minimum wage. I aim to use this comparison to show how they arent not proportionate"),
  sliderInput(inputId = "year", label = "Select a year:", 
              min = 2013, max = 2020, value = 2013, sep = ""),
  
  # Dropdown input for type of institution
  selectInput(inputId = "type", label = "Select an institution type:", 
              choices = c("Private", "Public In-State", "Public Out-of-State"), 
              selected = "Private"),
  
  # Button input for institution length
  checkboxInput(inputId = "length", label = "2-year", value = FALSE),
  
  plotOutput(outputId = "tuition_map"),
  plotOutput(outputId = "wage_map"),
  tags$caption("The below visual is a ceteris paribus plot from the American Community Survey. It shows the relationship between educational attainment and median wage by state, controlling for age and access to health care"),
  plotOutput(outputId = "cet_paribus")
)

server <- function(input, output) {
  # Reactive expression for the subset of college data based on user inputs
  college_subset <- reactive({
    college[college$Year == input$year & college$Type == input$type, ]
  })
  
  wage_subset <- reactive({
    wage[wage$Year == input$year, ]
  })
  
  # Heatmap of tuition using the usmap function
  output$tuition_map <- renderPlot({
    usmap(df = college_subset(), "Value")
  })
  
  # Heatmap of minimum wage using the usmap function
  output$wage_map <- renderPlot({
    usmap(df = wage_subset(), "State.Minimum.Wage")
  })
  
  # Render the plot output
  output$cet_paribus <- renderPlot({
    cetparibus(input$year)
  })
  
}

app = shinyApp(ui, server)

##### Shiny app template #####
library(shiny)
library(mgcv)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/y4/Advanced Data Analysis/10. App assignment/Task1-app")
# Please set your working directory accordingly
wrexham.full <- read.csv("WREXHAM copy.csv")
wrexham <- wrexham.full %>%
  dplyr::select(price, propertytype, oldnew, duration, towncity,
                year, tfarea, numberrooms, CURRENT_ENERGY_EFFICIENCY, BUILT_FORM, ENVIRONMENT_IMPACT_CURRENT)
# Make the variable names easier to read (don't bother changing those which will not be seen by the user)
colnames(wrexham) <- c("Price", "PropertyType", "oldnew", "duration", "Town", "year", "TotalFloorArea", 
                       "NumberOfRooms", "EnergyEfficiencyScore", "BUILT_FORM", "EnvironmentalImpactScore")

glm6 <- glm(Price ~ PropertyType+ Town+ year+ NumberOfRooms+ EnergyEfficiencyScore+ 
              BUILT_FORM, family = Gamma(link = "log"), data = wrexham)




# Define UI ----
ui <- fluidPage(
  titlePanel(
    h1("Property price calculator", align = "centre")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      h2("Welcome"), 
      h3("This is an interactive tool to explore property prices in Wrexham."),
      p('Under the "Explore variables" tab, you can select pairs of variables to plot against each other
        to explore relationships between them.'),
      br(),
      p('Under the "Price calculator" tab, you can enter information about your property into the
        calculator to see how much your property is worth.'),
      br(),
      p('Under the "Property upgrades" tab, you can enter information about potential upgrades or extensions
        you could add to your property and calculate how much its predicted value would be with 
        these upgrades.'),
      
      
      
      
      
    ),
    
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
      
                  tabPanel("Explore Variables",
                           h3("Explore relationships"), #add info about what the variables are
                           p("You can use this tool to explore relationships between pairs of variables. Please use the drop-down
        menus to select which variables to see plotted."),
                           
                           selectInput(inputId='x',label='Select x-axis variable',
                                       choices = c("Number of rooms" = "NumberOfRooms", 
                                                   "Energy efficiency score" = "EnergyEfficiencyScore", 
                                                   "Total floor area" = "TotalFloorArea", 
                                                   "Environmental impact score" = "EnvironmentalImpactScore",
                                                   "Property price" = "Price"),
                                       selected = c("NumberOfRooms")),
                           
                           selectInput(inputId='y',label='Select y-axis variable',
                                       choices = c("Number of rooms" = "NumberOfRooms", 
                                                   "Energy efficiency score" = "EnergyEfficiencyScore", 
                                                   #"Location" = "Town", 
                                                   "Total floor area" = "TotalFloorArea", 
                                                   "Environmental impact score" = "EnvironmentalImpactScore",
                                                   "Property price" = "Price"),
                                       selected = c("Price")),
                           checkboxInput("checkbox", "Split by town", value = FALSE),
                           plotOutput(outputId = "explot1"),
                           plotOutput(outputId = "explot2")),
                  
                  tabPanel("Price calculator", 
                           h3("What is the current value of my property?"),
                           p("This is a tool to calculate the price of your property as it is currently.
                             Please enter details about your property below and then press the 'Calculate' 
                             button to calculate an estimate for your current property value as of this year (2023)."),
                           p("Press 'Show plot' to see the estimated value of your property over time, including forecasted price until 2030."),
                           p("If you would like an estimate for your property's potential future value 
                             after adding extensions or upgrades, please go to the tab labelled 'Property upgrades'!"),
                           
                           fluidRow(
                             column(width = 4, selectInput(inputId = 'PropertyType', label = "Select property type", 
                                                           choices = c("Detached" = "D", "Semi-detached" = "S",
                                                                       "Terraced" = "T", "Flat/Maisonette" = "F"), selected = "S")),
                             column(width = 4, selectInput(inputId = 'Town', label = "Select your town within Wrexham county",
                                                           choices = c("Wrexham" = "WREXHAM", "Llangollen" = "LLANGOLLEN",
                                                                       "Mold" = "MOLD", "Chester" = "CHESTER", 
                                                                       "Whitchurch" = "WHITCHURCH", "Malpas" = "MALPAS", 
                                                                       "Oswestry" = "OSWESTRY"))),
                             column(width = 4, selectInput(inputId = "builtform", label = "Select build type",
                                                           choices = c("Semi-Detached", "Detached", "End-Terrace", "Enclosed End-Terrace",
                                                                       "Mid-Terrace", "Enclosed Mid-Terrace")))),
                           
                           helpText("Please enter numeric inputs only into the following boxes"),
                           
                           fluidRow(
                             column(width = 4, numericInput("NumberOfRooms", label = "Enter number of rooms", value = 3)),
                             column(width = 4, numericInput("energy", label = "Enter energy efficiency score", value = 65))),
                           helpText("Press the button and scroll down to see estimate"),
                           actionButton(inputId = "button1", label = "Calculate"),
                           textOutput("current"),
                           helpText("Press the button to see your property's estimated value over time. Press again to reload"),
                           actionButton(inputId = "button1.2", label = "Show plot"),
                           plotOutput(outputId = 'valueplot')),
                  
                  tabPanel("Property upgrades",
                           h3("How much could my property be worth?"),
                           p("Thinking of adding rooms or improving your insulation? This calculator predicts the potential future value
                             of your property after these extensions"),
                           p("Enter details of about your property after the extensions and press the 'Calculate' button."),

                           fluidRow(
                             column(width = 4, selectInput(inputId = 'PropertyType2', label = "Select property type", 
                                                           choices = c("Detached" = "D", "Semi-detached" = "S",
                                                                       "Terraced" = "T", "Flat/Maisonette" = "F"), selected = "S")),
                             column(width = 4, selectInput(inputId = 'Town2', label = "Select your town within Wrexham county",
                                                           choices = c("Wrexham" = "WREXHAM", "Llangollen" = "LLANGOLLEN",
                                                                       "Mold" = "MOLD", "Chester" = "CHESTER", 
                                                                       "Whitchurch" = "WHITCHURCH", "Malpas" = "MALPAS", 
                                                                       "Oswestry" = "OSWESTRY"))),
                             column(width = 4, selectInput(inputId = "builtform2", label = "Select build type",
                                                           choices = c("Semi-Detached", "Detached", "End-Terrace", "Enclosed End-Terrace",
                                                                       "Mid-Terrace", "Enclosed Mid-Terrace")))),
                           
                           helpText("Please enter numeric inputs only into the following boxes"),
                           
                           fluidRow(
                             column(width = 4, numericInput("year2", label = "Enter year the sale will be completed", value = 2023, min = 2023)),
                             column(width = 4, numericInput("NumberOfRooms2", label = "Enter future number of rooms", value = 3)),
                             column(width = 4, numericInput("energy2", label = "Enter future energy efficiency score", value = 65))),
                           helpText("Press the button and scroll down to see estimate"),
                           actionButton(inputId = "button2", label = "Calculate"),
                           textOutput("future"))
      )
    )
  )
)



# Define server logic ----
server <- function(input, output) {
  
  #----Tab 1
  observeEvent(input$checkbox, {
    if (input$checkbox == TRUE){
      output$explot1 <- renderPlot(
        ggplot(data = wrexham, aes_string(input$x, input$y)) +
          geom_point() +
          labs(x = input$x, y = input$y) + 
          facet_wrap(~Town))  
    }
    else{
      output$explot1 <- renderPlot(
        ggplot(data = wrexham, aes_string(input$x, input$y)) +
          geom_point() +
          labs(x = input$x, y = input$y)) 
    }

  }
  )         
  
  #---- Tab 2
  predvalue <- function(yr){ # Define a function to easily compute price estimates based on data entered by user (will use in a for loop later)
    val <- round(predict.glm(glm6, newdata = data.frame(PropertyType = input$PropertyType, 
                                                        Town = input$Town, year = yr, 
                                                        NumberOfRooms = input$NumberOfRooms, 
                                                        EnergyEfficiencyScore = input$energy, 
                                                        BUILT_FORM = input$builtform), type = "response"), 2)
    return(val)
  } 
  
  
    

  observeEvent(input$button1,
               output$current <- renderText(paste("Estimated price of your property in 2023:  £",
                                                  predvalue(2023)))
  )
  observeEvent(input$button1.2, {
    propvals <- numeric(30)
                for (i in 1:length(propvals)){ #create dataframe containing estimated/predicted price for user's property through the years
                  y <- 2000 + i
                  propvals[i] <- predvalue(y)}
    
                propcurr <- data.frame(c(2001:2030),propvals)
                colnames(propcurr) <- c("Year", "Value")
                
                output$valueplot <- renderPlot(
                  ggplot(data = propcurr,aes_string('Year', 'Value')) +
                    geom_point()
                )}
                )
               
               
               
  #-----Tab 3                                          
  observeEvent(input$button2,
               output$future <- renderText(paste("Estimated future price of your property:  £",
                                                 round(predict.glm(glm6, newdata = data.frame(PropertyType = input$PropertyType2, 
                                                                                              Town = input$Town2, year = input$year2, 
                                                                                              NumberOfRooms = input$NumberOfRooms2, 
                                                                                              EnergyEfficiencyScore = input$energy2, 
                                                                                              BUILT_FORM = input$builtform2), type = "response"), 2)
                                                  )))
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

#install.packages("readxl")
#install.packages("ggplot2")
#install.packages(shiny)
#install.packages("shinyjs")
#install.packages("plotly")
library(plotly)

remotes::install_github("deepanshu88/shinyDarkmode")
#install.packages(shinyDarkmode)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shinyDarkmode)
library(dplyr)


#library("readxl")
library("ggplot2")

#to read data set
library(readr)
courseUdemy <-read_csv("udemy_courses.csv")

#check data type
str(courseUdemy)

# To Convert date to Known shape ex: 2022/2/22
courseUdemy$published_timestamp <-as.Date(courseUdemy$published_timestamp)
#check change is done

#check null values
colSums(is.na(courseUdemy))
sapply(courseUdemy, function(x) sum(is.na(x)))

#check duplicated values
any(duplicated(courseUdemy))
sum(duplicated(courseUdemy))



#check data validity for numerical columns
summaryStats <- function(column) {
  c(Min = min(column, na.rm = TRUE), 
    Mean = mean(column, na.rm = TRUE), 
    Median = median(column, na.rm = TRUE), 
    Max = max(column, na.rm = TRUE), 
    '1st Qu' = quantile(column, 0.25, na.rm = TRUE), 
    '3rd Qu' = quantile(column, 0.75, na.rm = TRUE))
}

# Apply the function to a column (replace 'numericColumn' with your column name)
numericStats <- summaryStats(courseUdemy$price)
numericStats1 <- summaryStats(courseUdemy$num_subscribers)
numericStats2 <- summaryStats(courseUdemy$num_reviews)
numericStats3 <- summaryStats(courseUdemy$num_lectures)

# Round the results
round(numericStats, 2)
round(numericStats1, 2)
round(numericStats2, 2)
round(numericStats3, 2)



# Calculate daily and cumulative courses
daily_courses <- courseUdemy %>%
  group_by(published_timestamp) %>%
  summarise(DailyCount = n())

cumulative_courses <- daily_courses %>%
  mutate(CumulativeCount = cumsum(DailyCount))






vars <- c("num_subscribers","price","num_reviews","num_lectures")
str(courseUdemy)
summary(courseUdemy)




ui <- dashboardPage(
  #Toggle switch for light/dark mode
  
  
  
  
  
  
  dashboardHeader(
    
    # dark/light mode toggle
    
    
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "dataset", icon = icon("th")),
      menuItem("Visualization", tabName = "visualization", icon = icon("dashboard")),
      menuItem("Instructions", tabName = "Instructions", icon = icon("book"))
    )
  ),
  dashboardBody(
    
    use_darkmode(),
    useShinyjs(),
    
    
    
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "dataset",
              h1("Dataset is Udemy Courses"),
              DT::dataTableOutput("DisplayData")
              
      ),
      # New tab: instructions
      tabItem(
        tabName = "Instructions",
        h1("Hope these instructions help you !"),
        
        verbatimTextOutput("instructionOutput")
      ),
      
      # Second tab content
      tabItem(tabName = "visualization",
              fluidPage(
                #theme = shinytheme("superhero"),
                titlePanel("Udemy Dataset"),
                tabsetPanel(
                  
                  
                  
                  
                  tabPanel(title = "Histogram Plot",h1("Histogram Plot"),
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput(
                                 inputId = "bins",
                                 label = "Number of bins:",
                                 min = 0,max = 50,value = 30),
                               radioButtons(inputId ="chose_freq_dinsty", label="Choose Y Type:", choices = c("Absolute", "Density"))
                             ),
                             mainPanel(
                               plotlyOutput(outputId = "histplot"),h2("Observation of the plot is : Most frequent course when the price = 20$ ")
                             )
                           )
                  ),
                  tabPanel(title = "Bar charts",h1("Bar charts"),
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput("categoryInput", "Select Categories:",
                                                  choices = unique(courseUdemy$subject), 
                                                  selected = unique(courseUdemy$subject)[1])
                               
                             ),
                             mainPanel(
                               plotlyOutput(outputId = "barchart"),h2("Observation of the plot is : Both Business Finance and Web Development are equal frequency and minimum frequency is graphic design.")
                             )
                           )
                  ),
                  
                  # Single tab containing both plots
                  tabPanel(title = "Combined Plots",
                           # Checkbox to show/hide Pie Chart
                           checkboxInput("showPieChart", "Show Pie Chart", value = TRUE),
                           
                           # Conditional UI for Pie Chart
                           uiOutput("pieChartUI"),
                           
                           # Checkbox to show/hide Freqpoly Plot
                           checkboxInput("showFreqpolyPlot", "Show Freqpoly Plot", value = FALSE),
                           
                           # Conditional UI for Freqpoly Plot
                           uiOutput("freqpolyPlotUI")
                  ),
                  # ... Other tabs if any ...
                  
                  
                  tabPanel(title = "Box Plot",h1("Box Plot"),plotlyOutput(outputId = "boxPlot"),h2("Observation of the plot is : Web development courses vary the most in content duration and Musical instruments courses are right skewed.")),
                  
                  
                  
                  tabPanel(title = "scatter Plot",h1("scatter Plot"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput('xcol', 'X Variable', vars,selected = vars[[3]]),
                               selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                               numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
                             ),mainPanel(plotOutput(outputId = "scatterPlot"),h2("Observation of the plot is : Majority of subscribers don't leave reviews.")
                             )
                           )),
                  tabPanel(title = "line Chart Plot",h1("line Chart Plot"),
                           
                           sidebarLayout(
                             sidebarPanel(
                               dateRangeInput("dateRange", 
                                              label = "Select Date Range",
                                              start = as.Date("2013-01-01"),
                                              end = as.Date("2017-01-01") 
                                              
                               )
                             ),
                             mainPanel(
                               plotlyOutput(outputId = "lineChart"),h2("Observation of the plot is :Year 2013 had the highest level of subscriber engagement or enrollment.")
                             )
                           )
                           
                  ),
                  tabPanel(title = "line chart plot 2",h1("line plot"),
                           sidebarLayout(
                             sidebarPanel(
                               
                               radioButtons("plotType", "Choose plot type:",
                                            choices = c("Daily" = "daily", "Cumulative" = "cumulative"))
                             ),
                             mainPanel(
                               plotlyOutput(outputId = "line"),h2("Observation of the plot is :the number of courses published on Udemy shows a directly proportional relationship, this means that as time progresses, the number of published courses increases at a steady, consistent rate")
                             )
                           )
                  ),
                  
                  
                  
                  
                  
                  
                )
              )
      )
    )
  )
)



server <- function(input, output, session) {
  darkmode(
    bottom = "32px",
    right = "32px",
    left = "unset",
    time = "0.5s",
    mixColor = "#fff",
    backgroundColor = "#fff",
    buttonColorDark = "#100f2c",
    buttonColorLight = "#fff",
    saveInCookies = FALSE,
    label = "???" ,
    autoMatchOsTheme = TRUE
  )
  
  #Tables of Dataset
  output$DisplayData <- DT::renderDataTable({
    courseUdemy
    #DT::datatable(Data_to_display(courseUdemy))
  })
  # instructions 
  output$instructionOutput <- renderText({
    sentences <- c(
      "Instructions : ",
      "Use the dropdown menus to select parameters for visualizations.",
      "Hover over data points for more information.",
      "Toggle between dark and light mode for comfortable viewing.",
      "Click on the 'Visualization' tab to explore charts and graphs.",
      "Use the checkbox to select parameters for visualizations." ,
      "Use checkboxes to dynamically show/hide different visualizations in combined plots tab",
      "Use tooltips or hover effects for detailed information in the visualizations."
    )
    paste(sentences, collapse = "\n")
  })
  
  # Histogram plot
  output$histplot <- renderPlotly({
    x <- courseUdemy$price
    if (input$chose_freq_dinsty == "Absolute")
    {p <- ggplot(data.frame(x), aes(x = x)) +
      geom_histogram(bins = as.numeric(input$bins), fill = "#007bc2", color = "white") +
      labs(x = "Price", 
           y = input$chose_freq_dinsty,  
           title = "Histogram of Courses Price")
    ggplotly(p, tooltip = c("x", "y"))
    } else
    {
      p <- ggplot(data.frame(x), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = as.numeric(input$bins), fill = "#007bc2", color = "white") +
        labs(x = "Price", 
             y = input$chose_freq_dinsty,  # Dynamic y-axis label
             title = "Histogram of Courses Price")
      ggplotly(p, tooltip = c("x", "y"))
      
      
    }
    
    
  })
  # Bar chart
  filteredData <- reactive({
    
    courseUdemy[courseUdemy$subject %in% input$categoryInput, ]
  })
  
  output$barchart <- renderPlotly({
    dataToPlot <- filteredData()
    p<-ggplot(dataToPlot, aes(subject)) + geom_bar(fill = "#FF6666")
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  # Box Plot
  output$boxPlot <- renderPlotly({
    sample_data <- courseUdemy[sample(nrow(courseUdemy), size = 300), ]
    p<-ggplot(sample_data, aes(x = subject, y = content_duration)) + geom_boxplot()
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Freqpoly Plot
  output$FreqpolyPlot <- renderPlot({
    ggplot(courseUdemy, aes(num_lectures)) + geom_freqpoly()
    
  })
  
  # Pie Chart
  courseUdemy$is_paid <- as.factor(courseUdemy$is_paid)
  output$pieChart <- renderPlot({
    pie(table(courseUdemy$is_paid),
        labels = levels(courseUdemy$is_paid),
        main = "Distribution of Courses ( Paid / Free )")
  })
  selectedData <- reactive({
    courseUdemy[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    palette(c("#FF6666", "#EEE7DA", "#C1F2B0", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 1)
    points(clusters()$centers, pch = 4, cex = 1, lwd = 1)
    
  })
  # Line Chart
  filteredData1 <- reactive({
    startDate <- input$dateRange[1]
    endDate <- input$dateRange[2]
    subset(courseUdemy, courseUdemy$published_timestamp >= startDate & courseUdemy$published_timestamp <= endDate)
  })
  output$lineChart <- renderPlotly({
    dataToPlot1 <- filteredData1()
    p<-ggplot(dataToPlot1, aes(x = published_timestamp, y = num_subscribers, group = 1)) +
      geom_line(color = "blue") +
      labs(title = "Number of Subscribers Over Time",
           x = "Time Variable",
           y = "Number of Subscribers") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y"))
  })
  #show or hide pichart
  output$pieChartUI <- renderUI({
    if(input$showPieChart) {
      tagList(
        h1("Pie Chart Plot"),
        plotOutput("pieChart"),
        h2("Observation of the pie chart is: Most courses are paid."),
        hr() # Optional separator
      )
    }
  })
  #show or hide freqpolyplot
  output$freqpolyPlotUI <- renderUI({
    if(input$showFreqpolyPlot) {
      tagList(
        h1("Freqpoly Plot"),
        plotOutput("FreqpolyPlot"),
        h2("Observation of the freqpoly plot is: Most courses have less than 100 lectures.")
        
        
      )
    }
  })
  output$line <- renderPlotly({
    
    if(input$plotType == "daily") {
      p<-ggplot(daily_courses, aes(x = published_timestamp, y = DailyCount)) +
        geom_line() +
        labs(title = "Daily Number of Courses Published on Udemy",
             x = "Date", y = "Number of Courses")
    } else {
      p<-ggplot(cumulative_courses, aes(x = published_timestamp, y = CumulativeCount)) +
        geom_line() +
        labs(title = "Cumulative Number of Courses Published on Udemy",
             x = "Date", y = "Cumulative Number of Courses")
    }
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################################End of server
}




shinyApp(ui = ui, server = server)

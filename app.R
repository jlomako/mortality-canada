######################################################################
#
# Weekly Death Counts Canada : mortality-canada
#
######################################################################

# based on data from Statistics Canada, downloaded on 9-9-2022:

# Table 13-10-0768-01 Weekly death counts, by age group and sex
# DOI: https://doi.org/10.25318/1310076801-eng
# https://www150.statcan.gc.ca/n1/tbl/csv/13100768-eng.zip

# population data incl age, sex, geo, year
# Table: 17-10-0005-01 (formerly CANSIM 051-0001)
# Table 17-10-0005-01  Population estimates on July 1st, by age and sex
# DOI: https://doi.org/10.25318/1710000501-eng


library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)

# get data incl death counts and population from repository
data <- vroom::vroom("https://github.com/jlomako/mortality-canada/raw/main/data/data.csv", show_col_types = FALSE)

data$geo <- as.factor(data$geo)
data$age <- as.factor(data$age)
data$sex <- as.factor(data$sex)
data$date <- as.Date(data$date)
data$year <- format(data$date, "%Y")
data$week <- format(data$date, "%U")
data$year <- as.factor(data$year)

# calculate death rate per 100.000 and add dummy variable CommonDate for better plotting
data <- data %>% mutate(death_rate = deaths / pop * 1e4) %>%
  mutate(CommonDate = as.Date(paste0("2000-", format(date, "%j")), "%Y-%j")) 

levels(data$sex) <- c("Both sexes", "Female", "Male")

provinces <- data %>% distinct(geo)
sex <- data %>% distinct(sex)
age <- data %>% distinct(age)
year <- data %>% distinct(year)
plot_title <- ""



#########################
## dashboard interface:
#########################
# library(shinydashboard)
# ui <- dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(
#     div(selectInput(inputId = "province", label = "Select a province", choices = provinces, width = "100%")),
#     div(selectInput(inputId = "sex", label = "Select group", choices = sex, width = "100%")),
#     div(selectInput(inputId = "age", label = "Select age", choices = age, width = "100%")),
#     #       div(pickerInput(inputId = "age", label = "Select age", choices = age, multiple = T, selected = "all ages")), # options = list(`actions-box` = TRUE)
#     #       div(pickerInput(inputId = "year", label = "Select year", choices = year, multiple = T, selected = 2021)) # options = list(`actions-box` = TRUE)
#     div(checkboxGroupInput(inputId = "year", label = "Select year",
#                            choices = list("2021" = 2021, "2020" = 2020, "2019" = 2019, "2018" = 2018, 
#                                           "2017" = 2017, "2016" = 2016, "2015" = 2015, "2014" = 2014,
#                                           "2013" = 2013, "2012" = 2012, "2011" = 2011, "2010" = 2010),
#                            selected = 2020:2021))
#   ),
#   
#   dashboardBody(
#     # plot
#     plotOutput("plot")
#     
#   ),
# )
###########################

#########################
# bootstrap 5 interface
#########################
thematic::thematic_shiny(font = "auto")
ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  # add title and google fonts
  tags$head(HTML('<title>Mortality rates in Canada. Weekly deaths, by age group and sex</title>
                  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
                  <link href="https://fonts.googleapis.com/css2?family=Noto+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" rel="stylesheet">')),
  
  div(class="container-sm",
      
      h1("Mortality in Canada", class="text-center pt-1"),
      h5("Weekly deaths, by age group and sex", class="text-center"),
      
      # card current occupancy 
      div(class="row gx-2",
          div(class="col-sm-3",
              div(class="card h-100",
                  # div(class="card-header bg-primary", h5("card header left", class="card-title")),
                  div(class="card-body bg-light px-2", 
                      div(selectInput(inputId = "province", label = "Select a province", choices = provinces, width = "100%")),
                      div(selectInput(inputId = "sex", label = "Select group", choices = sex, width = "100%")),
                      div(selectInput(inputId = "age", label = "Select age", choices = age, width = "100%")),
                      div(radioButtons(inputId = "type", label = "", 
                                       choices = c("Deaths per 100.000" = "death_rate", "Number of Deaths" = "deaths"),
                                       selected = "death_rate", width="100%"))
                      # div(class="card-footer", h5("card footer right", class="small"))
                      ),
                  # div(class="card-footer")
                 ),
          ),
          div(class="col-sm-9",
              div(class="card h-100",
                      div(class="card-body px-0", div(plotOutput("plot", width = "100%"))),
                      div(class="card-body bg-light border-top", 
                          div(checkboxGroupInput(inputId = "year", 
                                                 label = "Select a year",
                                                 choices = list("2021" = 2021, "2020" = 2020, "2019" = 2019, "2018" = 2018, 
                                                 "2017" = 2017, "2016" = 2016, "2015" = 2015, "2014" = 2014,
                                                 "2013" = 2013, "2012" = 2012, "2011" = 2011, "2010" = 2010),
                                                 selected = 2020:2021,
                                                 inline=T))
                          ),
                      div(class="card-footer bg-white", 
                          tags$div(
                          HTML('<p class="text-center small">
                              Data source: Statistics Canada.<br>
                              DOI: https://doi.org/10.25318/1310076801-eng<br>
                              DOI: https://doi.org/10.25318/1710000501-eng<br>
                              © Copyright 2022, <a href="https://github.com/jlomako">jlomako</a>, Montr&#233;al</p>'),
                          )
                        ),
                      ),    
              ),
          ),
     ),
)


server <- function(input, output) { 
  
  output$plot <- renderPlot({
    selected_province <- reactive(data %>% 
                                    filter(year %in% input$year) %>% 
                                    filter(geo == input$province & sex == input$sex & age == input$age))
                                    
    ifelse(input$type == "deaths", plot_title <- "Weekly Number of deaths", plot_title <- "Weekly Mortality rate per 100.000 inhabitants")
    
    selected_province() %>% 
      ggplot(aes(x = CommonDate, y = get(input$type), colour = year)) +
      geom_line(size = 0.71, na.rm=T) +
      scale_x_date(date_labels = "%b %d", date_breaks = "2 month") +
      labs(title = plot_title, 
           subtitle = paste0(input$province, ": ", input$sex, ", ", input$age), 
           caption = NULL, y = NULL, x = NULL,) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0.5), 
            legend.position="top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
            axis.ticks = element_blank(), axis.text = element_text(size = 9)) +
      scale_colour_manual(values = c("2021"="deepskyblue3", "2020" ="darkgoldenrod1", "2019"="chocolate", "2018"="blue1", "2017"="black", "2016"="red", 
                                     "2015"="deeppink4", "2014"="deeppink", "2013"="darkviolet", "2012"="darkturquoise", "2011"="coral", "2010"="darkgrey"),
                          breaks=(droplevels(selected_province()$year)))
  }, res = 96, height = "auto")
  
}

shinyApp(ui, server)

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

# calculate death rate per 100.000 and add dummy variable CommonDate for better plotting
data <- data %>% mutate(death_rate = deaths / pop * 1e4) %>%
  mutate(CommonDate = as.Date(paste0("2000-", format(date, "%j")), "%Y-%j"))

provinces <- data %>% distinct(geo)
sex <- data %>% distinct(sex)
age <- data %>% distinct(age)
year <- data %>% distinct(year)

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

#########################
# bootstrap 5 interface
#########################
thematic::thematic_shiny(font = "auto")
ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  tags$head(HTML("<title>Mortality rates in Canada</title>")),
  
  div(class="container-sm",
      
      h1("Mortality in Canada", class="text-center pt-3"),
      h5("Weekly death counts, by age group and sex", class="text-center pt-0"),
      
      # card current occupancy 
      div(class="row",
          div(class="col-sm-3",
              div(class="card",
                  # div(class="card-header bg-primary", h5("card header left", class="card-title")),
                  div(class="card-footer px-2", 
                      div(selectInput(inputId = "province", label = "Select a province", choices = provinces, width = "100%")),
                      div(selectInput(inputId = "sex", label = "Select group", choices = sex, width = "100%")),
                      div(selectInput(inputId = "age", label = "Select age", choices = age, width = "100%"))
                      # div(class="card-footer", h5("card footer right", class="small"))
                      ),
                  # div(class="card-footer", h5("card footer left", class="small"))
                 ),
          ),
          div(class="col-sm-9",
              div(class="card w-100",
                    #  div(class="card-header bg-primary", h5("card header right", class="card-title")),
                      div(class="card-body px-0", div(class ="w-100", plotOutput("plot", width = "100%"))),
                      div(class="card-footer", 
                          div(checkboxGroupInput(inputId = "year", 
                                                 label = "Select a year",
                                                 choices = list("2021" = 2021, "2020" = 2020, "2019" = 2019, "2018" = 2018, 
                                                 "2017" = 2017, "2016" = 2016, "2015" = 2015, "2014" = 2014,
                                                 "2013" = 2013, "2012" = 2012, "2011" = 2011, "2010" = 2010),
                                                 selected = 2020:2021,
                                                 inline=T))
                          )
                    #  div(class="card-footer", h5("card footer right", class="small"))
                      )    
              )
          )
     )
  )


server <- function(input, output) { 
  
  output$plot <- renderPlot({
    selected_province <- reactive(data %>% 
                                    filter(year %in% input$year) %>%
                                    filter(geo == input$province & 
                                             sex == input$sex & 
                                             age == input$age))
    selected_province() %>% 
      ggplot(aes(x = CommonDate, y = death_rate, colour = year)) +
      geom_line(size = 0.5, na.rm=T) +
      scale_x_date(date_labels = "%b %d", date_breaks = "2 month") +
      # ylim(0,50) +
      labs(title = "Weekly mortality rate per 100.000 inhabitants", 
           subtitle = paste0("in ", input$province, ": ", input$sex, ", ", input$age), 
           y = NULL, x = NULL,) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5), 
            legend.position="top", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  }, res = 96, height = "auto")
  
}

shinyApp(ui, server)
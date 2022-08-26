
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(openxlsx)
library(tidyr)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(forcats)


#setwd("C:/Users/ngbao/OneDrive/Desktop/2022/Data viz/a3/assignment3")
filename <- "Deaths-web-report-suppl-tables_2021.xlsx"

# data 1
death_case_by_sex<- read.xlsx(filename, sheet=, "Table S4.1", rows=4:117, cols = c(1, 2, 6, 10) )
colnames(death_case_by_sex) <- c("Year", "Males", "Females", "Both Sex")
death_case_by_sex$Year <- as.factor(death_case_by_sex$Year)
choice1 <- names(death_case_by_sex)[-1]

# data per thousand
death_case_by_sex_perthousand <- death_case_by_sex %>% mutate("Males_perthousand" = Males/1000,
                                                              "Females_perthousand"= Females/1000,
                                                              "Both_Sex_perthousand" = `Both Sex`/1000
                                                              ) %>% select(-Males, -Females, -`Both Sex`)
colnames(death_case_by_sex_perthousand) <- c("Year", "Males", "Females", "Both Sex")


# data 2
death_case_by_broad_cause <- read.xlsx(filename, sheet="Table S4.2", rows= 4:117)[, c(1,2,5,8,11,14,17,20)]
colnames(death_case_by_broad_cause) <- c("Year", "Circulatory_diseases", "Cancers", "Respiratory_diseases", "Infectious_diseases", "Injury&poisoning", "Other", "All")
death_case_by_broad_cause$Year <- as.factor(death_case_by_broad_cause$Year)
year <- unique(death_case_by_broad_cause$Year)

death_case_by_broad_cause2 <- death_case_by_broad_cause %>% gather(key= "Cause", value="case", 2:8)%>% mutate("case"= case/10000)


# data 3
death_sex_age_2019 <- read.xlsx(filename, sheet = "Table S2.1", rows = 3:24)[-4]
death_sex_age_2019 <- death_sex_age_2019 %>% gather(key="Gender", value= "case", 2:3)
# factorize Age.group column 
age <- c("0-4", "5-9", "10-14", "15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69","70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+" )
death_sex_age_2019$Age.group <- factor(death_sex_age_2019$Age.group, labels=age)

# data 4
underlying_cause_of_death_2019 <- read.xlsx(filename, sheet = "Table S3.1", rows = 4:24)[, c(2,3,7,8)] 
data1 <- underlying_cause_of_death_2019 %>% select(1,2) %>% mutate("Gender" = "Males")
colnames(data1) <- c("Cause.of.death", "Case", "Gender")

data2 <- underlying_cause_of_death_2019 %>% select(3,4) %>% mutate("Gender" = "Females") 
colnames(data2) <- c("Cause.of.death", "Case", "Gender")

data <- union(data1, data2)

trim <- function(x){
  x <- str_trim(str_split(x, '[(]')[[1]][1], side = "right")
}

data$Cause.of.death <- unlist(lapply(data$Cause.of.death, FUN=trim))
data <- data %>% mutate("Case" = Case/1000)








# Define UI for application that draws a histogram
sidebar <- dashboardSidebar(
  sidebarMenu(
    # change name for this
    menuItem("By sex(1907-2019)", tabName = "sex"),
    menuItem("By board cause(1907-2019)", tabName = "cause"),
    menuItem("By Leading cause of death in 2019",tabName = "2019" )
  )
)

body <- dashboardBody(
  tags$head(tags$style("body {overflow-y: hidden;}" ) ),
  tabItems(
    tabItem( tabName = "sex", 
             fluidRow(box(title ="Death case by Sex from 1907-2019", plotlyOutput("plot1", height = 250), width = 12)),
             fluidRow(
               box(selectInput("var1","Sex" , choices = choice1, selected = choice1[3]), width = 3,
                   sliderInput("bins1", "Number of bins:",min = 1, max = 100,value = 30)),
               box(title="Historgram of Death case by sex from 1907-2019", plotlyOutput("plot2", height = 160), width = 8)),
             p("reference: Australian Institution of Health and Welfare 2021, Deaths in Australia, Viewed 20 May 2022, https://www.aihw.gov.au/reports/life-expectancy-death/deaths-in-australia/data")
             ),
    
    
    tabItem( tabName = "cause",
             fluidRow( box(title="Death Case by Broad Cause from 1907-2019",plotlyOutput("plot3", height = 250), width = 12)),
             fluidRow(box(sliderInput("year_", label = "Year", min = 2001, sep="", 
                                  max = 2019, value = 2001, 
                                  animate = animationOptions(interval = 500, loop = TRUE)), width=4),
                      box(title="Distribution of Broad Cause by Death case from 2001-2019",plotlyOutput("plot4", height = 190), width = 8)),
             p("reference: Australian Institution of Health and Welfare 2021, Deaths in Australia, Viewed 20 May 2022, https://www.aihw.gov.au/reports/life-expectancy-death/deaths-in-australia/data"),
           
             ),
    tabItem(tabName = "2019" , 
            fluidRow(
              box(selectInput(input="sex_", label = "Sex", choices = unique(data$Gender), selected = unique(data$Gender)), width=2, 
                  tags$style(type='text/css', ".selectize-input { font-size: 11px; line-height: 11px;} ")),
              box(title="Leading Underlying Cause of Death case by sex in 2019", plotlyOutput("plot6", height = 500), width= 5),
              box( title = "Death case by Sex for each Age group in 2019", plotlyOutput("plot5", height = 500), width = 5)) ,
              p("reference: Australian Institution of Health and Welfare 2021, Deaths in Australia, Viewed 20 May 2022, https://www.aihw.gov.au/reports/life-expectancy-death/deaths-in-australia/data"),
            
    )
  )
)



ui <- dashboardPage(dashboardHeader(title="Death in Australia\n 1907-2019"),sidebar, body) 

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # plot for death rate by sex 1907-2019
  output$plot1 <- renderPlotly({
    death1 <- plot_ly(death_case_by_sex, x= ~Year)
    death1 <- death1 %>% add_lines(y= ~Males, name="Males")
    death1 <- death1 %>% add_lines(y= ~Females, name="Females")
    death1 <- death1 %>% add_lines(y= ~`Both Sex`, name = "Both Sex")
    death1 <- death1 %>% layout( yaxis =list(title= "Death cases"))
  })
  
  
  #distribution plot for death rate by sex 1907-2019
   
  output$plot2 <- renderPlotly({
    ggplot(data = death_case_by_sex_perthousand, aes(x = get(input$var1))) +
      geom_histogram(bins=input$bins1, aes(y= ..density..),colour = "white") + 
      geom_density(colour="blue") +
      stat_function(fun = dnorm, colour = "red") +
      labs(x= "Death cases per thousand", y = "frequency", title = input$var1 )+
      theme(text = element_text(size=12))
  })
  
  # plot for death rate by board cause 1907-2019
  output$plot3 <- renderPlotly({
    cause <- plot_ly(death_case_by_broad_cause, x= ~ Year)
    cause <- cause %>% add_lines(y= ~ Circulatory_diseases, name = "Circulatory")
    cause <- cause %>%  add_lines(y= ~ `Cancers`, name = "Cancers")
    cause <- cause %>%  add_lines(y= ~ Respiratory_diseases, name = "Respiratory")
    cause <- cause %>%  add_lines(y= ~ Infectious_diseases, name = "Infectious")
    cause <- cause %>%  add_lines(y= ~ `Injury&poisoning`, name = "Injury&\npoisoning")
    cause <- cause %>%  add_lines(y= ~ Other, name = "Other")
    cause <- cause %>%  add_lines(y= ~ All, name = "All")
    cause <- cause %>% layout( yaxis = list(title ="Death cases")) 
    
  })
  
  #distribution plot for eath rate by board cause 1907-2019
  
  output$plot4 <- renderPlotly({
    ggplot(death_case_by_broad_cause2, aes( x= reorder(Cause, case), y=case, fill=Cause)) + 
      geom_bar( data= filter(death_case_by_broad_cause2, Year == input$year_ ),stat = "identity") +
      labs(y= "case per 10,000", x = "Cause", title = input$year_  ) +
      scale_fill_manual(values=c("#E69F00","#999999" ,"#999999", "#999999", "#999999", "#999999","#999999"))+
      theme(axis.text.x = element_text(angle=10, hjust=1, size = 7),
            legend.position="none")
  })
  
  
  
  # plot death rate 2019
  output$plot5 <- renderPlotly({
    death_2019 <- ggplot(death_sex_age_2019 , aes(x=Age.group, y = case, fill=Gender)) +
         geom_bar(data = filter(death_sex_age_2019, Gender=="Males"), stat = "identity") +
         geom_bar(data= filter(death_sex_age_2019, Gender=="Females"), aes(y = case*(-1)), stat = "identity" ) +
         scale_y_continuous(breaks = seq(-16000, 16000, 1000),
                            limits = c(-16000, 16000),
                            labels =  as.character(abs(seq(-16, 16, 1))) )  +
      labs(x="Age Group", y="case per thousand")+
      theme(axis.text.x=element_text(angle = 90) , 
            text = element_text(size=10), 
            legend.title = element_text(size=9),
            legend.key.width = unit(.8, 'cm'),#change legend title font size
            legend.text = element_text(size=8))+
      coord_flip()
      
  })
  
  # plot 6
  output$plot6 <- renderPlotly({
    
    p <- ggplot(data, aes(y=reorder(Cause.of.death,Case), x =Case)) + 
         geom_bar(data = filter(data, Gender==input$sex_), stat = "identity") +
        labs(title =input$sex_, x= "Case per thousand", y="Cause")+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=90, hjust=1, size = 10))
        
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

library(httr)
library(jsonlite)
library(readr) # to read CSV data
library(openxlsx) # to write to excel file
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)


datafile<- "C:/Users/jiskanis/Desktop/Datasets/RI Admin Data.xlsx"


setwd("C:/Users/jiskanis/Desktop/Data Science/R Programming/Shiny/Immunization2")






RI <- as.data.frame(read.xlsx(datafile))
options (scipen = 999)


RI$Total.Population <- round(RI$Total.Population,0)
RI$LB <- round(RI$LB,0)
RI$SI <- round(RI$SI,0)
RI <- gather(RI,key="antigen", value = "children", 8:25)
RI$children <- as.numeric(RI$children)

RI$coverage = NA
RI$coverage <- ifelse(is.na(RI$coverage) & RI$antigen=="BCG", round(RI$children/RI$LB*100,1) , round(RI$children/RI$SI*100,1)) 

### Summary/Aggregated Tables
## District Annual

district_a <- RI %>% 
  group_by(Province, District, Year,  antigen) %>% 
  summarise(
    LB = sum(LB, na.rm = TRUE),
    SI = sum(SI, na.rm = TRUE),
    children = sum(children, na.rm = TRUE)
  )
district_a$coverage = NA
district_a$coverage <- ifelse(is.na(district_a$coverage) & district_a$antigen=="BCG", round(district_a$children/district_a$LB*100,1) , round(district_a$children/district_a$SI*100,1)) 


## Provincial month
province_m <- RI %>% 
  group_by(Province, Year, Month, antigen) %>% 
  summarise(
    LB = sum(LB, na.rm = TRUE),
    SI = sum(SI, na.rm = TRUE),
    children = sum(children, na.rm = TRUE)
  )
province_m$coverage = NA
province_m$coverage <- ifelse(is.na(province_m$coverage) & province_m$antigen=="BCG", round(province_m$children/province_m$LB*100,1) , round(province_m$children/province_m$SI*100,1)) 



## Provincial annual
province_a <- RI %>% 
  group_by(Province, Year, antigen) %>% 
  summarise(
    LB = sum(LB, na.rm = TRUE),
    SI = sum(SI, na.rm = TRUE),
    children = sum(children, na.rm = TRUE)
  )
province_a$coverage = NA
province_a$coverage <- ifelse(is.na(province_a$coverage) & province_a$antigen=="BCG", round(province_a$children/province_a$LB*100,1) , round(province_a$children/province_a$SI*100,1)) 

### National Monthly
national_m <- RI %>% 
  group_by(Year, Month, antigen) %>% 
  summarise(
    LB = sum(LB, na.rm = TRUE),
    SI = sum(SI, na.rm = TRUE),
    children = sum(children, na.rm = TRUE)
  )
national_m$coverage = NA
national_m$coverage <- ifelse(is.na(national_m$coverage) & national_m$antigen=="BCG", round(national_m$children/national_m$LB*100,1) , round(national_m$children/national_m$SI*100,1)) 


## National Annual
national_a <- RI %>% 
  group_by(Year, antigen) %>% 
  summarise(
    LB = sum(LB, na.rm = TRUE),
    SI = sum(SI, na.rm = TRUE),
    children = sum(children, na.rm = TRUE)
  )
national_a$coverage = NA
national_a$coverage <- ifelse(is.na(national_a$coverage) & national_a$antigen=="BCG", round(national_a$children/national_a$LB*100,1) , round(national_a$children/national_a$SI*100,1)) 


## Header of Page
header <- dashboardHeader(title = "Expanded Programme on Immunization, Pakistan", titleWidth = 500)

## Side bar of Page
siderbar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Antigen wise Coverage", tabName = "antigen_wise"),
    menuItem("Province wise coverage", tabName = "province_wise")
  )
)

## Body of Page
body <- dashboardBody(
  
  tabItems(
    # First tab content
    tabItem(tabName = "antigen_wise",
            fluidPage( 
              navbarPage("Coverage",
                         tabPanel("National",
                                  fluidRow( title = "Page title",
                                            box(title = "Select Antigen: ", status = "primary", solidHeader = TRUE,width = 4,
                                                collapsible = TRUE,
                                                selectInput("antigen_name", "",
                                                            choices = national_a %>%
                                                              ungroup() %>%
                                                              select(antigen) %>%
                                                              distinct() %>%
                                                              arrange(antigen) %>%
                                                              as.list()
                                                            ) ## End antigen_name input
                                                ), # End Select Antigen Box
                                            box(title = "Boxplot (Monthly admin data)", solidHeader = TRUE, width = 8,plotOutput("boxplot_n", height = 100)
                                                ) # End boxplot box
                                            ), ### End Fluidrow Page title
                                  fluidRow(
                                    infoBoxOutput("lb"),
                                    valueBoxOutput("si"),
                                    #infoBoxOutput("si"),
                                    infoBoxOutput("Vaccinated"),
                                    infoBoxOutput("coverage")
                                    ), # End 2nd Row of Info Boxes
                                  fluidRow(
                                    box(title = "Children vaccinated and coverage(%)", solidHeader = TRUE, 
                                        height = "30%", 
                                        width = 4,
                                        tableOutput('table_n')
                                        ), # End box of table
                                    box(title = "Vaccination coverage(%)", solidHeader = TRUE,
                                        width = 8,
                                        plotOutput('plot_n')
                                        ) # End box of plot_n
                                    ) ## End 3rd Row
                       ), ## End tabPanel National
                       tabPanel("Provincial",
                                fluidRow( title = "Page title",
                                          box(title = "Select Province: ", status = "primary", solidHeader = TRUE,width = 4,
                                              collapsible = TRUE,
                                              selectInput("province_name", "",
                                                          choices = province_a %>%
                                                            ungroup() %>%
                                                            select(Province) %>%
                                                            distinct() %>%
                                                            arrange(Province) %>%
                                                            as.list()
                                                          ) # End Input for province
                                              ), # End box for province selection
                                          box(title = "Select Antigen: ", solidHeader = TRUE,width = 4,
                                              collapsible = TRUE,
                                              selectInput("antigen_name_p", "",
                                                          choices = province_a %>%
                                                            ungroup() %>%
                                                            select(antigen) %>%
                                                            distinct() %>%
                                                            arrange(antigen) %>%
                                                            as.list()
                                                          ) # End of antigen selection
                                              ), ## End of box for antigen selection
                                          box(title = "Boxplot (Monthly admin data)", solidHeader = TRUE, width = 4,
                                              plotOutput("boxplot_p", height = 100)
                                              ) # End of box for box plot provincial
                                          ), # End of Row 1
                                fluidRow(
                                  box(title = "Children vaccinated and coverage(%)", solidHeader = TRUE,
                                      height = "30%",
                                      width = 12,
                                      plotOutput('plot_p')
                                      ) # End of box for plot_p
                                  ), # End of Row 2
                                ), ## End of Provincial Tab Panel
                       tabPanel("District",
                                fluidRow( 
                                          
                                          box(title = "Select District: ", status = "primary", solidHeader = TRUE,width = 4,
                                              collapsible = TRUE,
                                              selectInput("district_name", "",
                                                          choices = district_a %>%
                                                            ungroup() %>%
                                                            select(District) %>%
                                                            distinct() %>%
                                                            arrange(District) %>%
                                                            as.list()
                                              ) # End Input for province
                                          ), # End box for district selection
                                          box(title = "Select Antigen: ", solidHeader = TRUE,width = 4,
                                              collapsible = TRUE,
                                              selectInput("antigen_name_d", "",
                                                          choices = province_a %>%
                                                            ungroup() %>%
                                                            select(antigen) %>%
                                                            distinct() %>%
                                                            arrange(antigen) %>%
                                                            as.list()
                                              ) # End of antigen selection
                                          ) ## End of box for antigen selection
                                          
                                ), # End of Row 1
                                fluidRow(
                                  box(title = "Boxplot (Monthly admin data)", solidHeader = TRUE, width = 12,
                                          #plotlOutput("boxplot_d", height = 100)
                                      plotlyOutput("boxplot_d", height = 100)
                                      )
                                ), # End of Row 2
                                fluidRow(
                                  box(title = "Children vaccinated and coverage(%)", solidHeader = TRUE,
                                      width = 12,
                                      height = "100px",
                                      plotlyOutput('plot_d')
                                  ) # End of box for plot_d
                                )
                       ) ## End of District Tab Panel
                       ) ## End of NavBar Coverage
              ) ## End of FluidPage
            ) # End of TabItem
    ) # End of TabItems
  ) ## End of Body



## Page of shiny
ui <- dashboardPage(skin="green", header, siderbar, body)



server <- function(input, output) { 
  national_a$children <- format(national_a$children, big.mark=',', scientific=FALSE) 
  national_a$coverage <- round(national_a$coverage,0)

  
  output$lb <- renderInfoBox({
    infoBox("Live Births", 
      icon = icon("list"),
      color = "purple",
      tmp<- national_a %>%
        drop_na(children) %>%
        filter(antigen=="BCG" & children>0) %>% 
        ungroup() %>% 
        tail(n = 1) %>%  
        select(LB),
     )
  })
  
  output$si <- renderValueBox({
    infoBox("Survived Infants", 
            icon = icon("list"),
            color = "green",
            tmp<- national_a %>%
              drop_na(children) %>%
              filter(antigen=="BCG" & children>0) %>% 
              ungroup() %>% 
              tail(n = 1) %>%  
              select(SI),
    )
  })
  
  output$text_out = renderText(input$antigen_name)
 
#  output$boxplot_n <- renderPlotly({
 #   plot_ly(national_m %>% 
  #            drop_na(children) %>% 
   #           filter(antigen==input$antigen_name & children>0),
    #         y = ~children, type = 'box', mode = 'markers')
    #})

  output$boxplot_n <- renderPlot(
    national_m %>%
      drop_na(children) %>%
      filter(antigen==input$antigen_name & children>0) %>% 
      ggplot(aes(children)) +
      geom_boxplot(notch = TRUE, fill="orange")+
      scale_x_continuous(labels = scales::comma)
    
  )
  
  ## Provincial Box Plot
  output$boxplot_p <- renderPlot(
    province_m %>%
      drop_na(children) %>%
      filter(Province==input$province_name & antigen==input$antigen_name_p & children>0) %>% 
      ggplot(aes(children)) +
      geom_boxplot(notch = TRUE, fill="orange")+
      scale_x_continuous(labels = scales::comma)
    
  )
  
  ## District Box Plot
#  output$boxplot_d <- renderPlot(
 #   district_a %>%
  #    drop_na(children) %>%
   #   filter(District == input$district_name & antigen==input$antigen_name_d & children>0) %>% 
    #  ggplot(aes(children)) +
     # geom_boxplot(notch = TRUE, fill="orange")+
  #    scale_x_continuous(labels = scales::comma)
    
  #)
  
  ## Boxplot with plotly
  output$boxplot_d <- renderPlotly(
    district_a %>%
      drop_na(children) %>%
      filter(District == input$district_name & antigen==input$antigen_name_d & children>0) %>%
      plot_ly(x = ~children, type = "box", notched=TRUE) 
  )
  

  
  
  
  ## Render table
  output$table_n <- renderTable(digits=0,
                                national_a %>% 
                                  filter(antigen==input$antigen_name) %>% 
                                  select(Year, children, coverage)
                                )
  ## Render National Bar graph               
  output$plot_n <- renderPlot(
    national_a %>% 
      filter(antigen==input$antigen_name) %>% 
      ungroup() %>% 
      select(Year, coverage) %>% 
      ggplot(aes(x=Year, y=coverage, label=coverage))+
      geom_text(vjust=-0.5)+
      geom_bar(stat = "identity", fill="olivedrab")+
      #scale_x_discrete("Year", limits=c(2011,2012,2013,2014,2015,2016, 2017,2018,2019,2020,2021))
      scale_x_discrete("Year", limits=c(2011:2021))+
      theme(axis.text.x = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5, face = "plain"))
      
    
  )
  
  ## Provincial Coverage Plot
  output$plot_p <- renderPlot(
    province_a %>% 
      filter(Province==input$province_name & antigen==input$antigen_name_p) %>% 
      ungroup() %>% 
      select(Year, coverage) %>% 
      ggplot(aes(x=Year, y=coverage, label=coverage))+
      geom_text(vjust=-0.5)+
      geom_bar(stat = "identity", fill="olivedrab")+
      #scale_x_discrete("Year", limits=c(2011,2012,2013,2014,2015,2016, 2017,2018,2019,2020,2021))
      scale_x_discrete("Year", limits=c(2011:2021))+
      theme(axis.text.x = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5, face = "plain")
            )
    ) ## End of Plot_p
  
  
  ## District Plotly
  ## Graph through Plotly
  output$plot_d <- renderPlotly( 
    district_a %>%
    filter(District==input$district_name & antigen==input$antigen_name_d) %>% 
    ungroup() %>% 
    select(Year, children, coverage) %>%
    plot_ly() %>%
    add_trace(x = ~Year, y = ~children,type = "bar",  name = "No of children vaccinated") %>%  
    add_trace(x = ~Year, y = ~coverage, mode = "lines", yaxis = "y2", name = "Coverage(%)") %>%
    layout(yaxis2 = list(overlaying = "y", side = "right"))
  )
  
  
  ## District Coverage Plot
 # output$plot_d <- renderPlot(
  #  district_a %>% 
   #   filter(District==input$district_name & antigen==input$antigen_name_d) %>% 
    #  ungroup() %>% 
     # select(Year, children, coverage) %>% 
    #  ggplot(aes(Year, children,  label = format(children, big.mark = ","))) +
     # geom_col(aes(fill = 'children')) +
#      geom_text(position=position_stack(vjust = .5), color = "black")+
 #     geom_point(aes(Year, coverage*max(children/max(coverage)), color = 'Coverage'), group=1) +
  #    geom_line(aes(Year, coverage*max(children/max(coverage)), color = 'Coverage'), size=1) +
   #   geom_label(
    #    aes(Year, coverage*max(children/max(coverage)), label = coverage),
     #   vjust = 1.4,
      #  show.legend = F) +
#      scale_y_continuous(labels = scales::comma,
 #                        sec.axis = sec_axis(~ (. - a) / b ))+
  #    coord_cartesian(ylim = c(x1, y2))+
   #   theme(axis.text.y=element_blank(), axis.title.y=element_blank(), legend.position = "bottom", 
    #        axis.text.x = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5, face = "plain"))+
     # labs(fill = NULL, color = NULL) +
      #scale_fill_manual('', labels = 'No of children vaccinated', values = "lightseagreen") +
#      scale_color_manual('', labels = 'Coverage (%)', values = 'black')  +
 #     scale_x_discrete("Year", limits=c(2011:2021))
    
    
#  ) ## End of Plot_p
  
  
  
}

shinyApp(ui, server)



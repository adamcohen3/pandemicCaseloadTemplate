#-----------------------------------#
#pandemicCaseLoadsTemplate.R----
#created on 2021/04/30 by Adam S. Cohen (Hawaii State Judiciary)
#Last modified on 2021/04/30 by Adam S. Cohen (Hawaii State Judiciary)
#-----------------------------------#


#rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(plotly)
library(shiny)

#-----------------------------------#
#read in files----
#-----------------------------------#
df <- read.csv('toyCaseloadData_v1.csv')

#-----------------------------------#
#themes----
#-----------------------------------#
theme_ac1 <- theme(axis.line = element_line(size=1, colour = "black"),
                   #panel.grid.major = element_line(colour = "#d3d3d3"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(), panel.background = element_blank(),
                   plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
                   plot.caption = element_text(size = 9, color = "blue"),
                   text=element_text(family="Tahoma"),
                   axis.text=element_text(color="black", size = 12, hjust = .9),
                   axis.title.x=element_text(color="black", size = 14),
                   axis.title.y=element_text(color="black", size = 14),
                   legend.text = element_text(size = 12),
                   legend.title = element_text(size = 14))

theme_plotly <- theme(axis.line = element_line(size=1, colour = "black"),
                      #panel.grid.major = element_line(colour = "#d3d3d3"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(), panel.background = element_blank(),
                      plot.title = element_text(size = 12, family = "Tahoma", face = "bold"),
                      plot.caption = element_text(size = 10, color = "blue"),
                      text=element_text(family="Tahoma"),
                      axis.text=element_text(color="black", size = 10, hjust = .9),
                      axis.title=element_text(color="black", size = 12))

#-----------------------------------#
#set up data----
#-----------------------------------#
dfAll <- df %>%
    group_by(Year,Month) %>%
    summarize(Case.Category = "All", Filings = sum(Filings), Dispositions = sum(Dispositions)) %>%
    select(Case.Category, Year, Month, Filings, Dispositions) %>%
    ungroup()
dfAll$Case.Category <- as.factor(dfAll$Case.Category)

dfCombo <- bind_rows(df,dfAll)

#valueboxes----
valueBox0 <- function(value, subtitle, icon, color) {
    div(class = "col-lg col-md",
        div(class = "panel panel-primary",
            div(class = "panel-heading", style = paste0("background-color:", color),
                div(class = "row",
                    # div(class = "col-xs-3",
                    #     icon(icon, "fa-5x")
                    # ),
                    div(class = ("col-xs text-center"),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            subtitle),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            textOutput(value)
                        )
                    )
                )
            ),
            div(class = "panel-footer",
                div(class = "clearfix")
            )
        )
    )
}
valueBox1 <- function(value, subtitle, icon, color) {
    div(class = "col-lg col-md",
        div(class = "panel panel-primary",
            div(class = "panel-heading", style = paste0("background-color:", color),
                div(class = "row",
                    # div(class = "col-xs-3",
                    #     icon(icon, "fa-5x")
                    # ),
                    div(class = ("col-xs text-center"),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            subtitle),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            textOutput(value)
                        )
                    )
                )
            ),
            div(class = "panel-footer",
                div(class = "clearfix")
            )
        )
    )
}

valueBox2 <- function(value, subtitle, icon, color) {
    div(class = "col-lg col-md",
        div(class = "panel panel-primary",
            div(class = "panel-heading", style = paste0("background-color:", color),
                div(class = "row",
                    # div(class = "col-xs-3",
                    #     icon(icon, "fa-5x")
                    # ),
                    div(class = ("col-xs text-center"),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            subtitle),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            textOutput(value)
                        )
                    )
                )
            ),
            div(class = "panel-footer",
                div(class = "clearfix")
            )
        )
    )
}

valueBox3 <- function(value, subtitle, icon, color) {
    div(class = "col-lg col-md",
        div(class = "panel panel-primary",
            div(class = "panel-heading", style = paste0("background-color:", color),
                div(class = "row",
                    # div(class = "col-xs-3",
                    #     icon(icon, "fa-5x")
                    # ),
                    div(class = ("col-xs text-center"),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            subtitle),
                        div(style = ("font-size: 20px; font-weight: bold;"),
                            textOutput(value)
                        )
                    )
                )
            ),
            div(class = "panel-footer",
                div(class = "clearfix")
            )
        )
    )
}

#-----------------------------------#
# SHINY----
#-----------------------------------#

# Define UI for application that draws a histogram----
ui <- fluidPage(

    # Application title
    titlePanel("Pandemic Caseloads: Court Filings and Dispositions in [STATE], 2019-2020"),

    fluidRow(column(width = 4, selectInput(inputId = "caseCategory",
                                           label = "Select case category:",
                                           choices = c("(All)" = "All",
                                                       "Adult criminal" = "Criminal",
                                                       "Civil" = "Civil",
                                                       "Domestic Relations" = "Family",
                                                       "Probate" = "Probate",
                                                       "Traffic" = "Traffic"),
                                           selected = "all"))
    ),
    
    #___valueboxes----
    fluidRow(column(width = 3, valueBox0(value = "overAllText",
                                         subtitle = "",
                                         icon = "",
                                         color = "")),
        
             column(width = 3, valueBox1(value = "filingsText",
                                         subtitle = "",
                                         icon = "",
                                         color = "blue")),
   
              column(width = 3, valueBox2(value = "dispositionsText",
                                         subtitle = "",
                                         icon = "",
                                         color = "green")),
   
               column(width = 3, valueBox3(value = "growthText",
                                         subtitle = "",
                                         icon = "",
                                         color = "purple"))
    ),
    
    #___plotlyOutputs----
    fluidRow(column(width = 6, plotlyOutput("caseLoads2020Plot")),
             column(width = 6, plotlyOutput("filingsPlot"))
    ),
    fluidRow(column(width = 12, plotlyOutput("accumPendingPlot"))
    )
)

# Define server logic required to draw a histogram----
server <- function(input, output) {

    output$overAllText <- renderText({ 
        paste0('2020 ',input$caseCategory, ' Cases')
    })

    output$filingsText <- renderText({ 
        filings2020 <- dfCombo %>%
            filter(Year == 2020, Case.Category == input$caseCategory) 
        paste0("Filings: ",sum(filings2020$Filings))
        #round(mean(2000),1)
    })
    
    output$dispositionsText <- renderText({ 
        dispositions2020 <- dfCombo %>%
            filter(Year == 2020, Case.Category == input$caseCategory) 
        paste0("Dispositions: ", sum(dispositions2020$Dispositions))
    })
    
    output$growthText <- renderText({ 
        growth2020 <- dfCombo %>%
            filter(Year == 2020, Case.Category == input$caseCategory) 
        paste0("Growth of active pending: ",sum(growth2020$Filings) - sum(growth2020$Dispositions))
    })
    
    #caseLoads2020Plot----
    output$caseLoads2020Plot <- renderPlotly({
        
        caseLoad2020 <- dfCombo %>%
        #note that pivot_longer created an error in the text hover when trying to display the countTotals...could not determine the source of the error, maybe a bug?
        #work around solution was to manually create new dataframe for caseLoad2020 using dplyr functions
            #pivot_longer(cols = c('Filings',"Dispositions"), names_to = "action", values_to = "countTotal") %>%
            filter(Year == 2020, Case.Category == input$caseCategory) %>%
            mutate(date = as.Date(paste0(Year,'-',Month,'-01'),format = '%Y-%B-%d'),
                   monthNum = match(as.character(Month),month.name),
                   monthNumRev = as.factor(-1*((as.numeric(match(as.character(Month),month.name)))-13))) 
        #workaround solution to pivot_longer problem above:
        caseLoad2020Filings <- caseLoad2020 %>%
            select(-Dispositions) %>%
            rename(countTotal = Filings) %>%
            mutate(action = "Filings")
        caseLoad2020Dispositions <- caseLoad2020 %>%
            select(-Filings) %>%
            rename(countTotal = Dispositions) %>%
            mutate(action = "Dispositions")
        caseLoad2020 <- rbind(caseLoad2020Filings,caseLoad2020Dispositions)
        
        ggplotly(ggplot(caseLoad2020,aes(x=monthNumRev, y=countTotal, fill = as.factor(action), width = if_else(action=='Filings',0.6,0.3),
                                            text = paste0(Month,' | ', 'Filings: ', countTotal[action == 'Filings'], '\n',
                                                          Month,' | ', 'Dispositions: ', countTotal[action == 'Dispositions'], '\n',
                                                          'Difference between Filings and Dispositions: ', countTotal[action=='Filings'] - countTotal[action=='Dispositions']))) +
                     #geom_col() +
                     geom_bar(stat="identity",position = "identity", alpha=.6) +
                     scale_fill_manual("",labels =  c("Dispositions","Filings"), values = c("black","blue")) +
                     scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
                     scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
                     labs(x = '', y='') +
                     coord_flip() +
                     theme_bw() +
                     theme_plotly +
                     ggtitle("How are [STATE] courts managing 2020 caseloads?"),
                  tooltip = c('text')
        )
    })
    
    #filingsPlot----
    output$filingsPlot <- renderPlotly({
        
        filingsDF <- dfCombo %>%
            filter(Case.Category == input$caseCategory) %>%
            mutate(date = as.Date(paste0(Year,'-',Month,'-01'),format = '%Y-%B-%d'),
                   monthNum = match(as.character(Month),month.name),
                   monthNumRev = as.factor(-1*((as.numeric(match(as.character(Month),month.name)))-13)))
        
        ggplotly(ggplot(filingsDF,aes(x=monthNumRev, y=Filings, fill = as.factor(Year), width = if_else(Year=='2020',0.6,0.3),
                                      text = paste0(Month,' | ', '2020 Filings: ', Filings[Year == 2020], '\n',
                                                    Month,' | ', '2019 Filings: ', Filings[Year == 2019], '\n',
                                                    'Change 2019 to 2020: ', Filings[Year == 2020] - Filings[Year == 2019]))) +
                     # geom_col(alpha = .3, position = "identity") +
                     # scale_fill_manual("Year",labels =  c("2019","2020"), values = c("gray50","blue")) +
                     geom_bar(stat="identity",position = "identity", alpha=.6) +
                     scale_fill_manual("Year",labels =  c("2019","2020"), values = c("gray50","blue")) +
                     scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
                     scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
                     labs(x = '', y='') +
                     coord_flip() +
                     theme_plotly +
                     ggtitle("How do 2020 filings compare to filings in 2019"),
                 tooltip = c('text')
        )
    })
    
    #accumPendingPlot----
    output$accumPendingPlot <- renderPlotly({
        
        accumDF1 <- dfCombo %>%
            filter(Case.Category == input$caseCategory) %>%
            mutate(date = as.Date(paste0(Year,'-',Month,'-01'),format = '%Y-%B-%d'),
                   monthNum = match(as.character(Month),month.name),
                   monthNumRev = as.factor(-1*((as.numeric(match(as.character(Month),month.name)))-13)))
        
        accumDF2 <- accumDF1 %>%
            arrange(monthNum) %>%
            group_by(Year) %>%
            mutate(accumFilings = cumsum(Filings-Dispositions)) %>%
            ungroup()
        
        options(scipen = 10000)  

        ggplotly(ggplot(accumDF2,aes(x=lubridate::month(date, label = TRUE, abbr = TRUE), y=accumFilings, color = as.factor(Year), 
                                     group = as.factor(Year), text = paste0('At the end of ',Month,' ',Year,',',format(accumFilings, big.mark = ','),' more cases were pending than on January 1, ',Year,'. \n',
                                                                            'During the month of ',Month,',',format(Filings-Dispositions, big.mark = ','),' cases were added to the active pending caseload.'))) +
            geom_line() + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') +
            #scale_x_date(date_breaks = '1 month', date_labels = '%B', expand = c(0,0)) +
            scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
                scale_color_discrete() +
            labs(x = '', y='', color = "Year") +
            theme_bw() +
            ggtitle("How does the accumulation of active pending cases in 2020 compare to 2019?"),
            tooltip = c('text'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

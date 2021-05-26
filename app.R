#-----------------------------------#
#pandemicCaseLoadsTemplate Dashboard----
#created on 2021/04/30 by Adam S. Cohen (Hawaii State Judiciary)
#Last modified on 2021/05/25 by Adam S. Cohen (Hawaii State Judiciary)
#-----------------------------------#

#-----------------------------------#
#clear workspace and load libraries----
#-----------------------------------#
#rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(plotly)
library(shiny)
library(shinydashboard)

#-----------------------------------#
#read in files----
#-----------------------------------#
#df <- read.csv('H:\\JIMS_SQL\\JIMS_requests\\2021-02-04_NCSC_flash_data_request_03\\pandemicCaseloadsTemplate\\toyCaseloadData_v2.csv')
df <- read.csv('toyCaseloadData_v2.csv')

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

#-----------------------------------#
# SHINY----
#-----------------------------------#

ui <- dashboardPage(
    dashboardHeader(title = "Pandemic Caseloads: Court Filings and Dispositions in [STATE], 2019-2021",
                    titleWidth = 710),
    dashboardSidebar(sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About the data", tabName = "about", icon = icon("info")),
        menuItem("_Github", href = "https://github.com/adamcohen3/pandemicCaseloadTemplate", icon = icon("github"))
        )
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard", class = "active",
                    
                    fluidRow(
                        box(width = 2, background = 'blue',
                            selectInput(inputId = "caseCategory",
                                        label = "Select case category:",
                                        choices = c("(All)" = "All",
                                                    "Criminal" = "Criminal",
                                                    "Civil" = "Civil",
                                                    "Domestic Relations" = "Family",
                                                    "Probate" = "Probate",
                                                    "Traffic" = "Traffic"),
                                        selected = "All")
                        )
                    ),
                    
                    fluidRow(valueBoxOutput("overAllText", width = 3),
                             
                             valueBoxOutput("filingsText", width = 3),
                             
                             valueBoxOutput("dispositionsText", width = 3),
                             
                             valueBoxOutput("growthText", width = 3)
                    ),
                    
                    fluidRow(box(title = "", width = 6, status = "primary", plotlyOutput("caseLoads2020Plot")),
                             tabBox(width = 6,
                                    tabPanel("2020 v 2019 filings", plotlyOutput("filings2019to2020Plot")),
                                    tabPanel("2020 v 2021 filings", plotlyOutput("filings2020to2021Plot"))
                             )
                             # column(width = 4, plotlyOutput("filings2019to2020Plot")),
                             # column(width = 4, plotlyOutput("filings2020to2021Plot"))
                    ),
                    br(),
                    fluidRow(box(title = "", width = 12, status = "primary", plotlyOutput("accumPendingPlot"))
                    )
            ),
            tabItem(tabName = "about",
                    h3("About the data"),
                    # h4("Clarifications/Disclaimers/Caveats/Warnings/Limitations"),
                    p("The Pandemic Caseload Dashboard shows court filings and dispositions statewide in [STATE] for each
                      month of 2020. By using the data selector, data can be looked at in aggregate or broken down by  
                      case categories. The dashboard also 
                      compares filings from 2020 to 2019 and 2021. Finally, it shows the accumulation of active pending cases for 
                      2019, 2020, and 2021."),
                    
                    p("Hover over graphs to display specific values. Click on variables in the legend 
                      to make hide/display data. Use the camera button in the upper right corner of each plot to download
                      the graph."),
                    
                    p("Data were pulled from [source] on [date]."), 
                    br(),


                    h3('About the dashboard'),
                    p("The dashboard is best viewed on a desktop. Formatting may be off on mobile devices and smaller 
                    desktops. Please send questions, comments, and feedback to ",a("Adam Cohen.", 
                                                                                   href = 'mailto:adam.s.cohen@courts.hawaii.gov')),
                    br(),br(),br()
            )
        )
    )
)


# Define server logic----
server <- function(input, output) {

    output$overAllText <- renderValueBox({ 
        valueBox(format(paste0(input$caseCategory),big.mark = ','), '2020 Cases', icon = icon("folder"), color = "blue")
    })
    
    output$filingsText <- renderValueBox({ 
        filings2020 <- dfCombo %>%
            filter(Year == 2020, Case.Category == input$caseCategory) 
        valueBox(format(sum(filings2020$Filings),big.mark = ','), 'Filings', icon = icon("folder-open"), color = "blue")
    })
    
    output$dispositionsText <- renderValueBox({ 
        dispositions2020 <- dfCombo %>%
            filter(Year == 2020, Case.Category == input$caseCategory) 
        valueBox(format(sum(dispositions2020$Dispositions),big.mark = ','), 'Dispositions', icon = icon("file-contract"), color = "blue")
    })
    
    output$growthText <- renderValueBox({ 
        growth2020 <- dfCombo %>%
            filter(Year == 2020, Case.Category == input$caseCategory) 
        valueBox(format(sum(growth2020$Filings) - sum(growth2020$Dispositions),big.mark = ','), 'Growth of active pending', icon = icon("chart-line"), color = "blue")
        
    })
    
    #caseLoads2020Plot----
    
    caseLoad2020 <- reactive({
        caseLoad2020.tmp <- dfCombo %>%
            #note that pivot_longer created an error in the text hover when trying to display the countTotals...could not determine the source of the error, maybe a bug?
            #work around solution was to manually create new dataframe for caseLoad2020 using dplyr functions
            #pivot_longer(cols = c('Filings',"Dispositions"), names_to = "action", values_to = "countTotal") %>%
            filter(Year == 2020, Case.Category == input$caseCategory) %>%
            mutate(date = as.Date(paste0(Year,'-',Month,'-01'),format = '%Y-%B-%d'),
                   monthNum = match(as.character(Month),month.name),
                   monthNumRev = as.factor(-1*((as.numeric(match(as.character(Month),month.name)))-13))) 
        #workaround solution to pivot_longer problem above:
        caseLoad2020Filings <- caseLoad2020.tmp %>%
            select(-Dispositions) %>%
            rename(countTotal = Filings) %>%
            mutate(action = "Filings")
        caseLoad2020Dispositions <- caseLoad2020.tmp %>%
            select(-Filings) %>%
            rename(countTotal = Dispositions) %>%
            mutate(action = "Dispositions")
        caseLoad2020 <- rbind(caseLoad2020Filings,caseLoad2020Dispositions)
    })
    
    
    output$caseLoads2020Plot <- renderPlotly({
        ggplotCaseloads2020 <- ggplot(caseLoad2020(),aes(x=monthNumRev, y=countTotal, fill = as.factor(action), 
                                                         width = if_else(action=='Filings',0.6,0.3),
                                                         text = paste0(Month,' | ', 'Filings: ', countTotal[action == 'Filings'], '\n',
                                                                       Month,' | ', 'Dispositions: ', countTotal[action == 'Dispositions'], '\n',
                                                                       'Difference between Filings and Dispositions: ', countTotal[action=='Filings'] - countTotal[action=='Dispositions']))) +
            #geom_col() +
            geom_bar(stat="identity",position = "identity", alpha=.6) +
            scale_fill_manual("",labels =  c("Dispositions","Filings"), values = c("black","blue")) +
            #option #1 - set aes(x = monthNumRev...)
            scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
            #option #2 - set aes(x = monthNum...)
            #scale_x_reverse(breaks = 1:12, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
            # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
            # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4), limits = c(0,60000)) +
            # scale_y_continuous(labels = seq(0,max(caseLoad2020$countTotal),length.out = 6)) +
            labs(x = '', y='') +
            coord_flip() +
            theme_bw() +
            theme_plotly +
            ggtitle("How are [STATE] courts managing 2020 caseloads?")
        
        if (max(caseLoad2020()$countTotal > 1000)){
            ggplotCaseloads2020 <- ggplotCaseloads2020 +
                scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4), limits = c(0,round(max(caseLoad2020()$countTotal+1000),-3))) 
        } else if (max(caseLoad2020()$countTotal < 1000)) {
            ggplotCaseloads2020 <- ggplotCaseloads2020 +
                scale_y_continuous(limits = c(0,round(max(caseLoad2020()$countTotal+100),-2))) 
        }
        
        ggplotly(ggplotCaseloads2020,
                 tooltip = c('text')
        ) #%>%
        # rangeslider(start = 0, end = 60000, thickness = .1)
        
        
    })
    
    #filings2019to2020Plot----
    output$filings2019to2020Plot <- renderPlotly({
        
        filingsDF <- dfCombo %>%
            filter(Case.Category == input$caseCategory,(Year == 2019 | Year == 2020)) %>%
            mutate(date = as.Date(paste0(Year,'-',Month,'-01'),format = '%Y-%B-%d'),
                   monthNum = match(as.character(Month),month.name),
                   monthNumRev = as.factor(-1*((as.numeric(match(as.character(Month),month.name)))-13)))
        
        #props: https://stackoverflow.com/questions/59611914/reverse-the-legend-order-when-using-ggplotly
        reverse_legend_labels <- function(plotly_plot) {
            n_labels <- length(plotly_plot$x$data)
            plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
            plotly_plot
        }
        
        ggplotFilings20192020 <- ggplot(filingsDF,aes(x=monthNumRev, y=Filings, fill = factor(Year, levels = c(2019,2020)), width = if_else(Year==2020,0.6,0.3),
                                                      text = paste0(Month,' | ', '2020 Filings: ', Filings[Year == 2020], '\n',
                                                                    Month,' | ', '2019 Filings: ', Filings[Year == 2019], '\n',
                                                                    'Change 2019 to 2020: ', Filings[Year == 2020] - Filings[Year == 2019]))) +
            geom_col(alpha = .6, position = "identity") +
            # scale_fill_manual("Year",labels =  c("2019","2020"), values = c("gray50","blue")) +
            # geom_bar(stat="identity",position = position_dodge2(), alpha=.6) +
            scale_fill_manual("Year",labels =  c("2019","2020"), breaks = c(2019,2020), values = c("gray40","blue")) +
            scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
            # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
            # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4), limits = c(0,60000)) +
            labs(x = '', y='') +
            coord_flip() +
            theme_plotly +
            ggtitle("How do 2020 filings compare to filings in 2019?")
        
        if (max(caseLoad2020()$countTotal > 1000)){
            ggplotFilings20192020 <- ggplotFilings20192020 +
                # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) 
                scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4), limits = c(0,round(max(caseLoad2020()$countTotal+1000),-3))) 
        } else if (max(caseLoad2020()$countTotal < 1000)) {
            ggplotFilings20192020 <- ggplotFilings20192020 +
                scale_y_continuous(limits = c(0,round(max(caseLoad2020()$countTotal+100),-2))) 
        }
        
        
        ggplotly(ggplotFilings20192020,
                 tooltip = c('text')
        )# %>%
        # reverse_legend_labels()
        
    })
    
    #filings2020to2021Plot----
    output$filings2020to2021Plot <- renderPlotly({
        
        filingsDF <- dfCombo %>%
            filter(Case.Category == input$caseCategory,(Year == 2020 | Year == 2021)) %>%
            mutate(date = as.Date(paste0(Year,'-',Month,'-01'),format = '%Y-%B-%d'),
                   monthNum = match(as.character(Month),month.name),
                   monthNumRev = as.factor(-1*((as.numeric(match(as.character(Month),month.name)))-13)))
        
        tmp <- filingsDF %>%
            filter(Year == 2020, monthNum > nrow(filter(filingsDF,Year == 2021))) %>%
            mutate(Year = 2021, Filings = 0, Dispositions = 0, date = date + 365) %>%
            bind_rows(filter(filingsDF,Year==2021)) %>%
            slice(match(filingsDF$Month[filingsDF$Year==2020],Month))
        
        
        filingsDF <- filingsDF %>%
            filter(Year == 2020) %>%
            bind_rows(tmp)
        
        #props: https://stackoverflow.com/questions/59611914/reverse-the-legend-order-when-using-ggplotly
        reverse_legend_labels <- function(plotly_plot) {
            n_labels <- length(plotly_plot$x$data)
            plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
            plotly_plot
        }
        
        ggplotFilings20202021 <- ggplot(filingsDF,aes(x=monthNumRev, y=Filings, fill = factor(Year, levels = rev(c(2020,2021))), width = if_else(Year==2020,0.6,0.3),
                                                      text = paste0(Month,' | ', '2021 Filings: ', Filings[Year == 2021], '\n',
                                                                    Month,' | ', '2020 Filings: ', Filings[Year == 2020], '\n',
                                                                    'Change 2020 to 2021: ', Filings[Year == 2021] - Filings[Year == 2020]))) +
            geom_col(alpha = .6, position = "identity") +
            # geom_bar(stat="identity",position = position_dodge2(), alpha=.6) +
            scale_fill_manual("Year",labels =  c("2020","2021"), breaks = c(2020,2021), values = c("blue",'yellow')) +
            scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
            # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
            # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4), limits = c(0,60000)) +
            labs(x = '', y='') +
            coord_flip() +
            theme_plotly +
            ggtitle("How do 2020 filings compare to filings in 2021")
        
        if (max(caseLoad2020()$countTotal > 1000)){
            ggplotFilings20202021 <- ggplotFilings20202021 +
                # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) 
                scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4), limits = c(0,round(max(caseLoad2020()$countTotal+1000),-3))) 
        } else if (max(caseLoad2020()$countTotal < 1000)) {
            ggplotFilings20202021 <- ggplotFilings20202021 +
                scale_y_continuous(limits = c(0,round(max(caseLoad2020()$countTotal+100),-2))) 
        }
        
        ggplotly(ggplotFilings20202021,
                 tooltip = c('text')
        ) %>%
            reverse_legend_labels()
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
        
        #2019 vs 2020 (no 2021)
        # ggplot(accumDF2,aes(x=lubridate::month(date, label = TRUE, abbr = TRUE), y=accumFilings, color = as.factor(Year), group = as.factor(Year))) +
        #     geom_line() + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') +
        #     #scale_x_date(date_breaks = '1 month', date_labels = '%B', expand = c(0,0)) +
        #     scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
        #     scale_color_discrete() +
        #     labs(x = '', y='', color = "Year") +
        #     theme_bw() +
        #     theme_ac1 +
        #     ggtitle("How does the accumulation of active pending cases in 2020 compare to 2019?")
        
        ggplotly(ggplot(accumDF2,aes(x=lubridate::month(date, label = TRUE, abbr = TRUE), y=accumFilings, color = as.factor(Year), 
                                     group = as.factor(Year), text = paste0('At the end of ',Month,' ',Year,',',format(accumFilings, big.mark = ','),' more cases were pending than on January 1, ',Year,'. \n',
                                                                            'During the month of ',Month,',',format(Filings-Dispositions, big.mark = ','),' cases were added to the active pending caseload.'))) +
                     geom_line() + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') +
                     #scale_x_date(date_breaks = '1 month', date_labels = '%B', expand = c(0,0)) +
                     scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
                     scale_color_manual(breaks = c(2019,2020,2021), values = c('gray40','blue','yellow')) +
                     labs(x = '', y='', color = "Year") +
                     theme_bw() +
                     ggtitle("How does the accumulation of active pending cases in 2020 compare to 2019 and 2021?"),
                 tooltip = c('text'))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

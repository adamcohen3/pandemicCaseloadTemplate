#-----------------------------------#
#pandemicCaseLoadsTemplate Dashboard----
#created on 2021/04/30 by Adam S. Cohen (Hawaii State Judiciary)
#based on the pandemic caseload dashboard created by Sarah Gibson (NCSC)
#Last modified on 2021/06/22 by Adam S. Cohen (Hawaii State Judiciary)
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
#df <- read.csv('H:\\p12_caseload_dashboard\\pandemicCaseloadsTemplate\\toyCaseloadData_v3.csv')
df <- read.csv('toyCaseloadData_v3.csv')

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
# Define UI----
#-----------------------------------#

ui <- dashboardPage(
    dashboardHeader(title = "Pandemic Caseloads: Court Filings and Dispositions in [STATE], 2019-2021",
                    titleWidth = 710),
    dashboardSidebar(sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About the data", tabName = "about", icon = icon("info")),
        menuItem("_Github", href = "https://github.com/adamcohen3/pandemicCaseloadsTemplate", icon = icon("github"))
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

                    fluidRow(box(title = actionButton('button','Click to see changes in active pending and "shadow" cases for 2020'),
                                 width = 12, status = "primary", 
                                 plotlyOutput("accumPendingPlot"))
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


                    h3('Acknowledgements'),
                    p('Many thanks to ', strong('Barbara Bettes'), '(Research and Statistics Office, Hawaii State
                    Judiciary) for answering (many) questions and providing feedback.',br(),
                      'Credit also goes to ', strong('Sarah Gibson'), '(National Center for State Courts) for building a 
                    pandemic caseload dashboard that served as an inspiration and template for this one.'),
                    
                    h3('About the dashboard'),
                    p("The dashboard is best viewed on a desktop. Formatting may be off on mobile devices and smaller 
                    desktops. Please send questions and comments to ",a("Adam Cohen.", 
                                                                                   href = 'mailto:adam.s.cohen@courts.hawaii.gov')),
                    br(),br(),br()
            )
        )
    )
)


# Define server logic----
server <- function(input, output, session) {

    #-----------------------------------#
    #render value boxes----
    #-----------------------------------#
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
    
    #-----------------------------------#
    #rev legend labels
    #props: https://stackoverflow.com/questions/59611914/reverse-the-legend-order-when-using-ggplotly
    #-----------------------------------#
    reverse_legend_labels <- function(plotly_plot) {
        n_labels <- length(plotly_plot$x$data)
        plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
        plotly_plot
    }
    
    #-----------------------------------#
    #caseLoads2020Plot----
    #-----------------------------------#
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
    
    #set matching axis limits for caseLoads2020Plot, filings2019to2020Plot, and filings2020to2021Plot
    axisSettings <- reactive({
        maxLimit <- dfCombo %>%
            filter(Case.Category == input$caseCategory) %>%
            summarize(maxFilings = max(Filings), maxDispositions = max(Dispositions))
        maxLimit2 <- max(maxLimit)
        
        #set scale_y_continuous scaling unit based on max value of caseLoad countTotal
        #set scale_y_continuous so limits for caseLoads[YEAR]Plot and Filings[YEAR]Plots match, round to nearest 1K or 100 and add 1K or 100 so no data gets cutoff
        if (maxLimit2 > 1000){
            gglabels <- scales::unit_format(unit = "K", scale = 10e-4)
            gglimits <- c(0,round(maxLimit2+1000,-3))
        } 
        else if (maxLimit2 < 1000) {
            gglabels = waiver()
            gglimits = c(0,round(maxLimit2+100,-2))
        }
        return(list(gglabels=gglabels, gglimits=gglimits))
    })
    
    output$caseLoads2020Plot <- renderPlotly({
        ggplotCaseloads2020 <- ggplot(caseLoad2020(),aes(x=monthNumRev, y=countTotal, fill = as.factor(action), 
                                                         width = if_else(action=='Filings',0.6,0.3),
                                                         text = paste0(Month,' | ', 'Filings: ', countTotal[action == 'Filings'], '\n',
                                                                       Month,' | ', 'Dispositions: ', countTotal[action == 'Dispositions'], '\n',
                                                                       'Difference between Filings and Dispositions: ', countTotal[action=='Filings'] - countTotal[action=='Dispositions']))) +
            geom_bar(stat="identity",position = "identity", alpha=.6) +
            scale_fill_manual("",labels =  c("Dispositions","Filings"), values = c("tomato3","dodgerblue")) +
            #option #1 - set aes(x = monthNumRev...)
            scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
            #option #2 - set aes(x = monthNum...)
            #scale_x_reverse(breaks = 1:12, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
            scale_y_continuous(labels = axisSettings()$gglabels, limits = axisSettings()$gglimits) +
            labs(x = '', y='') +
            coord_flip() +
            theme_bw() +
            theme_plotly +
            ggtitle("How are [STATE] courts managing 2020 caseloads?")

        ggplotly(ggplotCaseloads2020,
                 tooltip = c('text')
        ) %>%
            reverse_legend_labels()
        # rangeslider(start = 0, end = 60000, thickness = .1)
        
        
    })
    #-----------------------------------#
    #filings2019to2020Plot----
    #-----------------------------------#
    output$filings2019to2020Plot <- renderPlotly({
        
        filingsDF <- dfCombo %>%
            filter(Case.Category == input$caseCategory,(Year == 2019 | Year == 2020)) %>%
            mutate(date = as.Date(paste0(Year,'-',Month,'-01'),format = '%Y-%B-%d'),
                   monthNum = match(as.character(Month),month.name),
                   monthNumRev = as.factor(-1*((as.numeric(match(as.character(Month),month.name)))-13)))
        
        ggplotFilings20192020 <- ggplot(filingsDF,aes(x=monthNumRev, y=Filings, fill = factor(Year, levels = c(2019,2020)), width = if_else(Year==2020,0.6,0.3),
                                                      text = paste0(Month,' | ', '2020 Filings: ', Filings[Year == 2020], '\n',
                                                                    Month,' | ', '2019 Filings: ', Filings[Year == 2019], '\n',
                                                                    'Change 2019 to 2020: ', Filings[Year == 2020] - Filings[Year == 2019]))) +
            geom_col(alpha = .6, position = "identity") +
            scale_fill_manual("Year",labels =  c("2019","2020"), breaks = c(2019,2020), values = c("black","dodgerblue")) +
            scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
            scale_y_continuous(labels = axisSettings()$gglabels, limits = axisSettings()$gglimits) +
            labs(x = '', y='') +
            coord_flip() +
            theme_plotly +
            ggtitle("How do 2020 filings compare to filings in 2019?")

        ggplotly(ggplotFilings20192020,
                 tooltip = c('text')
        ) %>%
            reverse_legend_labels()
        
    })
    
    #-----------------------------------#
    #filings2020to2021Plot----
    #-----------------------------------#
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

        ggplotFilings20202021 <- ggplot(filingsDF,aes(x=monthNumRev, y=Filings, fill = factor(Year, levels = rev(c(2020,2021))), width = if_else(Year==2020,0.6,0.3),
                                                      text = paste0(Month,' | ', '2021 Filings: ', Filings[Year == 2021], '\n',
                                                                    Month,' | ', '2020 Filings: ', Filings[Year == 2020], '\n',
                                                                    'Change 2020 to 2021: ', Filings[Year == 2021] - Filings[Year == 2020]))) +
            geom_col(alpha = .6, position = "identity") +
            scale_fill_manual("Year",labels =  c("2020","2021"), breaks = c(2020,2021), values = c("dodgerblue",'goldenrod1')) +
            scale_x_discrete(labels = rev(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) +
            scale_y_continuous(labels = axisSettings()$gglabels, limits = axisSettings()$gglimits) +
            labs(x = '', y='') +
            coord_flip() +
            theme_plotly +
            ggtitle("How do 2020 filings compare to filings in 2021")
        
        ggplotly(ggplotFilings20202021,
                 tooltip = c('text')
        ) %>%
            reverse_legend_labels()
    })
    
    #-----------------------------------#
    #accumPendingPlot----
    #-----------------------------------#
    #counter for shadow cases button and dataset
    vars = reactiveValues(counter = 0)
    
    #update action button label and increment counter on click
    observeEvent(input$button, {
        if(vars$counter %% 2 == 1){
            vars$counter <- vars$counter + 1
            updateActionButton(inputId = 'button',paste0('Click to see changes in active pending and "shadow" cases for 2020'), session = session)
        }
        else{
            vars$counter <- vars$counter + 1
            updateActionButton(inputId = 'button',paste0('Click to compare accumulated pending cases in 2020 to 2019 and 2021'), session = session)
        }
    })
    
    
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
        
        #compute shadow cases by comparing 2020 to 2019 filings
        filings2019 <- accumDF2 %>%
            filter(Year == 2019) %>%
            select('month2019' = Month, 'filings2019' = Filings)
        filings2020 <- accumDF2 %>%
            filter(Year == 2020) %>%
            rename('month2020' = Month, 'filings2020' = Filings)
        
        filings2020Tmp <- filings2020 %>%
            bind_cols(filings2019) %>%
            mutate(accumShadow = cumsum(filings2019-filings2020),
                   Year = '2020+shadow') %>%
            select(-month2019,-filings2019) %>%
            rename(Month = month2020, Filings = filings2020)
        
        filings2020Shadow <- filings2020Tmp %>%
            mutate(filingType = 'shadow') %>%
            select(filingType, date, Month, filings = accumShadow) 
        
        filings2020Active <- filings2020Tmp %>%
            mutate(filingType = 'active') %>%
            select(filingType, date, Month, filings = accumFilings)
        
        filings2020Total <- filings2020Tmp %>%
            mutate(filingType = 'total', filings = accumFilings+accumShadow) %>%
            select(filingType, date, Month, filings) %>%
            bind_rows(filings2020Active,filings2020Shadow)
        
        # #wrangle back into main df to compare 2019, 2020, 2020+shadow, 2021
        # accumDF3 <- accumDF2 %>%
        #     mutate(shadow = 0,
        #            Year = as.character(Year)) %>%
        #     bind_rows(filings2020Shadow) %>%
        #     # left_join(shadow2020, by=(c('Year', 'Month' = 'month2020'))) %>%
        #     rowwise() %>%
        #     mutate(accumFilingsPlusShadow = sum(accumFilings,shadow,na.rm = TRUE))
        
        
        
        options(scipen = 10000)  

        ggplotAccumPending <- ggplot(accumDF2,aes(x=lubridate::month(date, label = TRUE, abbr = TRUE), y=accumFilings, color = as.factor(Year), 
                                                  group = as.factor(Year), text = paste0('At the end of ',Month,' ',Year,',',format(accumFilings, big.mark = ','),' more cases were pending than on January 1, ',Year,'. \n',
                                                                                         'During the month of ',Month,',',format(Filings-Dispositions, big.mark = ','),' cases were added to the active pending caseload.'))) +
            geom_line() + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') +
            # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
            scale_color_manual(breaks = c(2019,2020,2021), values = c('black','dodgerblue','goldenrod1')) +
            labs(x = '', y='', color = "Year") +
            theme_bw() +
            ggtitle("How does the accumulation of active pending cases in 2020 compare to 2019 and 2021?")
        
        #set scale_y_continuous scaling unit based on max/min values of accumDF2 accumFilings
        if (max(accumDF2$accumFilings > 1000) | min(accumDF2$accumFilings < -1000)){
            ggplotAccumPending <- ggplotAccumPending +
                scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4))
        }
        
        #generate plot with shadow cases on click
        if(vars$counter %% 2 == 1){
            ggplotAccumPending2 <- ggplot(filings2020Total,aes(x=lubridate::month(date, label = TRUE, abbr = TRUE), y=filings, color = as.factor(filingType), 
                                                      group = as.factor(filingType), text = paste0(filingType,' ',Month,': ',format(filings, big.mark = ',')))) +
                geom_line() + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') +
                #scale_x_date(date_breaks = '1 month', date_labels = '%B', expand = c(0,0)) +
                # scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) +
                # scale_color_manual(breaks = c('total','shadow','active'), values = c('black','dodgerblue','goldenrod1')) +
                labs(x = '', y='', color = " ") +
                theme_bw() +
                ggtitle("How did active pending and \"shadow\" cases change in 2020?")
            
            #set scale_y_continuous scaling unit based on max/min values of filings2020Total filings
            if (max(filings2020Total$filings > 1000) | min(filings2020Total$filings < -1000)){
                ggplotAccumPending2 <- ggplotAccumPending2 +
                    scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 10e-4)) 
            }# else if (max(accumDF3$accumFilingsPlusShadow < 1000)) {
            #   ggplotAccumPending <- ggplotAccumPending +
            #     scale_y_continuous(limits = c(NA,round(max(accumDF3$accumFilingsPlusShadow+100),-2))) 
            # }   
            
            ggplotly(ggplotAccumPending2,
                     tooltip = c('text')) %>%
                layout(hovermode = 'x unified') %>% 
                # style(text = paste0('test'), 
                #       traces = 1) %>%
                reverse_legend_labels()
        }
        else{
            ggplotly(ggplotAccumPending,
                     tooltip = c('text'))
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# To run this program, set working directory, ensure that proper libaries are installed, and execute all code. 

setwd('/Users/benjaminforleo/R/fall_project/campaign_contributions_repository')

library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)
library(plotly)
library(tidyquant)
library(shiny)
library(rsconnect)

# Load the data and concat into one file. 

tc1 <- fread('./data/tc1.csv')
tc2 <- fread('./data/tc2.csv')
tc3 <- fread('./data/tc3.csv')
tc4 <- fread('./data/tc4.csv')

total_contributions <- rbind(tc1,tc2, tc3, tc4)
rm(tc1, tc2, tc3, tc4)


total_contributions$TRANSACTION_DATE <- as_date(total_contributions$TRANSACTION_DATE)

chart1 <- total_contributions %>%
  group_by(CAND_NAME, CAND_PTY_AFFILIATION, TRANSACTION_PGI) %>%
  summarise(total_contributions = round(sum(TRANSACTION_AMT)/1000000, 1))

chart2 <- total_contributions %>% 
  group_by(CAND_NAME, TRANSACTION_DATE) %>% 
  summarise(CONTRIBUTIONS = sum(TRANSACTION_AMT)) %>% 
  filter(CONTRIBUTIONS >0)

# Let's create a dataframe that we will use for out plotly map
# We need to exclude DC because it is such an outlier it screws up the scales on our graph
chart3 <- total_contributions %>% 
  group_by(CAND_NAME, STATE, CAND_PTY_AFFILIATION) %>% 
  summarise(CONTRIBUTIONS = sum(TRANSACTION_AMT)) %>% 
  filter(STATE != 'DC')

# Here we read in population for each state. 
# Our ultimate  goal is to get contributions per 10,000 residents, by state
state_population <- fread('./data/population_estimate.csv')

# State population names have full state name as identifier
# We need abbreviations to join with our main dataframe
state_abreviations <- fread('./data/state_abbrevations.csv')

# Create a state population file
states <- inner_join(state_population, state_abreviations)
states <- select(states, ABBREVIATION, POPULATION)

#Join the population data to our chart 3 datafram3
chart3 <- inner_join(chart3, states, by = c('STATE' = 'ABBREVIATION'))

# Calculate contributions per 10000 residents.  
chart3$CONTR_PER_10000 <- chart3$CONTRIBUTIONS/(chart3$POPULATION/10000)


# Here, I Create a column that adds a color given party affiliation. 
# I am sure that there is an easier way to do this. Regardless, this column will come in handy later. 

color_function <- function(party){
  if(party == 'REP'){
    return('Reds')
  }else if(party == 'DEM'){
    return('Blues')
  }else if(party == 'LIB'){
    return('YlOrBr')
  }else{
    return('Greens')
  }
}


chart3$COLORS <- sapply(chart3$CAND_PTY_AFFILIATION, color_function)

# Initialize some settings for some plotly maps that will be used in shiny
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Create a vector for our search engine to use (State selector part)
state_vector <- c('USA', states$ABBREVIATION)

names <-c('Hillary Clinton', 'Bernie Sanders', 'Donald Trump', 'Ted Cruz', 'Marco Rubio', 
          'Jeb! Bush', 'Ben Carson', 'John Kasich', 'Chris Christie', 'Carly Fiorina', 'Scott Walker',
          'Rand Paul', 'Lindsey Graham', 'Martin O Malley','Gary Johnson', 'Jill Stein')


####### SHINY APP ######

ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel('Chart 1',
             titlePanel('Total Raised from Individuals: 2016 Election'), br(),
             sidebarLayout(
               
               sidebarPanel(width = 2,
                            
                            checkboxGroupInput(inputId = 'candidate',
                                               label = 'Candidates',
                                               choices = names,
                                               selected = names)),
               
               mainPanel(
                 plotOutput(outputId = 'barplot', height = '700px')
               )
             )
             
    ),
    
    
    
    tabPanel('Chart 2',
             titlePanel('Individual Campaign Contributions Over Time: 2016 Presidential Election'), br(),      
             sidebarLayout(
               
               sidebarPanel(width = 2,
                            
                            dateRangeInput(inputId = 'date_range',
                                           label = 'Select Range',
                                           start = as.Date('2015-04-01'),
                                           end = as.Date('2016-12-31')),
                            
                            sliderInput(inputId = 'ma_slider',
                                        label = 'Select Moving Average (Days)',
                                        min = 10,
                                        max = 200,
                                        step = 20,
                                        value = 30),          
                            
                            checkboxGroupInput(inputId = 'y',
                                               label = 'Candidate',
                                               choices = names,
                                               selected = c('Hillary Clinton', 'Donald Trump'))
               ),
               mainPanel(
                 plotOutput(outputId = 'lineplot', height = '700px')
               )
             )
    ),
    tabPanel('Chart 3',
             
             titlePanel('Campaign Contributions by State: Sum of Individual Contributions ($) per 10,000 Residents'), br(),
             
             sidebarLayout(
               
               sidebarPanel(width = 3,
                            
                            selectInput(inputId = 'candidate1_map',
                                        label = 'Select Candidate',
                                        choices = names,
                                        selected = 'Hillary Clinton'),
                            
                            selectInput(inputId = 'candidate2_map',
                                        label = 'Select Candidate',
                                        choices = names,
                                        selected = 'Donald Trump')),
               
               mainPanel(
                 plotlyOutput(outputId = 'plotlymap1'),
                 plotlyOutput(outputId = 'plotlymap2')
               )
             )
    ),
    
    tabPanel('ReadME', titlePanel('ReadMe'), br(), HTML("<font size='5'> Hello! My name is <a href = http://www.linkedin.com/in/benjamin-forleo >Ben Forleo.</a> This Shiny app 
                                                        visualizes campaign finance data for the 2016 presidential election and was completed as part of a school project at the University
                                                        of New Hampshire. For me, this exercise highlighted just how complicated campaign finance data really is, and I chose to only look at data 
                                                        that is reported as transaction type 15, 15C, and 15E. This data includes contributions to political committees made by individuals 
                                                        (including partnerships and LLC) who contributed in excess of $200 during the 2016 election cycle. The data can be found 
                                                        <a href = https://www.fec.gov/data/browse-data/?tab=bulk-data>here</a> on the FEC website. Enjoy!</font>"))
    )
    )
server <-function(input,output){
  
  output$barplot <- renderPlot({
    
    nudge_function <-function(input){
      if('Hillary Clinton' %in% input){
        return(10)
      }else{
        return(2)
      }
    }
    
    ggplot(filter(chart1, CAND_NAME %in% input$candidate), aes(x = reorder(CAND_NAME, total_contributions), y = total_contributions, fill = TRANSACTION_PGI)) + 
      geom_col(alpha = 0.9, width = 0.75) +
      geom_text(aes(CAND_NAME, total, label = total, fill = NULL), nudge_y = nudge_function(input$candidate), data = filter(chart1, 
                                                                                                                            CAND_NAME %in% input$candidate) %>% 
                  group_by(CAND_NAME, CAND_PTY_AFFILIATION) %>%
                  summarise(total = sum(total_contributions))) +
      coord_flip() +
      scale_y_continuous(expand = expand_scale(mult = c(.01, .2))) +
      scale_fill_manual(values = c('#FF7F0E', '#1F77B4'), breaks = c('G', 'P'), labels = c('General Election', 'Primary')) +
      ggtitle('Total Raised from Individuals: 2016 Presidential Election', 
              subtitle = 'Contributions from Individuals, Partnerships, LLCs (transaction type 15, 15E & 15C)') +
      xlab('Candidate') +
      ylab('Total Contributions (Millions of $)') +
      theme_bw() + 
      theme(legend.title = element_blank(), axis.title = element_text(size = 18), axis.text = element_text(size = 12), 
            plot.title = element_text(size = 20), plot.subtitle = element_text(size = 18), legend.text = element_text(size = 18))
    
    
  })
  
  
  output$lineplot <- renderPlot({
    ggplot(filter(chart2, CAND_NAME %in% input$y), aes(x = TRANSACTION_DATE, y = CONTRIBUTIONS, color = CAND_NAME)) + 
      geom_ma(linetype = 'solid', n = input$ma_slider) +
      geom_vline(aes(xintercept = as.Date('2016-07-27')), color = 'dodgerblue2') +
      geom_vline(aes(xintercept = as.Date('2016-07-18')), color = 'firebrick') +
      geom_vline(aes(xintercept =as.Date('2016-11-08'))) +
      ggtitle(paste(input$ma_slider, 'Day Moving Average: Sum of Individual Campaign Contributions ($)')) + 
      theme_bw() +
      xlab('Date')+
      ylab('Sum of Contributions ($)') +
      guides(color = guide_legend(title = 'Candidate', keyheight = 0.2)) + 
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = 18), 
            title = element_text(size = 18), legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18), legend.spacing = unit(5, 'line')) +
      coord_cartesian(xlim = input$date_range)
  })
  
  output$plotlymap1 <- renderPlotly({
    plot_geo(filter(chart3, CAND_NAME == input$candidate1_map), locationmode = 'USA-states') %>% 
      add_trace(z = ~CONTR_PER_10000, locations = ~STATE, color = ~CONTR_PER_10000, 
                colors = as.character(chart3[chart3$CAND_NAME == input$candidate1_map, 'COLORS'][1,])) %>% 
      layout(title = input$candidate1_map, geo = g) %>% 
      colorbar(title = 'Contributions ($) per 10,000')
  })
  
  output$plotlymap2 <- renderPlotly({
    plot_geo(filter(chart3, CAND_NAME == input$candidate2_map), locationmode = 'USA-states') %>% 
      add_trace(z = ~CONTR_PER_10000, locations = ~STATE, color = ~CONTR_PER_10000, 
                colors = as.character(chart3[chart3$CAND_NAME == input$candidate2_map, 'COLORS'][1,])) %>% 
      layout(title = input$candidate2_map, geo = g) %>% 
      colorbar(title = 'Contributions ($) per 10,000')
  })
  
}

shinyApp(ui = ui, server = server)


setwd('/Users/benjaminforleo/R/fall_project/campaign_contributions')

library(tidyverse)
library(dplyr)
library(lubridate)
library(rebus)
library(data.table)
library(lubridate)
library(tidyquant)
library(plotly)
library(shiny)
library(DT)
library(rsconnect)

# Read in the large df, read in and assign column names, remomve col name dataset
total_contributions <- fread('total_contributions.txt', sep ='|', header = FALSE)

total_contributions_cols <- read_csv('total_contributions_header.csv')
colnames(total_contributions) <- colnames(total_contributions_cols)
rm(total_contributions_cols)

head(total_contributions)


# READ IN THE COMMITTEE LINKAGE FILE 

committee_link <- fread('committee_link.txt', sep = "|", header = FALSE)
committe_link_cols <- read_csv('committee_link_header.csv')
colnames(committee_link) <- colnames(committe_link_cols)
rm(committe_link_cols)


# READ IN THE CANDIDATE MASTER FILE

candidate_master <- fread('candidate_master.txt', sep = '|', header = FALSE)
candidate_master_cols <- read_csv('candidate_master_header.csv')
colnames(candidate_master) <- colnames(candidate_master_cols)
rm(candidate_master_cols)


# FIRST JOIN: TOTAL CONRIBUTIONS AND COMMITTEE LINKAGE FILE
total_contributions <- inner_join(total_contributions, committee_link, by ='CMTE_ID')


# SECOND JOIN: TOTAL CONTRIBUTIONS AND CANDIDATE MASTER FILE
total_contributions <- inner_join(total_contributions, candidate_master, by = 'CAND_ID')
rm(candidate_master)
rm(committee_link)

#######################################


#### CLEAN THE DATE OF CONTRIBUTION COLUMN. CREATE NEW 

# Pad the date column with 0 so that we con convert to date format.
total_contributions$TRANSACTION_DT <- str_pad(total_contributions$TRANSACTION_DT, 8, side = 'left', pad ='0')

# Convert date column to date
total_contributions$TRANSACTION_DATE <- as.Date(total_contributions$TRANSACTION_DT, '%m%d%Y')

# CREATE CLEAN ZIP CODE
total_contributions$ZIP_CODE <- str_extract(total_contributions$ZIP_CODE, pattern = '[\\d].{4}')


# CREATE A COLUMN LIST TO SUBSET THE DATFRAME TO A MORE MANAGEABLE SIZE
col_list <- c('CMTE_ID', 'CAND_ID', 'CAND_NAME', 'CAND_PTY_AFFILIATION', 'CAND_ELECTION_YR.x', 'CAND_OFFICE_ST',
              'CAND_OFFICE', 'CAND_OFFICE_DISTRICT', 'CAND_ICI', 'CAND_STATUS', 'CAND_PCC', 'AMNDT_IND', 'RPT_TP', 'TRANSACTION_PGI', 'TRANSACTION_TP', 'ENTITY_TP',
              'NAME','CITY', 'STATE', 'ZIP_CODE', 'EMPLOYER', 'OCCUPATION', 'TRANSACTION_DATE', 'TRANSACTION_AMT', 'OTHER_ID', 'MEMO_CD', 'MEMO_TEXT')

total_contributions <- select(total_contributions, col_list)

# FIX A COUMN NAME
colnames(total_contributions)[5] <- 'CAND_ELECTION_YR'

rm(col_list)

# FIND THE TOP CANDIDATES IN TURMS OF FUNDRAISING TO INCLUDE IN OUR ANALYSIS
president_summary <- total_contributions %>%
  filter(CAND_OFFICE == 'P') %>% 
  group_by(CMTE_ID, CAND_ID, CAND_NAME) %>% 
  summarise(TOTAL_TRANSACTIONS = sum(TRANSACTION_AMT)) %>% 
  arrange(desc(TOTAL_TRANSACTIONS))

# CREATE A SUBSET OF TOP 10
president_summary <- president_summary[1:16, ]

#############################


# FILTER DOWN TO ONLY INCLUDE PRESIDENTIAL CANDIDATES (FUNDRAISING FOR GENERAL, PRIMARY, AND CONVENTION)
total_contributions <- filter(total_contributions, CAND_NAME %in% president_summary$CAND_NAME,
                    CAND_OFFICE == 'P', ENTITY_TP == 'IND', OTHER_ID == "", 
                    TRANSACTION_PGI %in% c('P', 'G', 'C'), TRANSACTION_TP %in% c('15', '15C', '15E'))

# LUT FOR CANDIDATES NAME 
names <-c('Hillary Clinton', 'Bernie Sanders', 'Donald Trump', 'Ted Cruz', 'Marco Rubio', 
          'Jeb! Bush', 'Ben Carson', 'John Kasich', 'Chris Christie', 'Carly Fiorina', 'Scott Walker',
          'Rand Paul', 'Lindsey Graham', 'Martin O Malley','Gary Johnson', 'Jill Stein')

president_summary$names <- names

total_contributions$CAND_NAME <- (president_summary[match(total_contributions$CAND_NAME, president_summary$CAND_NAME), 'names'])$names


#### THAT SHOULD BE IT FOR CLEANING

total_contributions <- select(total_contributions, CAND_NAME, CAND_PTY_AFFILIATION, STATE, ZIP_CODE,
                           TRANSACTION_AMT, TRANSACTION_PGI, TRANSACTION_TP, TRANSACTION_DATE)

tc1<- total_contributions[1:1000000,]
tc2 <- total_contributions[1000001:2000000,]
tc3 <- total_contributions[2000001:3000000,]
tc4 <- total_contributions[3000001:nrow(total_contributions),]

fwrite(tc1, '/Users/benjaminforleo/R/fall_project/campaign_contributions_code_repository/data/tc1.csv')
fwrite(tc2, '/Users/benjaminforleo/R/fall_project/campaign_contributions_code_repository/data/tc2.csv')
fwrite(tc3, '/Users/benjaminforleo/R/fall_project/campaign_contributions_code_repository/data/tc3.csv')
fwrite(tc4, '/Users/benjaminforleo/R/fall_project/campaign_contributions_code_repository/data/tc4.csv')

#fwrite(total_contributions, '/Users/benjaminforleo/R/fall_project/shiny_deployment_portfolio/total_contributions_final.csv')

####### SHINY PRE-PROCESSING #######

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
state_population <- read_csv('population_estimate.csv')

# State population names have full state name as identifier
# We need abbreviations to join with our main dataframe
state_abreviations <- read_csv('state_abbrevations.csv')

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
                                                        that is reported as transaction type 15, 15C, and 15E. This data include contributions to political committees made by individuals 
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


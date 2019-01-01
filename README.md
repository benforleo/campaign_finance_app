# Visualizing Campaign Finance Data for the 2016 Presidential Election

## An application of the Shiny package in R

This repository was created as part of a school project at the University of New Hampshire which asked students to create a Shiny app in R using a dataset of the students choice. I chose to work with campaign finance data for the 2016 election cycle, and I eventually settled on individual contributions to the the top 16 presidential contendors classified as transaction type 15, 15C, and 15E.

The original data file includes all contributions to federal candidates from individuals (including partnerships and LLCs) who donated in excess of $200 during the 2016 cycle. This file contains nearly 20 million rows and can be found on the FEC website [here.](https://www.fec.gov/data/browse-data/?tab=bulk-data) 

Fortunatly, we can filter this dataset down to a more manageable size (about 4 million rows) by only focusing on the top 16 presidential candidates. All of the data required to run the app can be found in the data folder within this repository. 

To run the app on your machine:
* Clone the repository to your local machine
* Change the working directory
* Run all code in election_shiny_app.R

To view the final Shiny app online, click [here.](https://benforleo.shinyapps.io/election_app/)

Enjoy!


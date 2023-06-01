# Introduction
This is the repository for the Thesis of Jan van der Doe as part of the MSc Marketing Management under the supervision of Dr. Hannes Datta.

The thesis researches the effect of processing fluency of product descriptions on the review rating, and how this effect depends on price. The processing fluency was measured by looking at the readability. The products that were looked into were wine products from gall.nl.

# Structure
????data #Raw data with the results from the pretest from Qualtrics
????gen #Generated datasets
?   ????input #Data scraped from the gall.nl
?   ????output #The clean datasets
?   ????paper #The data that was used for the thesis, such as tables
????src #Code in either R or Python
    ????analysis #Code for the analysis, such as regressions
    ????cleaning #Code used to clean the data
    ????Pretest #Code specifically used to clean and analyze the pre-test data
    ????scraping #The webscraper

# Running instructions
The library package 'beautifulsoup' was used to retrieve the data from gall.nl. The data cleaning was partially done in R and in Python. The modeling and visualization in R.

Packages needed for Python
- selenium.webdriver
- bs4
- pandas
- re
- sleep
- json
- csv
- requests
- nltk
- nltk.tokenize
- statistics

Packages needed for R
- ggplot2
- tidyverse
- car
- broom
- texreg
- writexl
- caret
- ggfortify
- lmtest
- quanteda
- reshape2
- ltm
- Hmsic
## THIS CODE TRANSFORMS .TXT DATA DOWNLOADED FROM LAB.JS & COMPILES .CSV FILES FROM OSWEB AND LAB.JS
## AG.Mitchell 10.02.21

##### PREP #####
# libraries to install
library(tidyverse)
library(jsonlite)

# paths
dataPath <- getwd()

# Read the text file from JATOS ...
read_file('jatos_results.txt') %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') %>%
  # ... parse JSON into a data.frame
  map_dfr(fromJSON, flatten=T) -> data

# Optionally save the resulting dataset
#write_csv(data, 'labjs_data_output.csv'
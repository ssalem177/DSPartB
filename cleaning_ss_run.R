# Nov 18 2025

# Author - Sami Salem

library(stringr)
library(tidyverse)

# cleaning all files in PDNC corpus

# getting the 'cleaning' function, that creates novel databases

source('cleaning_ss.R')

Novels <- read_csv('pdnc_dataset/PDNC-Novel-Index.csv')

# reading all novels and supplying novel databases to the 'cleaned_books' folder

for (i in 1:length(Novels$`Novel Title`)){
  
  # making the novel titles readable
  name <- str_replace_all(Novels$`Novel Title`[i]," ","")
  name <- str_replace_all(name,"'","")
  name <- str_replace_all(name,"-","")
  
  # running the cleaning
  cleaning(name)
}

"
Author: Razak Nart
Created on: 04/07/2019
"

"Importing each dataset from the ExcelCPDatasets"

# Get the working directory for this project
getwd()

# Required packages
library(tidyverse) # for data manipulation and visualization
library(readxl) # for loading and reading datasets from excel



# Function to combine each files by id using row bind
# Create a new variable (Code) to differentiate each obsevation on year
# Drop the id variable as it will be needed anymore
CombineFiles <- function(x) {h
  sapply(x, read_excel, simplify = FALSE) %>% bind_rows(.id = "id") %>%
    mutate(Code = sub("_.*", "", basename(id))) %>%
    select(-c(id))
}



# Loading all the Investigation files from the folder and placing them in a list
Investigationfiles <- list.files(path = "ExcelCPDatasets/", pattern = "*INVESTIGATION.xlsx", full.names = T)


# Loading all the CPCC files from the folder and placing them in a list
CPCCFiles <- list.files(path = "ExcelCPDatasets/", pattern = "*CONFERENCE.xlsx", full.names = T)


# Loading all the Child files from the folder and placing them in a list
ChildFiles <- list.files(path = "ExcelCPDatasets/", pattern = "*CHILD.xlsx", full.names = T)


# Loading all the Transfer files from the folder and placing them in a list
Transferfiles <- list.files(path = "ExcelCPDatasets/", pattern = "*TRANSFER.xlsx", full.names = T)



# Using the combine file function on all the read files from the ExcelCPDatasets
Investigation <- CombineFiles(Investigationfiles)
CPCC <- CombineFiles(CPCCFiles)
Child <- CombineFiles(ChildFiles)
Transfer<- CombineFiles(Transferfiles)




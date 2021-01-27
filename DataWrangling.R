"
Author: Razak Nart
Created on: 09/07/2019
"

# Get the working directory for this project
getwd()

source("ImportData.R")

# Required packages

library(writexl) # for saving excel file
library(plyr) #for joining
library(tidyverse) # for data manipulation and visualization
library(lintr) #for checking style of code is consistent



# Function to join two datasets using left join
# Id variable will then be removed as it will not be needed again
joinTwoDataset <- function(x, y) {
  left_join(x,y, by = c("Id", "Code")) %>%
    select(-c(Id))
}


# Function for sampling the Child dataset
# then generate a unique identifier for each child
childSampling <- function(x) {
  select(x, Code,LaCode:ScottishCandidateNumber) %>%
    mutate(UnqIdentifier = paste(LaCode,ChildId, sep = ""))
}


# Function for sampling the Investigation dataset
# Renaming the ChildRecordId to Id
# Add a new column (variable) that number the observation to show the times it appears
InvestSampling <- function(x) {
  x %>% select(Code, ChildRecordId:SubjectToInitialCPCC) %>%
    mutate(UnqCode = paste(ChildRecordId,Code, sep = "")) %>%
    group_by(UnqCode) %>%
    mutate(Investigation = row_number()) %>%
    rename(Id = ChildRecordId)
}


# Function for sampling the CPCC dataset
# selecting specific variables and renaming ChildRecordId to Id
# in the last deregistration date variable, the date 1900-01-02 was replaced with NA as the means nothing
CPCCSampling <- function(x) {
  select(x, Code, ChildRecordId,Type:PreviousChildId,ChildSexualExploitation,Trafficking,ForcedOrDangerousLabour) %>%
    rename(Id = ChildRecordId) %>%
    mutate(LastDeRegistrationDate = na_if(LastDeRegistrationDate, "1900-01-02"))
}

# Function for sampling the Transfer dataset
# selecting specific variables and renaming ChildRecordId to Id
TransferSampling <- function(x) {
  select(x, Code, ChildRecordId:ReceivingLaCode) %>%
    rename(Id = ChildRecordId)
}


# Function to combine the Investigation and the Case Conference
# Creating a new variables (EventDate and EvenType)
# to combine all the dates in one variable and state if the observation is an investigation or CC
combineInvestCPCC <- function(Invest, CPCC){
  bind_rows(Invest, CPCC) %>%
    mutate(EventDate = coalesce(DateInvestigationEnded, ConferenceDate)) %>% 
    mutate(EventType = ifelse(is.na(Investigation), "Conference", "Investigation"))
}


# Using the sampling function on Child, CPCC, investigation and transfer datasets
SampleChild <- childSampling(Child)
SampleCPCC <- CPCCSampling(CPCC)
SampleInvest <- InvestSampling(Investigation)
SampleTransfer <- TransferSampling(Transfer)

# Using a function to combine the investigation and CPCC for the datasets
InvestCPCC <- combineInvestCPCC(SampleInvest, SampleCPCC)

# Joining the Child data to the combine Investigation and CP Case Conference
CompleteDataset <- joinTwoDataset(SampleChild, InvestCPCC)

# Joining the child and transfer datasets
ChildTransfer <- joinTwoDataset(SampleTransfer, SampleChild)


# Checking the tranfer dataset with the complete dataset
# Using the complete dataset, it was grouped by the unique identifier and filtered with the type "04" to get only transfer records
# For this checks, only specific variables were needed from the complete dataset for the join
# a mutating join (right_join) was used to join the child transfer dataset to the complete dataset
TransferChecks <- CompleteDataset %>%
  group_by(UnqIdentifier) %>%
  filter(Type == "04") %>% 
  select (UnqIdentifier, OriginatingLaCode, PreviousChildId) %>% 
  right_join(ChildTransfer, by = c("UnqIdentifier"))


# Complete tranfer dataset 
# Using the complete dataset, it was grouped by the unique identifier
# Creating a new unique ID by adding the originating LaCode with previous child ID
# this was achieved using if_else statement to check if the type is 04 (Transfer) if not NA is used.
# Filtered with the type "04" to get only transfer records 
# and originating LaCodes with 900 were filtered out because this shows the transfers were outside Scotland
# Selected only the two unique identifiers for the join
# Using a mutating join (right_join), the new dataset was joined to the complete dataset
# A new variable was created to hold the new unique identifier
# The UnqPreviousChildId and UnqIdentifier were remove dfrom the final dataset
CompleteTransferDataset <-   CompleteDataset %>%
  group_by(UnqIdentifier) %>%
  mutate(UnqPreviousChildId = ifelse(Type == "04", paste(OriginatingLaCode, PreviousChildId, sep = ""), NA)) %>%
  filter(Type == "04" & OriginatingLaCode != "900") %>% select(UnqPreviousChildId, UnqIdentifier) %>%
  right_join(CompleteDataset, c("UnqPreviousChildId" = "UnqIdentifier")) %>%
  mutate(NewIdentifier = coalesce(UnqIdentifier, UnqPreviousChildId)) %>% ungroup() %>%
  select(NewIdentifier, everything(), -c(UnqPreviousChildId, UnqIdentifier))



# Save the joined dataset
write_xlsx(CompleteDataset, "ExcelCPDatasets/CompleteDataset.xlsx") # Complete Datasets
write_xlsx(TransferChecks, "ExcelCPDatasets/TransferChecks.xlsx") # Transfer check with complete dataset
write_xlsx(CompleteTransferDataset, "ExcelCPDatasets/CompleteTransferDataset.xlsx") # Complete transfer datasets


#start of TA edits Jan 2021------------------------------------------------------------

#This section will do some additional checks for potential data quality issues

#find any children with more than one SCN first 

SCNConflicts <- CompleteDataset %>%
arrange(UnqIdentifier, ScottishCandidateNumber) %>%
mutate(LagSCN = lag(ScottishCandidateNumber)) %>%
mutate(LagUID = lag(UnqIdentifier)) %>%
mutate(SCN_flag = ifelse((LagSCN != ScottishCandidateNumber) & (LagUID == UnqIdentifier), "1", "0")) %>%
filter(SCN_flag == "1") %>%
group_by(UnqIdentifier) %>%
summarise(NumberOfSCNs = n_distinct(ScottishCandidateNumber)) %>%
select(UnqIdentifier, NumberOfSCNs)
SCNConflicts$NumberOfSCNs <- SCNConflicts$NumberOfSCNs + 1 

#This code not needed except for counting how many people have 2 SCNs, 3 SCNs etc.:
#ungroup() %>%
#count(SCNConflicts$"No_SCNs") 

#Join the No_SCNs variable onto the main dataset, this way every row containing a UnqIdentifier with more 
#than one SCN will get the count of SCNs for that ID added
CompleteDatasetWithSCNCount <-
left_join(CompleteDataset, SCNConflicts, by = "UnqIdentifier", suffix = c("", ".y")) %>%
select(UnqIdentifier, Code, LaCode, DateOfBirth, Gender, ScottishCandidateNumber, NumberOfSCNs, EventDate, EventType,
       Type, DeRegistered, DeRegisteredReason, Registered, ConferenceDate, LastDeRegistrationDate, 
       PreviouslyRegistered, OriginatingLaCode, PreviousChildId, SubjectToInitialCPCC, 
       DateInvestigationEnded) 

#Find any children who have changed DOB over time
  
DOBConflicts <- CompleteDataset %>%
  arrange(UnqIdentifier, DateOfBirth) %>%
  mutate(LagDOB = lag(DateOfBirth)) %>%
  mutate(LagUID = lag(UnqIdentifier)) %>%
  mutate(DOB_flag = ifelse((LagDOB != DateOfBirth) & (LagUID == UnqIdentifier), "1", "0")) %>%
  filter(DOB_flag == "1") %>%
  group_by(UnqIdentifier) %>%
  summarise(NumberOfDOBs = n_distinct(DateOfBirth)) %>%
  select(UnqIdentifier, NumberOfDOBs)
  DOBConflicts$NumberOfDOBs <- DOBConflicts$NumberOfDOBs + 1  #add 1 because if a child has 2 DOBs, they will only appear in this dataset once and so on

#This code was used at the end of the above code block to count the number of people who have 2 DOBs, 3 DOBs and so on. For information only
  ungroup(DOBConflicts) %>%
  count(DOBConflicts$"NumberOfDOBs")
  
#same as process for SCNs above, add the DOB count variable to the data

CompleteDatasetWithAllCounts <- 
  left_join(CompleteDatasetWithSCNCount, DOBConflicts, by = "UnqIdentifier") 

#Create a new version of the CompleteDataset with a variable to show alternate SCNs where a person has more than one.
#Currently this just takes the lag value as the value for the alternate field, so the alternate value will never be
#the most recent value, which is the one we will retain in the original field. This could possibly be improved. 

AlternateFields <- CompleteDatasetWithAllCounts %>%
  arrange(UnqIdentifier, EventDate) %>%
  mutate(LagSCN = lag(ScottishCandidateNumber)) %>%
  mutate(LagDOB = lag(DateOfBirth)) %>%
  mutate(LagUID = lag(UnqIdentifier)) %>%
  mutate(Alt_SCN = ifelse((NumberOfSCNs == 2 | NumberOfSCNs ==3), (Alt_SCN = LagSCN), NA)) %>%
  mutate(Alt_DOB = ifelse((NumberOfDOBs == 2 | NumberOfDOBs == 3), (Alt_DOB = LagDOB), NA)) %>%
  select -c(LagSCN, LagDOB, LagUID)
  
#Create a variable to show whether a child has had an investigation and retain the date the investigation ended

HasInvestigation <- filter(AlternateFields, EventType == 'Investigation') %>%
  mutate(InvestigationStatus = ifelse(!is.na(DateInvestigationEnded), (InvestigationStatus=as.Date(DateInvestigationEnded)), NA)) %>%
  mutate(HasOpenInvestigation = ifelse(is.na(DateInvestigationEnded), (HasOpenInvestigation=1), NA)) %>%
  group_by(UnqIdentifier) %>%
  mutate(CurrentlyOpenInvestigation = coalesce(HasOpenInvestigation))

HasInvestigation$InvestigationStatus <- as.Date(HasInvestigation$InvestigationStatus, origin="1970-01-01") 

 


#keep only the most recent row showing the most recent event for each person
arrange(UnqIdentifier, rev(EventDate))
CompleteDatasetWithSCNCount <- CompleteDatasetWithSCNCount[!duplicated(CompleteDatasetWithSCNCount$UnqIdentifier),]
  
  

  
  








Working <- unique(CompleteDataset) 
  


  
#testing dataset  
a <- c(1, 1, 2, 2, 3, NA, 5)
is.na(a)
b <- c(1, 1, 2, 55, 3, 66, 77)
df <- data.frame(a, b) %>%
mutate(LagA = lag(a)) %>%
mutate(LagB = lag(b)) %>%
group_by(a) %>%
mutate(SCN_flag = ifelse((LagA == a) & (LagB != b), "1", "0"))

!is.na(df$a)
df <- ifelse(is.na(df$a), ) 




 







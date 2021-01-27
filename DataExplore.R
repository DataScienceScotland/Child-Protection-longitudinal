"
Author: Razak Nart
Created on: 10/07/2019
"

# Get the working directory for this project
getwd()

# Required packages
library(tidyverse) # for data manipulation and visualization
library(readxl) # for loading and reading datasets from excel
library(lubridate) # for working with date and time
library(formattable) # for formatting tables


# Load and read the completeed dataset for Child, Conference and Investigation
CPDataset <- read_excel("ExcelCPDatasets/CompleteDataset.xlsx")

Transfer <- read_excel("ExcelCPDatasets/TransferChecks.xlsx")

# Complete with a new variables that groups the child's activities
# This was done using SAS
Invest_CPCC_Group <- read_excel("ExcelCPDatasets/Invest_CC_Group_Fixed.xlsx")

# The grouped dataset is then filtered to get only Case Conference
CPCC_Group <- Invest_CPCC_Group %>% filter(EventType == "Conference") %>%
  select(Code:Conf_group)


# Function for calculating date difference mainly on month
# using two columns
month_diff <- function(end_date, start_date) {
  edate <- ymd(end_date)
  sdate <- ymd(start_date)
  12 * (year(edate) - year(sdate)) + (month(edate) - month(sdate))
}


# List of local authority names
LaNames <- c("Aberdeen City", "Aberdeenshire","Angus", "Argyll & Bute", "Clackmannanshire", "Dumfries & Galloway", "Dundee City",
             "East Ayrshire", "East Dunbartonshire", "East Lothian", "East Renfrewshire", "City of Edinburgh", "Na h-Eileanan Siar",
             "Falkirk", "Fife", "Glasgow City", "Highland", "Inverclyde", "Midlothian", "Moray", "North Ayrshire", "North Lanarkshire",
             "Orkney Islands", "Perth & Kinross", "Renfrewshire", "Scottish Borders", "Shetland Islands", "South Ayrshire",
             "South Lanarkshire", "Stirling", "West Dunbartonshire", "West Lothian")


# List of local authority codes 
LaCodeLevels <- c(100,110,120,130,150,170,180,190,200,210,220,230,235,240,250,260,270,280,290,300,310,320,330,340,350,355,360,370,
                 380,390,395,400)


# List of year
CPYears <- c("2013", "2014", "2015", "2016", "2017", "2018")


# List of year code
CPYearLevels <- c("CP1213", "CP1314", "CP1415", "CP1516", "CP1617", "CP1718")




############## INITIAL ANALYSIS #################

#Chart for the number of child protection process recorded 

# Using the imported complete dataset, the data was grouped by years(Code)
# The summarise fumnction was used to create new variable (Count) to count all the data for the year
# The mutate function was used to change the Code to years (e.g CP1213 to 2013)
# The year 2018 was filtered out due to lack of data
# Finally a ggplot was used to plot the chart (line and point geoms)

CPDataset %>%
  group_by(Code) %>% #code = year
  summarise(Count = n()) %>%
  mutate(CPYear = factor(Code, levels = CPYearLevels, labels = CPYears )) %>%
  filter(CPYear != "2018") %>%
  ggplot(aes(x = CPYear, y = Count, group=1)) + geom_line(size = 1.5, colour="#2e9ec1") + 
  geom_point(shape = 19, size = 3, colour="#0f6883") +
  ggtitle("The Number of Child Protection Investigations and Case Conferences Recorded, 2013-2017") +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x="Year", y="NUmber of Records") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) 





# Using the imported complete dataset, the data was grouped by years(Code) and LaCode
# The summarise fumnction was used to create new variable (Count) to count all the data for the year
# The spread function was used to reshape the dataframe to make the years(Code) variable
# and the Count the values
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# The select function was used to select the needed variables
# The NA values were replaced with 0
# New variables were added, one to sum the row and the other to sum the column 
NumCPProcess <- CPDataset %>%
  group_by(Code, LaCode) %>% #code = year
  summarise(Count = n()) %>%
  spread(key = Code, value = Count) %>% #rearranges the table, turns all the values of code into their own variables.
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>% #changes lacode to LA names
  select(LaName,CP1213:CP1718, -LaCode) %>% 
  replace(is.na(.), 0) %>% #NA values replaced with 0
  mutate(Total = rowSums(.[2:7])) %>%
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) #add column totals?
  
# Using the formattable function to make the dataframe look more nicer  
formattable(NumCPProcess,align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'CP1718' = formatter("span", style = x ~ style("font-weight" = ifelse(x == 0, "bold", NA))),
              area(col = 8) ~ color_tile("lightblue", "lightpink"),
              area(row = 33) ~ color_tile("lightblue", "lightpink"),
              'LaName' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold"))
            ))



# Chart for number of Children on the Child Protection Register

# A variable containing a vector of the values from the published figures 
PCount <- c(2645, 2877, 2741, 2715, 2600, NA)

# A simple dataframe containing all the end dates of the CP returns
RDate <- tibble(
  CP1213 = "2013-07-31", 
  CP1314 = "2014-07-31", 
  CP1415 = "2015-07-31", 
  CP1516 = "2016-07-31",
  CP1617 = "2017-07-31",
  CP1718 = "2018-07-31"
  )

# The output for the loop. The vector function was used for efficiency
output <- vector("list", ncol(RDate))

# The for loop statment
# The seq_along function determine what to loop over, in this case was the 
# dataframe containing the end dates of the CP ruturns
# Using the CPCC_Group dataset, the data was grouped by UnqIdentifier
# For each loop, the dataset was filtered to get CPCC that are happens before the end date
# After, the maximum remaining grouped CPCC are kept
# The last CPCC are checked, any  deregistered CPCC is removed
# The data was ungrouped, removed any duplicates before counting
for (i in seq_along(RDate)) {             
  output[[i]] <- CPCC_Group %>%
    group_by(UnqIdentifier) %>% 
    filter(ConferenceDate < RDate[[i]]) %>% 
    filter(Conf_group == max(Conf_group)) %>%
    filter(any(Registered == "1") & last(DeRegistered %in% c("0", NA))) %>%
    ungroup()%>% distinct(UnqIdentifier) %>% count()
}

# The converted the outputs to a dataframe as they were in a list format
# The gather function was used to reshape the dataframe in two variable
# The mutate function was used to add a new variable(Previous) 
# the IDs were chnaged to years (e.g n to 2013)
output <- data.frame(output) %>% 
  gather(key = "ID", value = "Current") %>% mutate(Previous = PCount) %>% 
  mutate(CPYear = factor(ID, levels = c("n","n.1", "n.2", "n.3", "n.4", "n.5"), 
                         labels = CPYears )) %>% select(CPYear,Previous, Current)

# Uisng the output dataset
# The abs function was used to get an absolute value of difference
# The NA values were replaced with *
# The Previous and Current Variables were renamed to add Number to them
# CPYear was also renamed 
NumCPRegistered <- output %>%
  mutate(Difference = abs(Current - Previous)) %>%
           replace(is.na(.), "*") %>%
           mutate(Current = replace(Current, CPYear == "2017/18", "*"))%>%
  rename_at(vars(-c("CPYear",Difference)),function(x) paste0(x," Number")) %>%
  rename("Child Protection Year" = CPYear)

  
# Using the formattable function to make the dataframe look more nicer
formattable(NumCPRegistered,align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Child Protection Year' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Current Number' = color_tile("transparent", "#8cfab6"),
              'Previous Number' = color_tile("transparent", "#8cfab6"), 
              'Difference' = color_tile("transparent", "#ff9680")
            ))





############## INVESTIGATION #################

# ~~ Identifying if a child has an intial/pre-birth case conference, but not had an investigation ~~

# Errors, some of results will be misleading as data from 2011/12 were not use during the joining. 
# which means some children will have CC but not investigation.
# The approach used for the grouping seems imprecise. 

# Using the Invest_CPCC_Group dataset, it was grouped by LA Code, Unique Identifier and Investigation & CC group variables
# During the grouping there few errors, so those were filtered out
# CPCC with the type review(03) and transfer_in (04) or
# investigations that were not recommended for CPCC were filtered out
# Summarise and if else functions were used to find the event types with investigation
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# Finally a ggplot was used to plot the chart (a bar geoms)
Invest_CPCC_Group %>%
  group_by(LaCode, UnqIdentifier, Invest_conf_group) %>% 
  filter(all(Invest_conf_group_Error != 1)) %>% 
  filter(Type %in% c("01", "02")  | SubjectToInitialCPCC == "01") %>%
  summarise(Outcome = ifelse(any(EventType == "Investigation"), "Yes", "No")) %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  mutate(LaName = fct_rev(LaName)) %>%
  ggplot( mapping =  aes(LaName, fill = Outcome)) +
  geom_bar(position = "fill") + coord_flip() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=c("#f03e35", "#28a0e3")) +
  ggtitle("Identifying Children that had an Investigation before Initial/Pre-birth Case Conference") +
  labs(x="Local Authorities", y="Percentage") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())
  







# ~~ Identify if the end date of the investigation is after the intial/pre-birth case conference ~~

# Using the Invest_CPCC_Group dataset, it was grouped by LA Code, Unique Identifier and Investigation & CC group variables
# During the grouping there few errors, so those were filtered out
# CPCC with the type review(03) and transfer_in (04) or
# investigations that were not recommended for CPCC were filtered out

# Also, children that had  Investigation and Conference as their only event type were filtered out  
# In the summarise function, the Datadiff variable holds the difference of the Investigation and CPCC
# the if else statements checks the difference, if its 0 the date is the same, if greater than 0 the investigation end date is before CPCC
# else the end date is after the CPCC
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# Finally a ggplot was used to plot the chart (a bar geoms)
Invest_CPCC_Group %>%
  group_by(LaCode, UnqIdentifier, Invest_conf_group) %>% 
  filter(all(Invest_conf_group_Error != 1)) %>% 
  filter(Type %in% c("01", "02")  | SubjectToInitialCPCC == "01") %>%
  filter(any(EventType == "Investigation") & any(EventType == "Conference")) %>%
  summarise(DateDiff =  last(EventDate) - first(EventDate),
            Outcome = ifelse(DateDiff == 0, "Sameday", ifelse(DateDiff > 0, "Yes", "No"))) %>% 
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  mutate(LaName = fct_rev(LaName)) %>%
  ggplot( mapping =  aes(LaName, fill = Outcome)) +
  geom_bar(position = "fill") + coord_flip() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=c("#136593","#56B4E9")) +
  ggtitle("Identifying if the Investigation End Date is after Initial/Pre-birth Case Conference") +
  labs(x="Local Authorities", y="Percentage") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())








# ~~ Investigate the recommendation for case conference variable – does this always result in an intial/pre-birth conference? ~~


# Using the Invest_CPCC_Group dataset, it was grouped by LA Code, Unique Identifier and Investigation & CC group variables
# During the grouping there few errors, so those were filtered out
# CPCC with the type review(03) and transfer_in (04) or
# investigations that were not recommended for CPCC were filtered out
# Also, children that had  Investigation and Conference as their only event type were filtered out
# Filtered the event type to take out children that has only conference as their event type
# Using the summarise with a condition to identify if all recommedered children go to an intial/pre-birth CC
# The dataset was then group by LaCode and Outcome to count outcome for each local authority
# After, the dataset was ungroup to reshape it to make the values(Yes and No) of the outcome variables(column)
# In the mutate function, row sum was used to calculate the total. Before calculating the percentage of the Yes and No for each LA
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# The select function was used to select the needed variables before renaming the variables
# A percentage sign was added to the all variables excluding LA
RecommenderedCPCC <- Invest_CPCC_Group %>%
  group_by(LaCode, UnqIdentifier, Invest_conf_group) %>% 
  filter(all(Invest_conf_group_Error != 1)) %>%
  filter(Type %in% c("01", "02") | SubjectToInitialCPCC == "01") %>% 
  filter(any(EventType == "Investigation")) %>% 
  summarise(Outcome = c("No", "Yes")[all(c("Investigation", "Conference") %in% EventType) + 1]) %>%
  group_by(LaCode, Outcome) %>%
  summarise(Count = n()) %>% ungroup() %>%
  spread(key = Outcome, value = Count) %>%
  mutate(Total = rowSums(.[2:3]), 
                         No= round(100*No/Total), 
                         Yes= round(100*Yes/Total)) %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  select(LaName, Yes, No) %>% rename("Local Authorities" = LaName, "Leading to Case Conference" = Yes,
                                     "Not leading to Case Conference" = No) %>%
  rename_at(vars(-"Local Authorities"),function(x) paste0(x," %"))

# It was not possible to export the formattable table because of a missing library
# So to solve this problem, the dataset was divided into two
# One from Aberdeen City to Glasgow City and the other Highland to West Lothian
RecCPCC1 <- slice(RecommenderedCPCC, 1:16)
RecCPCC2 <- slice(RecommenderedCPCC, 17:32)

# Using the formattable function to make the dataframe look more nicer
formattable(RecCPCC1, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Local Authorities' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Leading to Case Conference %' = color_tile("transparent", "#71CA97"),
              'Not leading to Case Conference %' = color_tile("transparent", "#ff7f7f")
            ))

# Using the formattable function to make the dataframe look more nicer
formattable(RecCPCC2, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Local Authorities' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Leading to Case Conference %' = color_tile("transparent", "#71CA97"),
              'Not leading to Case Conference %' = color_tile("transparent", "#ff7f7f")
            ))



# Same code as above but this dataframe contains the four LA with the most errors 
RecommenderedOutliers <- Invest_CPCC_Group %>%
  group_by(LaCode, UnqIdentifier, Invest_conf_group) %>% 
  filter(all(Invest_conf_group_Error != 1)) %>%
  filter(Type %in% c("01", "02") | SubjectToInitialCPCC == "01") %>% 
  filter(any(EventType == "Investigation")) %>% 
  summarise(Outcome = c("No", "Yes")[all(c("Investigation", "Conference") %in% EventType) + 1]) %>%
    filter(LaCode %in% c("130", "150", "235", "380")) %>%
    mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  group_by(LaName, Outcome) %>%
  summarise(Count = n()) %>% ungroup() %>%
    spread(key = Outcome, value = Count) %>% select(LaName, Yes, No) %>%
  rename("Local Authorities" = LaName, "Leading to Case Conference" = Yes,
         "Not leading to Case Conference" = No)
  
# Using the formattable function to make the dataframe look more nicer
formattable(RecommenderedOutliers, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Local Authorities' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold"))
            ))


# This shows the list of IDs and LA that has a duplicate end of investigation date
# The count was filtered to show counts greater than 1
View (
  Invest_CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, DateInvestigationEnded) %>% summarise(Count = n()) %>%
    na.omit() %>% filter(Count > 1) %>% ungroup() %>%
    mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
    select(LaName,UnqIdentifier, Count)
)

# This shows the list of IDs that can be used to check errors for LA with the most count
View (
  Invest_CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, Invest_conf_group) %>% 
    filter(all(Invest_conf_group_Error != 1)) %>%
    filter(Type %in% c("01", "02") | SubjectToInitialCPCC == "01") %>% 
    filter(any(EventType == "Investigation")) %>% 
    summarise(Outcome = c("No", "Yes")[all(c("Investigation", "Conference") %in% EventType) + 1]) %>%
    filter(LaCode %in% c("130", "150", "235", "380")) %>% ungroup() %>%
    mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
    select(LaName,UnqIdentifier, Outcome) %>% filter(Outcome == "No")
)






############## CAES CONFERENCE #################

# ~~ Pre-birth – check if the child is not born, the first case conference is pre-birth ~~


# Using the CPCC_Group dataset, it was grouped by Unique Identifier and date of birth
# In the slice function, which function was used to filter observations that matches the logical statement
# The dataset was then filtered by the types. 
# Because we wented to count the years(Code), the dataset was grouped into years(Code) before counting

#Correct: The was not born before the first case conference and had pre-birth (01) as type
pb_d1 <- CPCC_Group %>%
  group_by(UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate < DateOfBirth)) %>%
  filter(Type == "01") %>% group_by(Code) %>% summarise(Correct_Count = n())


#Error: The child was born before first case conference but type is Pre-birth (01)
pb_d2 <- CPCC_Group %>%
  group_by(UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate > DateOfBirth)) %>%
  filter(Type == "01") %>% group_by(Code) %>% summarise(Error_Count = n())


# The datasets containing the count for both correct and error checks are join together
# Using a left join both dataset was join by the years(Code)
# Excluding the year(Code) variable, a percentage value was calculated for the remaining variables
# The mutate function was used to change the Code to years (e.g CP1213 to 2013)
# Each variable were renamed and a percentage sign was added to the all variables excluding year
PreBirthCPCC <- pb_d1 %>% left_join(pb_d2, by = "Code") %>% 
  replace(is.na(.), 0) %>%
  mutate_at(vars(-Code), function(x) round(x/rowSums(.[2:3]) * 100))  %>%
  mutate(CPYear = factor(Code, levels = CPYearLevels, labels = CPYears )) %>%
  select(CPYear, Correct_Count, Error_Count) %>%
  rename("Child Protection Year" = CPYear,"Pre-birth as their type" = Correct_Count, "Incorrect use of pre-birth" = Error_Count) %>%
  rename_at(vars(-"Child Protection Year"),function(x) paste0(x," %"))

# Using the formattable function to make the dataframe look more nicer
formattable(PreBirthCPCC, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Child Protection Year' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Pre-birth as their type %' = color_tile("transparent", "#71CA97"),
              'Incorrect use of pre-birth %' = color_tile("transparent", "#ff7f7f")
            ))




# Same as above but in this case we are counting by LA, 
# the dataset was grouped into years(Code) before counting

#Correct: The was not born before the first case conference and had pre-birth (01) as type
pb_LA1 <- CPCC_Group %>%
  group_by(UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate < DateOfBirth)) %>%
  filter(Type == "01") %>% group_by(LaCode) %>% summarise(Correct_Count = n())


#Error: The child was born before first case conference but type is Pre-birth (01)
pb_LA2 <- CPCC_Group %>%
  group_by(UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate > DateOfBirth)) %>%
  filter(Type == "01") %>% group_by(LaCode) %>% summarise(Error_Count = n())

PreBirth_LA <- pb_LA1 %>% left_join(pb_LA2, by = "LaCode") %>% 
  replace(is.na(.), 0) %>%
  mutate_at(vars(-LaCode), function(x) round(x/rowSums(.[2:3]) * 100)) %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  select(LaName, Correct_Count, Error_Count) %>%
  rename("Local Authorities" = LaName,"Pre-birth as their type" = Correct_Count, 
         "Incorrect use of pre-birth" = Error_Count) %>%
  rename_at(vars(-"Local Authorities"),function(x) paste0(x," %"))

# Using the formattable function to make the dataframe look more nicer
formattable(PreBirth_LA, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Local Authorities' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Pre-birth as their type %' = color_tile("transparent", "#71CA97"),
              'Incorrect use of pre-birth %' = color_tile("transparent", "#ff7f7f")
            ))



# Using similar code as above but checking if the DOB is 9 months more then CPCC date
View(
  CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, DateOfBirth) %>%
    slice(which(EventDate < DateOfBirth)) %>%
    filter(Type == "01") %>%
    mutate(MonthDiff = month_diff(DateOfBirth, EventDate)) %>%
    filter(MonthDiff >= 9)
)

# The count of the result in Code(years) and LaCode
PreBirthCPCC2 <- CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, DateOfBirth) %>%
    slice(which(EventDate < DateOfBirth)) %>%
    filter(Type == "01") %>%
    mutate(MonthDiff = month_diff(DateOfBirth, EventDate)) %>%
    filter(MonthDiff >= 9) %>% ungroup() %>% group_by(LaCode) %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  select(LaName,MonthDiff)

# Using the formattable function to make the dataframe look more nicer
formattable(PreBirthCPCC2, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'LaName' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold"))
            ))






# ~~ Initial – check if the child has been born, the first case conference in initial. ~~


# Using the CPCC_Group dataset, it was grouped by Unique Identifier and date of birth
# In the slice function, which function was used to filter observations that matches the logical statement
# The dataset was then filtered by the types. 
# Because we wented to count the years(Code), the dataset was grouped into years(Code) before counting

# Correct: The child was born before the first case conference, the type is initial
i_d1 <- CPCC_Group %>%
  group_by(UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate > DateOfBirth)) %>%
  filter(Type == "02") %>% group_by(Code) %>% summarise(Correct_Count = n())


# Error: The child was not born before the first case conference but the type is initial
i_d2 <- CPCC_Group %>%
  group_by(Code, UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate < DateOfBirth)) %>%
  filter(Type == "02") %>% group_by(Code) %>% summarise(Error_Count = n())


# The datasets containing the count for both correct and error checks are join together
# Using a left join both dataset was join by the years(Code)
# Excluding the year(Code) variable, a percentage value was calculated for the remaining variables
# The mutate function was used to change the Code to years (e.g CP1213 to 2013)
# Each variable were renamed and a percentage sign was added to the all variables excluding year
InitialCPCC <- i_d1 %>% left_join(i_d2, by = "Code") %>% 
  replace(is.na(.), 0) %>%
  mutate_at(vars(-Code), function(x) round(x/rowSums(.[2:3]) * 100)) %>%
  mutate(CPYear = factor(Code, levels = CPYearLevels, labels = CPYears )) %>%
  select(CPYear, Correct_Count, Error_Count) %>%
  rename("Child Protection Year" = CPYear,"Initial as their type" = Correct_Count, 
         "Incorrect use of initial" = Error_Count) %>%
  rename_at(vars(-"Child Protection Year"),function(x) paste0(x," %"))

# Using the formattable function to make the dataframe look more nicer
formattable(InitialCPCC, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Child Protection Year' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Initial as their type %' = color_tile("transparent", "#71CA97"),
              'Incorrect use of initial %' = color_tile("transparent", "#ff7f7f")
            ))



# Same as above but in this case we are counting by LA, 
# the dataset was grouped into years(Code) before counting

#Correct: The was not born before the first case conference and had pre-birth (01) as type
i_LA1 <- CPCC_Group %>%
  group_by(UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate > DateOfBirth)) %>%
  filter(Type == "02") %>% group_by(LaCode) %>% summarise(Correct_Count = n())


#Error: The child was born before first case conference but type is Pre-birth (01)
i_LA2 <- CPCC_Group %>%
  group_by(UnqIdentifier, DateOfBirth) %>%
  slice(which(EventDate < DateOfBirth)) %>%
  filter(Type == "02") %>% group_by(LaCode) %>% summarise(Error_Count = n())

InitialCPCC_LA <- i_LA1 %>% left_join(i_LA2, by = "LaCode") %>% 
  replace(is.na(.), 0) %>%
  mutate_at(vars(-LaCode), function(x) round(x/rowSums(.[2:3]) * 100)) %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  select(LaName, Correct_Count, Error_Count) %>%
  rename("Local Authorities" = LaName,"Initial as their type" = Correct_Count, 
         "Incorrect use of initial" = Error_Count) %>%
  rename_at(vars(-"Local Authorities"),function(x) paste0(x," %"))

# Using the formattable function to make the dataframe look more nicer
formattable(InitialCPCC_LA, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Local Authorities' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Initial as their type %' = color_tile("transparent", "#71CA97"),
              'Incorrect use of initial %' = color_tile("transparent", "#ff7f7f")
            ))



# ~~ Check the first case conference is not initial/pre-birth. ~~

# Error, some of the data will be lost as some first CPCC was in 2011/12
# Another error, childern that were tranfered will have tranfered (04) or review (03) as their first CPCC
# To fix that error, I have to filter by Type (01 and 02)
# Also, the child must be recommended (01) before pre-birth/ initial CPCC type

# Using the imported complete dataset, the data was grouped by LA and Unique Identifier
# The dataset was arrange in a ascending oder starting from the LaCode
# The event type was filtered to get only CPCC. types with transfer-in (04) were filtered out
# The event date was filtered to show which date comes first for each child events
# The dataset was then filtered to show only type with review(03)
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# To count the total of intial/prebirth type for each LA, the dataset was grouped by LaName
# before counting
First_CPCC_InitialPrebirth <- CPDataset %>%
  group_by(LaCode, UnqIdentifier) %>%
  arrange(LaCode, UnqIdentifier, EventDate) %>%
  filter(EventType == "Conference") %>%
  filter(Type != "04") %>%  #filters out transfer in
  slice(which.min(EventDate)) %>% #selects earliest event date
  filter(Type != "03") %>% #filters out review
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  group_by(LaName) %>% summarise(Initial_Prebirth_Count = n())


# Using the imported complete dataset, the data was grouped by LA and Unique Identifier
# The event date was arrange in a ascending order
# The event type was filtered to get only CPCC. types with transfer-in (04) were filtered out
# The event date was filtered to show which date comes first for each child events
# The dataset was then filtered to show only type with review(03)
# The conference dates were filtered to show only above 01/02/2013 to reduce the number of number of error 
# due to lack of data from 2012
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# To count the number of data by LA, the dataset was grouped by LaName before counting
# The dataset joined to the First_CPCC_InitialPrebirth dataset to get the total count
# All NA values were replaced with 0
# To calculate the rate, each count for the review was divided by the totai count of initial/prebirth type
First_CPCC_Review <- CPDataset %>%
    group_by(LaCode, UnqIdentifier) %>%
    arrange(EventDate) %>%
    filter(EventType == "Conference") %>%
    filter(Type != "04") %>%
    slice(which.min(EventDate)) %>% 
    filter(Type == "03") %>% 
    filter(ConferenceDate > "2013-02-01") %>%
    mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
    group_by(LaName) %>% summarise(Count = n()) %>%
  left_join(First_CPCC_InitialPrebirth, by = "LaName") %>%
  replace(is.na(.), 0) %>% 
  mutate(Per = round(Count/Initial_Prebirth_Count * 100, 1)) %>%
  mutate(LaName = fct_rev(LaName))

#A bar chart showing first case conference as review in rate 
ggplot(First_CPCC_Review, mapping =  aes(LaName, y=Per)) +
  geom_bar(stat='identity', fill = "#34a8f9") + coord_flip() +
  geom_text(aes(x = LaName, y = Per, label = paste0(Per, "%"), hjust = 0)) +
  ggtitle("First Case Conference is Review") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Local Authorities", y="Percentage") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())
  


# The old chart 
# Bar chart plot showing first case conference as review

# Using the imported complete dataset, the data was grouped by LA and Unique Identifier
# The event date was arrange in a ascending order
# The event type was filtered to get only CPCC. types with transfer-in (04) were filtered out
# The event date was filtered to show which date comes first for each child events
# The dataset was then filtered to show only type with review(03)
# The conference dates were filtered to show only above 01/02/2013 to reduce the number of number of error 
# due to lack of data from 2012
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# The mutate function was used to change the Code to years (e.g CP1213 to 2013)
# Finally a ggplot was used to plot the chart (a bar geoms)
CPDataset %>%
  group_by(LaCode, UnqIdentifier) %>%
  arrange(EventDate) %>%
  filter(EventType == "Conference") %>%
  filter(Type != "04") %>%
  slice(which.min(EventDate)) %>% 
  filter(Type == "03") %>% 
  filter(ConferenceDate > "2013-02-01") %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  mutate(CPYear = factor(Code, levels = CPYearLevels, labels = CPYears)) %>%
  mutate(LaName = fct_rev(LaName)) %>%
ggplot( mapping =  aes(LaName, fill = CPYear)) +
  geom_bar(position = "fill") + coord_flip() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=c("#1cbfd3","#d3311d","#f98534","#8c1cd3","#63d31c","#e734f9")) +
  ggtitle("First Case Conference is Review") +
  labs(x="Local Authorities", y="Percentage") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())








# ~~ Check the child has been registered at an initial/pre-birth case conference if they have a review case conference. ~~

# Error, some of the data will have "NO" because 2011/12 were not joined


# Using the CPCC_Group dataset, it was grouped by the conference groups, LaCode and Unique Identifier
# The event date was arrange in a ascending oder
# Children that were not registered were filtered out
# After, the registered children were filtered to make sure they have a review
# Using the summarise function and if statment to check that a child that is registered has a review or not
RegCPCCChart <- CPCC_Group %>%
  group_by(Conf_group, LaCode,UnqIdentifier) %>%
  arrange(EventDate) %>%
  filter(Registered %in% c("1", NA)) %>%
  filter(n() > 1 & any(Type == '03')) %>%
  summarise(Outcome = if ((any(Registered == "1", na.rm = TRUE))) 
    c("No", "Yes")[(which.max(Registered == "1") < which.max(Type == "03"))+1] else "No",
    Code = min(Code))


# The new dataset was filtered to show only outcome with the value "No"
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# To count by LA, the dataset was grouped by LaName before counting
# In the mutate fucnction, the ratio and label for the plot was calculated
# Finally a ggplot was used to plot the chart (a bar geoms)
RegCPCCChart %>% filter(Outcome == "No") %>% 
        mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
        group_by(LaName) %>%
        summarise(Count = n()) %>%
        mutate(LaName = fct_rev(LaName),
               Ratio = Count/ sum(Count),
               label = round(Ratio * 100, 1)) %>%
  ggplot( mapping =  aes(LaName, y=Ratio)) +
  geom_bar(stat='identity', fill = "#34a8f9") + coord_flip() + 
  geom_text(aes(x = LaName, y = Ratio,
                label = paste0(label,"%")), size=4, hjust = 0)+
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Children with Review Case Conference type but were not Registered") +
  labs(x="Local Authorities", y="Percentage") + 
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())







#  ~~ Check the gap between case conferences for those on the register. ~~~

# Using the CPCC_Group dataset, it was grouped by the conference groups, LaCode and Unique Identifier
# The event date was arrange in a ascending oder
# Children that were not registered were filtered out
# The select function was usde to selesct specific variables
# To calculate the difference, the next event date was subtracted from the first event date, and so on
# This was achieved using the lag function
# The dataset was filtered to show month difference > 0, 
# because when a value is 0 it means that's the date the child was registered
ReviewInterval <- CPCC_Group %>%
    group_by(Conf_group, LaCode, UnqIdentifier) %>%
    arrange(EventDate) %>%
    filter(Registered %in% c("1", NA)) %>% #keep only those registered or blanks (ie not deregistered?)
    select(Code:UnqIdentifier, Conf_group, EventDate) %>%
    mutate(MonthDif = EventDate - lag(EventDate),
           MonthDif = floor(MonthDif/2629743)) %>% filter(MonthDif > 0)


# Using the new dataset, the month different were grouped
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# Finally a ggplot was used to plot the chart (a bar geoms)
ReviewInterval %>% 
  mutate(Groups = ifelse(MonthDif <= 3, "1-3 months", 
                         ifelse(MonthDif  > 3 & MonthDif <= 6  ,"4-6 months", "more than 6 months"))) %>%
  group_by(LaCode, Groups) %>% 
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  mutate(LaName = fct_rev(LaName)) %>%
  ggplot( mapping =  aes(LaName, fill = Groups)) +
  geom_bar(position = "fill") + coord_flip() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=c("#34a8f9","#3446f9","#34f9e8")) +
  ggtitle("Review: Interval between Case Conferences") +
  labs(x="Local Authorities", y="Percentage") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())



# Simple dataframe to show the count and percentage of the Group
View(
  ReviewInterval %>% 
    mutate(Groups = ifelse(MonthDif <= 3, "1-3 months", 
                           ifelse(MonthDif  > 3 & MonthDif <= 6  ,"4-6 months", "more than 6 months"))) %>%
    group_by(Groups) %>% summarise(Count = n()) %>% 
    mutate(Per = round(Count/sum(Count) * 100, 2))
)



# Calculate the time (months) on the register for children who are registered.

# If the month is zero could mean the child's CPCC started from a previous which we do not have dataset on 
# (an example will be dataset from 2011/12). Or child whose CPCC had not ended yet (an example is child that CPCC continue to the 
# next calendar year 2018/19)

# Using the CPCC_Group dataset, it was grouped by the conference groups, LaCode and Unique Identifier
# The event date was arrange in a ascending oder
# Children that were not registered were filtered out
# The month_diff function was used to get the difference of the first date and last date
# The dataset was filtered to show month difference > 0
# The mutate function was used to change the Code to years (e.g CP1213 to 2013)
# Finally a ggplot was used to plot the chart (a freqpoly geoms)
CPCC_Group %>%
  group_by(Conf_group, LaCode, UnqIdentifier) %>%
  arrange(EventDate) %>%
  filter(Registered %in% c("1", NA)) %>%
  summarise(MonthRegistered = month_diff(max(EventDate), min(EventDate)), 
            Code = min(Code)) %>% filter(MonthRegistered > 0) %>% 
  mutate(CPYear = factor(Code, levels = CPYearLevels, labels = CPYears)) %>%
  ggplot(mapping = aes(MonthRegistered,colour = CPYear)) + geom_freqpoly(binwidth = 1, size = 1) +
  scale_fill_manual(values=c("#1cbfd3","#d3311d","#f98534","#8c1cd3","#63d31c","#e734f9")) +
  ggtitle("Number of Months on CP Register for Registered Children") +
  labs(x="Number of registered month", y="Total count") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())





# ~~ Calculate the time since the last deregistration for children who are registered more than once. ~~

# When months is zero means the person had been on the register once or thats the first time the person was 
# registered if the person had appeared on the register more than one. 


# Simple dataframe showing the count of registered children group in years(Code)
View (CPCC_Group %>%
        group_by(Conf_group, UnqIdentifier) %>%
        arrange(EventDate) %>%
        filter(Registered == "1") %>% group_by(Code) %>%
        summarise(Count = n())
)


# Using the CPCC_Group dataset, it was grouped by the conference groups, LaCode and Unique Identifier
# The event date was arrange in a ascending oder
# Children that had never been registered were filtered out
# The dataset was ungrouped before creating to variables NewDate (lag of the event date) 
# and NewID (lag of the unique identifier)
# The dataset was then grouped year(Code), LaCode, conference group and Unique Identifier
# Using the transmute function and ifelse statement to calculate the month differenc if the 
# Unique identifier and the new Id matches
RegChildren <- CPCC_Group %>%
    group_by(Conf_group, UnqIdentifier) %>%
    arrange(EventDate) %>%
    filter(Registered %in% c("1", NA)) %>%
    ungroup() %>%
    mutate(NewDate = lag(ymd(EventDate))) %>%
    mutate(NewID = lag(UnqIdentifier)) %>% group_by(Code, LaCode, Conf_group, UnqIdentifier) %>% 
    transmute(Months = ifelse(UnqIdentifier == NewID & Registered == "1", month_diff(EventDate, NewDate), 0)) %>% 
    na.omit()




# Simple dataframe showing the count of the Groups
View (RegChildren %>% filter(Months > 0) %>% 
        mutate(LastDeregistered = ifelse(Months >= 1 & Months <= 6, "6 months or less", 
                                         ifelse(Months > 6 & Months <= 12, "more than 6 month to a year", 
                                                ifelse(Months > 12 & Months <= 24, "more than a year to 2 years", "more than 2 years")))) %>%
        group_by(LastDeregistered, Code) %>% summarise(Count = n()) %>%
        spread(key = Code, value = Count)
)



# Using the new dataset, the month different were grouped
# The dataset was grouped in years(Code) and LastDeregisted
# The mutate function was used to change the Code to years (e.g CP1213 to 2013)
# Finally a ggplot was used to plot the chart (a bar geoms)
RegChildren %>% filter(Months > 0) %>% 
  mutate(LastDeregistered = ifelse(Months >= 1 & Months <= 6, "6 months or less", 
                                   ifelse(Months > 6 & Months <= 12, "more than 6 month to a year", 
                                          ifelse(Months > 12 & Months <= 24, "more than a year to 2 years", "more than 2 years")))) %>%
  group_by(Code, LastDeregistered) %>% 
  mutate(CPYear = factor(Code, levels = CPYearLevels, labels = CPYears)) %>%
  ggplot(mapping = aes(CPYear, fill = LastDeregistered)) + geom_bar(position = "dodge") +
  scale_fill_manual(values=c("#15aad5","#a115d5","#d54015","#4ad515")) +
  ggtitle("Number of Months Since Last Deregistered") +
  labs(x="Year", y="Number of children") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())







#Checking for children who had been registered before but the variable PreviouslyRegistered says "No"

# Using the CPCC_Group dataset, it was grouped by the LaCode and Unique Identifier
# The event date was arrange in a ascending oder
# Children that had never been registered were filtered out
# Because we wanted to number the registrartion, we filtered out observation that do not have 1 in their Register variable
# Using the transmute and ifelse statement,we checked if the previously registered variable contains the required value
# The dataset was grouped by LaCode, Error to count them
# Using the spread function the dataset was reshape it to make the values(Yes and No) of the error variables(column)
# The NA values were replaced by 0
# In the mutate function, row sum was used to calculate the total. Before calculating the percentage of the Yes and No for each LA
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# The select function was used to select the needed variables before renaming the variables
# A percentage sign was added to the all variables excluding LA
PreviouslyRegistered <- CPCC_Group %>%
    group_by(UnqIdentifier, LaCode) %>% 
    arrange(EventDate) %>%
    filter(Registered %in% c("1", NA)) %>%
    filter(Registered == "1") %>% mutate(Count = row_number()) %>%
    transmute(Error = ifelse(Registered == "1" & Count == 1 & PreviouslyRegistered  %in% c("0", "1", "2"), "No", 
                             ifelse(Registered == "1" & Count > 1 & PreviouslyRegistered %in% c("1","2"), "No", "Yes"))) %>%  # 0 =no 1=yes 2=don't know
  group_by(LaCode, Error) %>% count(name = "Count") %>% ungroup() %>%
  spread(key = Error, value = Count) %>%
  replace(is.na(.), 0) %>% mutate(Total = Yes + No) %>%
  mutate(No= round(100*No/Total)) %>%
  mutate(Yes= round(100*Yes/Total)) %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  select(LaName, No, Yes) %>% 
  rename("Local Authorities" = LaName, "Have the specified item" = No,
         "Did not have the specified item" = Yes) %>%
  rename_at(vars(-"Local Authorities"),function(x) paste0(x," %"))


# It was not possible to export the formattable table because of a missing library
# So to solve this problem, the dataset was divided into two
# One from Aberdeen City to Glasgow City and the other Highland to West Lothian
PreReg1 <- slice(PreviouslyRegistered, 1:16)
PreReg2 <- slice(PreviouslyRegistered, 17:32)


# Using the formattable function to make the dataframe look more nicer
formattable(PreReg1, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Local Authorities' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Have the specified item %' = color_tile("transparent", "#71CA97"),
              'Did not have the specified item %' = color_tile("transparent", "#ff7f7f")
            ))


# Using the formattable function to make the dataframe look more nicer
formattable(PreReg2, align =c("l","c","c","c","c", "c", "c", "c", "r"),
            list(
              'Local Authorities' = formatter("span", style = ~ style(color = "#2d2e2e",font.weight = "bold")),
              'Have the specified item %' = color_tile("transparent", "#71CA97"),
              'Did not have the specified item %' = color_tile("transparent", "#ff7f7f")
            ))






# Then compare with the time since last deregistration variable.

# Using the CPCC_Group dataset, it was grouped by the conference group and Unique Identifier
# The event date was arrange in a ascending oder
# Children that had never been registered were filtered out
# The dataset was ungrouped before creating to variables NewDate (lag of the event date) 
# and NewID (lag of the unique identifier)
# The dataset was then grouped year(Code), LaCode, conference group and Unique Identifier
# Using the transmute function and ifelse statement to check if the two dates matches
# The na.omit function is there to remove any NA values
# The mutate function was used to change the LaCode to readable LA names
# (e.g 100 to Aberdeen City)
# Finally a ggplot was used to plot the chart (a bar geoms)
CPCC_Group %>%
  group_by(Conf_group, UnqIdentifier) %>%
  arrange(UnqIdentifier, EventDate) %>%
  filter(Registered %in% c("1", NA)) %>%
  ungroup() %>%
  mutate(NewDate = lag(ymd(EventDate))) %>%
  mutate(NewID = lag(UnqIdentifier)) %>% group_by(Code, LaCode, Conf_group, UnqIdentifier) %>%
  transmute(Outcome = ifelse(UnqIdentifier != NewID, NA, 
                             ifelse(UnqIdentifier == NewID & Registered == "1" & NewDate == LastDeRegistrationDate, "Yes", "No"))) %>% 
  na.omit() %>% mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  mutate(LaName = fct_rev(LaName)) %>%
 
   ggplot( mapping =  aes(LaName, fill = Outcome)) +
  geom_bar(position = "fill") + coord_flip() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=c("#f03e35", "#28a0e3")) +
  ggtitle("Comparison between Deregistered Date and Last Deregisteration Date Variable") +
  labs(x="Local Authorities", y="Percentage") +
  theme(plot.title = element_text(hjust=0.5,size = 14, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.title = element_blank()) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())


# Same code as above but this shows the count and percentage
View (
  CPCC_Group %>%
    group_by(Conf_group, UnqIdentifier) %>%
    arrange(EventDate) %>%
    filter(Registered %in% c("1", NA)) %>%
    ungroup() %>%
    mutate(NewDate = lag(ymd(EventDate))) %>%
    mutate(NewID = lag(UnqIdentifier)) %>% group_by(Code, LaCode, Conf_group, UnqIdentifier) %>%
    transmute(Outcome = ifelse(UnqIdentifier != NewID, NA, 
                               ifelse(UnqIdentifier == NewID & Registered == "1" & NewDate == LastDeRegistrationDate, "Yes", "No"))) %>% 
    na.omit() %>% group_by(Outcome) %>%
    summarise(Count = n()) %>%
    mutate(Per = round(Count/sum(Count) * 100))
)





# ~~ Check date of birth is consistent for children who have records in more than one year.~~

# Using the complete dataset, the observations were grouped by unique identifier and LA code
# Using the summarise and an if_else statement, the date of birth for each child was check to find the consistency
View (
  CPDataset %>% 
    group_by(UnqIdentifier, LaCode) %>%
    summarise( Consistency = ifelse(n_distinct(DateOfBirth) == 1, "Yes", "No"))
)






############## TRANSFER #################

# ~~ How many matches are in the transfer file and case conference file to identify the history of a transfer child ~~

# The complete dataset was filtered to show only tranfer in data
# Specific variables were selected
# After, a unique ID was created using the originating LaCode and previous child ID
# Children that were tranfered in outside scotland were filtered out
# Using a semi join, the Transfer dataset was joined to the complete dataset
View (
  CPDataset %>%
    filter(Type == "04") %>%
    select(Code:UnqIdentifier, Type, OriginatingLaCode, PreviousChildId) %>%
    mutate(UnqID = paste(OriginatingLaCode, PreviousChildId, sep = "")) %>% #this is so the original uniqueid can be matched 
    filter(OriginatingLaCode != "900") %>%   #900 = outside Scotland
    semi_join(Transfer, c("UnqID" = "UnqIdentifier")) #semi_join never duplicates rows of x
)









# Child Sexual Abuse and Exploitation

# This code count if a child had ever been sexually abused. This also shows if they had been sexually abused on all the time they had been registered. 

View (
  CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, Conf_group, Code) %>% 
    filter(SexualAbuse == "1") %>% select(Code:UnqIdentifier, Conf_group, SexualAbuse) %>% 
    distinct %>%
    count(UnqIdentifier, Code, name = "Count")
)

# Total number a child has been recognised that they have been sexually abused if the have been registered multiple times
View (
  CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, Conf_group, Code) %>% 
    filter(SexualAbuse == "1") %>% select(Code:UnqIdentifier, Conf_group, SexualAbuse) %>% 
    distinct %>%
    count(UnqIdentifier, Code) %>% ungroup() %>%
    group_by(LaCode, UnqIdentifier) %>% summarise( Count = n())
)


# This code count if a child had ever been sexually exploited. This also shows if they had been sexually abused on all the time they had been registered.
View (
  CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, Conf_group, Code) %>% 
    filter(ChildSexualExploitation == "1") %>% select(Code:UnqIdentifier, Conf_group, ChildSexualExploitation) %>% 
    distinct %>%
    count(UnqIdentifier, Code, name = "Count")
)


# Total number a child has been recognised that they have been sexually exploited if the have been registered multiple times
View (
  CPCC_Group %>%
    group_by(LaCode, UnqIdentifier, Conf_group, Code) %>% 
    filter(ChildSexualExploitation == "1") %>% select(Code:UnqIdentifier, Conf_group, ChildSexualExploitation) %>% 
    distinct %>%
    count(UnqIdentifier, Code) %>% ungroup() %>%
    group_by(LaCode, UnqIdentifier) %>% summarise( Count = n())
)

CPCC_Group %>%
  group_by(LaCode, UnqIdentifier, Conf_group, Code) %>% 
  filter(ChildSexualExploitation == "1") %>% select(Code:UnqIdentifier, Conf_group, ChildSexualExploitation) %>% 
  distinct %>%
  count(UnqIdentifier, Code) %>% ungroup() %>%
  group_by(LaCode, UnqIdentifier, Code) %>% summarise( Count = n()) %>% 
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  ggplot(aes(LaName, fill = Code)) + geom_bar() +
  coord_flip() + ggtitle("Child Sexual Exploitation") +
  xlab("Local Authorities")



CSE <- CPCC_Group %>%
  group_by(LaCode, UnqIdentifier, Conf_group, Code) %>% 
  filter(ChildSexualExploitation == "1") %>% select(Code:UnqIdentifier, Conf_group, ChildSexualExploitation) %>% 
  distinct %>%
  count(UnqIdentifier, Code) %>% ungroup() %>%
  group_by(LaCode) %>% summarise( SexualExploitation = n())


SA <-   CPCC_Group %>%
  group_by(LaCode, UnqIdentifier, Conf_group, Code) %>% 
  filter(SexualAbuse == "1") %>% select(Code:UnqIdentifier, Conf_group, SexualAbuse) %>% 
  distinct %>%
  count(UnqIdentifier, Code) %>% ungroup() %>%
  group_by(LaCode) %>% summarise( SexualAbuse = n())


full_join(SA,CSE, by = "LaCode") %>% mutate_all(~replace(., is.na(.), 0)) %>% 
  gather(key = "Abuse", value, -LaCode) %>%
  uncount(value) %>%
  mutate(LaName = factor(LaCode, levels = LaCodeLevels, labels = LaNames)) %>%
  ggplot(mapping = aes(LaName, fill = Abuse)) + geom_bar() +
  coord_flip() + ggtitle("Abuse: Child Sexual Exploitation vs Abuse") +
  xlab("Local Authorities")









  
 


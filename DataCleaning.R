##################################
##################################

# Title: 'IMPORT-DATA.R'
# Description: This script imports, merges and labels the required data for every year for the analysis of the German Minimum 
# Wage increase
# Author: Dominik Prugger
# Date: 2.2.2018

##################################
##################################


#### SKRIPT  ####  ####

#install necessary packages
#install.packages("dplyr")

"dplyr"### IMPORT DATA SKRIPT ### Load required libraries. They are preinstalled except plyr & dplyr
library(foreign)
library(stringr)
library(plyr)
library(dplyr)
# Make Sure you check your Working Directory so that the code works flawless!
getwd()
# Otherwise Set the Working Directory 
setwd("~/Utrecht/GithubCode/Analysis-Minimum Wage")


### IMPORT, MERGE AND CLEAN ALL DATA ### First for the -p dataset (personal questions)
#We need two control variables: i is to step through the list of years, beginning with
### 1 k is always one digit higher as it reads the second column of the feature selection list (the first column is the label)
i = 1
k = 2
# List all directories within the input data, non-recursive
list_dirs = list.dirs(path = "./waves-input", recursive = FALSE)
# Extract the year name of the directories, so the last 4 digits
list_years = str_sub(list_dirs, -4)
# Create Variable names for every merged year based on the style merged[year]
list_varnames = paste("merged", list_years, sep = "")
# Load the feature list we cleaned manually in Excel as CSV
soep_selection = read.table("soep-feature-selection3.csv", header = TRUE, sep = ";", check.names = FALSE)
close(soep_selection)
# Get all Labels, unfiltered
labels = soep_selection[, 1]
# Create a vector to put object names of all years in it
datalist = c()

# Loop through all the years, import the data, merge, clean and label them
for (years in list_years) {
    # Define Current List of import data based on the 'i' value
    list_files = list.files(path = list_dirs[i], pattern = "", full.names = TRUE)
    # Import all the data from the current list with the read.dta-Function (part of foreign package) for SPSS-Files
    list_import = lapply(list_files, read.dta)
    # Merge it into one file
    data_merged = Reduce(function(x, y) merge(x, y, by = "persnr", all.x = TRUE), list_import)
    # Cut the .x and y. values from the merge process, so that we have clean column names
    colnames(data_merged) = gsub("\\.x|\\.y", "", colnames(data_merged))
    # Get the feature list of the current year
    current_list = sort(soep_selection[, k])
    # Delete all columns where no data exists (as the surveys differed every year) -> not needed as import function excludes
    # missing values shortlist = na.omit(current_list)
    
    # ONLY take the data shortlisted for the current year
    cleaned = data_merged[, which(names(data_merged) %in% current_list == TRUE)]
    
    # Select the Label Column and the Variable Column of the current Year
    soep_subscrit = c(1, k)
    # Subset the Feature list so that only the label and the current year exist
    soep_selection_sub = soep_selection[soep_subscrit]
    # Delete NA-Values from the list
    soep_selection_sub = na.omit(soep_selection_sub)
    
    # Create a subset of the clean labels, where all codenames match, to make sure that the labels are correct
    test = which(soep_selection_sub[, 2] %in% colnames(cleaned))
    clean_labels = soep_selection_sub[test, ]
    # Order Dataframe alphabetically
    clean_sorted = cleaned[, order(names(cleaned))]
    # Order Frame with the Labels based on the ID
    ordered_colnames = clean_labels[order(clean_labels[2]), ]
    # Label the columns properly
    colnames(clean_sorted) = ordered_colnames[, 1]
    # Assign data_merged to current merge[year]
    assign(list_varnames[i], clean_sorted)
    # Add Year Variable to a list so that we can access all years by a loop
    datalist = c(datalist, list_varnames[i])
    # Update our variables for the next round
    i = i + 1
    k = k + 1
}

#Merge all Data files
#OPTIONAL; if you want a fully balanced panel: Find the Variables that exist in all datasets (balanced)
#commonvar = Reduce(intersect, list(colnames(merged2010),colnames(merged2011),colnames(merged2012), colnames(merged2013), colnames(merged2014),
                       #colnames(merged2014)))
#function to subset dataframes
#commonvar_fun = function(x){
#  subset(x, select = commonvar)}

#apply the function for all 6 years
#merged2010 = commonvar_fun(merged2010)
#merged2011 = commonvar_fun(merged2011)
#merged2012 = commonvar_fun(merged2012)
#merged2013 = commonvar_fun(merged2013)
#merged2014 = commonvar_fun(merged2014)
#merged2015 = commonvar_fun(merged2015)

#Append all into one panel

df = rbind.fill(merged2010, merged2011, merged2012, merged2013, merged2014, merged2015)

###### As the first part is for the general personalised data, in a second step another loop is performed to get the 
######personal regional data (federal counties) from the equiv-core data sets. 

i = 1
k = 2
# List all directories within the input data, non-recursive
list_dirs = list.dirs(path = "./equiv-input", recursive = FALSE)
# Extract the year name of the directories, so the last 4 digits
list_years = str_sub(list_dirs, -4)
# Create Variable names for every merged year based on the style merged[year]
list_varnames = paste("merged", list_years, sep = "")
# Load the feature list we cleaned manually in Excel as CSV
soep_selection = read.table("soep-equiv-selection.csv", header = TRUE, sep = ";", check.names = FALSE)
close(soep_selection)
# Get all Labels, unfiltered
labels = soep_selection[, 1]
# Create a vector to put object names of all years in it
datalist = c()

# Loop through all the years, import the data, merge, clean and label them
for (years in list_years) {
  # Define Current List of import data based on the 'i' value
  list_files = list.files(path = list_dirs[i], pattern = "", full.names = TRUE)
  # Import all the data from the current list with the read.dta-Function (part of foreign package) for SPSS-Files
  list_import = lapply(list_files, read.dta)
  # Merge it into one file
  data_merged = Reduce(function(x, y) merge(x, y, by = "persnr", all.x = TRUE), list_import)
  # Cut the .x and y. values from the merge process, so that we have clean column names
  colnames(data_merged) = gsub("\\.x|\\.y", "", colnames(data_merged))
  # Get the feature list of the current year
  current_list = sort(soep_selection[, k])
  # Delete all columns where no data exists (as the surveys differed every year) -> not needed as import function excludes
  # missing values shortlist = na.omit(current_list)
  
  # ONLY take the data shortlisted for the current year
  cleaned = data_merged[, which(names(data_merged) %in% current_list == TRUE)]
  
  # Select the Label Column and the Variable Column of the current Year
  soep_subscrit = c(1, k)
  # Subset the Feature list so that only the label and the current year exist
  soep_selection_sub = soep_selection[soep_subscrit]
  # Delete NA-Values from the list
  soep_selection_sub = na.omit(soep_selection_sub)
  
  # Create a subset of the clean labels, where all codenames match, to make sure that the labels are correct
  test = which(soep_selection_sub[, 2] %in% colnames(cleaned))
  clean_labels = soep_selection_sub[test, ]
  # Order Dataframe alphabetically
  clean_sorted = cleaned[, order(names(cleaned))]
  # Order Frame with the Labels based on the ID
  ordered_colnames = clean_labels[order(clean_labels[2]), ]
  # Label the columns properly
  colnames(clean_sorted) = ordered_colnames[, 1]
  # Assign data_merged to current merge[year]
  assign(list_varnames[i], clean_sorted)
  # Add Year Variable to a list so that we can access all years by a loop
  datalist = c(datalist, list_varnames[i])
  # Update our variables for the next round
  i = i + 1
  k = k + 1
}

#then merge the files into one panel and append it to the existing data frame 
#add a year variable for the dataframes, as it doesnt exist in the SOEP-EQUIV
merged2010$Year = 2010
merged2011$Year = 2011
merged2012$Year = 2012
merged2013$Year = 2013
merged2014$Year = 2014
merged2015$Year = 2015

#Append the Equiv data sets and merge them with df
df2 = rbind(merged2010, merged2011, merged2012, merged2013, merged2014, merged2015)

#to merge, need to change the column name of DF2
names(df2)[names(df2) == "never.Changing.Person.ID"] = "never Changing Person ID"
df3 =merge(df, df2, by=c("never Changing Person ID", "Year"))

#delete columns that are not needed
drops = c("Label", "Future Job Prospects" )
df3 = df3[ , !(names(df) %in% drops)]
#save as new csv file in working directory 
write.csv(df3, file = "df1-Panel.csv")


# Read in the data from the previous do-file
df = df3

################################ Calculate Dummies and reassign specific values

#Define the hourly wage as: gross monthly income divided by contracted hours *4 
#before: Define the gross income as 0 if -1 (no answer); -2 (doesn't apply); -3 (implausible) 
df$`Gross income last month`[df$`Gross income last month` < 0 ] = NA 

#Then calculate the monthly working time and hourly wage
df$`Contracted Working Hours`[df$`Contracted Working Hours` < 0 ] = NA
df$`Hours per Week Actual`[df$`Hours per Week Actual` <0] = NA
df$work.monthly = (df$`Contracted Working Hours`/10) *4 
df$wage.hour = df$`Gross income last month`/df$work.monthly

#Create Dummy that indicates if wage per hour in 2014 was belov 8,50 € 
df$d.wage = ifelse(df$wage.hour < 8.5 & df$Year == 2014, 1, 0)

#Create Dummy for sex 
df$d.sex[df$Sex == "[1] Maennlich"] = 0
df$d.sex[df$Sex == "[2] Weiblich"] = 1
df$d.sex[df$Sex == "[-5] In Fragebogenversion nicht enthalten"] = NA

#Create Dummy for Sex
df$d.sex[df$Sex == "[1] Maennlich"] = 0
df$d.sex[df$Sex == "[2] Weiblich"] = 1
df$d.sex[df$Sex == "[-5] In Fragebogenversion nicht enthalten"] = NA

#create dummy for health (just better than avg (1) and worse than avg. (0))
df$d.health[df$`Current Health` == "[4] Weniger gut"] = 0
df$d.health[df$`Current Health` == "[5] Schlecht"]  = 0
df$d.health[df$`Current Health` == "[2] Gut"]  = 1
df$d.health[df$`Current Health` == "[1] Sehr gut"]  = 1
df$d.health[df$`Current Health` == "[3] Zufriedenstellend"]  = NA
df$d.health[df$`Current Health` == "[-1] keine Angabe"] = NA

#create dummy for unemployment
df$d.employment[df$`Registered Unemployed` == "[-1] keine Angabe"] = NA
df$d.employment[df$`Registered Unemployed` == "[1] Ja"] = 1
df$d.employment[df$`Registered Unemployed` == "[2] Nein"] = 0

#change the negative values for the asked wage to get a job to NA
df$`Monthly Salary at which would take Job`[df$`Monthly Salary at which would take Job` <0] = NA

#create an age variable, as current year minus year of birth and set all negative values to NA
df$`Year of Birth`[df$`Year of Birth`<0] = NA
df$age = df$Year - df$`Year of Birth`

#Change years worked on last job: Negative values are NA
df$Years.Worked.on.Last.Job[df$Years.Worked.on.Last.Job < 0 ] = NA

###########Part 2: Collapse and Aggregate the individual Panel into an aggregated Data set over the two states and 
#merge them with google trend Data
df$Federal.State <- df$`Federal State`
df$Monthly.Salary.at.which.would.take.Job <- df$`Monthly Salary at which would take Job`

#Collapse the panel into an aggregate data for the Federal states
df$Federal.State <- df$`Federal State`
#Define a second aggregate Dataset based on the federal states, which shows the mean wage for every state and year
Statedata = ddply(df, .(Federal.State, Year), summarize, wage.mean = mean(wage.hour, na.rm = T), 
                  female.perc = mean(d.sex, na.rm = T),
                  good.health.perc = mean(d.health, na.rm = T),
                  unemp.perc = mean(d.employment, na.rm = T), 
                  mean.age = mean(age, na.rm = T), 
                  mean.askingwage = mean(Monthly.Salary.at.which.would.take.Job, na.rm = TRUE))

#Define a "Bite" for the minimum wage for the as the Kaitz Index: Mean(wage) / minimum wage
Statedata$Bite = Statedata$wage.mean/8.5

#create dataframe for the google trend data

#We need two control variables: i is to step through the list of years, beginning with
### 1 k is always one digit higher as it reads the second column of the feature selection list (the first column is the label)
i = 1
k = 2
# List all directories within the input data, non-recursive
list_dirs = list.dirs(path = "./google-input", recursive = FALSE)
# Extract the year name of the directories, so the last 4 digits
list_years = str_sub(list_dirs, -4)
# Create Variable names for every merged year based on the style merged[year]
list_varnames = paste("merged", list_years, sep = "")
# Create a vector to put object names of all years in it
datalist = c()

# Loop through all the years, import the data, merge, clean and label them
for (years in list_years) {
  # Define Current List of import data based on the 'i' value
  list_files = list.files(path = list_dirs[i], pattern = "", full.names = TRUE)
  #define function for reading in the google data
  f = function(x){
    read.csv(x, skip = 2)
  }
  # Import all the data from the current list with the read.dta-Function (part of foreign package) for SPSS-Files
  list_import = lapply(list_files, f)
  # Merge it into one file
  cleaned = as.data.frame(list_import)
  #rename column name
  colnames(cleaned) = c("Federal.State", "Trend.perc")
  #add a Year Variable 
  cleaned$Year = i+2009
  # Assign data_merged to current merge[year]
  assign(list_varnames[i], cleaned)
  # Update our variables for the next round
  i = i + 1
  k = k + 1
}

#add the respective year 
#and append all merged data
Google = rbind(merged2010, merged2011, merged2012, merged2013, merged2014, merged2015)

#and merge with existing aggregate data set Statedata
#Step 1: Check if they both have the same colnames
commoncol = intersect(colnames(Statedata), colnames(Google))
print(commoncol)

#Step 2: Check if they both have the same Statenames
commonstates = intersect(Statedata$Federal.State, Google$Federal.State)
print(commonstates)

#As the Country names differ; one has to merge the two different set of state names
Statedata$Federal.State = as.character(Statedata$Federal.State)
Google$Federal.State    = as.character(Google$Federal.State)

#Change all the Names for Statedata
Statedata$Federal.State[grep("erlin", Statedata$Federal.State)] = "Berlin"
Statedata$Federal.State[grep("avaria", Statedata$Federal.State)] = "Bavaria"
Statedata$Federal.State[grep("heinland", Statedata$Federal.State)] = "Rheinland-Pfalz"
Statedata$Federal.State[grep("essen", Statedata$Federal.State)] = "Hessen"
Statedata$Federal.State[grep("hine", Statedata$Federal.State)] = "North-Rhine-Westphalia"
Statedata$Federal.State[grep("remen", Statedata$Federal.State)] = "Bremen"
Statedata$Federal.State[grep("ower", Statedata$Federal.State)] = "Lower-Saxon"
Statedata$Federal.State[grep("amburg", Statedata$Federal.State)] = "Hamburg"
Statedata$Federal.State[grep("huringia", Statedata$Federal.State)] = "Thuringia"
Statedata$Federal.State[grep("aden", Statedata$Federal.State)] = "Baden-Wuerttenberg"
Statedata$Federal.State[grep("nhalt", Statedata$Federal.State)] = "Saxon-Anhalt"
Statedata$Federal.State[grep("Saxony", Statedata$Federal.State)] = "Saxony"
Statedata$Federal.State[grep("orpommern", Statedata$Federal.State)] = "Mecklenburg"
Statedata$Federal.State[grep("randenburg", Statedata$Federal.State)] = "Brandenburg"
Statedata$Federal.State[grep("aarland", Statedata$Federal.State)] = "Saarland"
Statedata$Federal.State[grep("chleswig", Statedata$Federal.State)] = "Schleswig-Holstein"

#Change all the names in Google 
Google$Federal.State[grep("avaria", Google$Federal.State)] = "Bavaria"
Google$Federal.State[grep("herlin", Google$Federal.State)] = "Berlin"
Google$Federal.State[grep("heinland", Google$Federal.State)] = "Rheinland-Pfalz"
Google$Federal.State[grep("essen", Google$Federal.State)] = "Hessen"
Google$Federal.State[grep("estphalia", Google$Federal.State)] = "North-Rhine-Westphalia"
Google$Federal.State[grep("remen", Google$Federal.State)] = "Bremen"
Google$Federal.State[grep("ower", Google$Federal.State)] = "Lower-Saxon"
Google$Federal.State[grep("hamburg", Google$Federal.State)] = "Hamburg"
Google$Federal.State[grep("huringia", Google$Federal.State)] = "Thuringia"
Google$Federal.State[grep("aden", Google$Federal.State)] = "Baden-Wuerttenberg"
Google$Federal.State[grep("nhalt", Google$Federal.State)] = "Saxon-Anhalt"
Google$Federal.State[grep("Saxony", Google$Federal.State)] = "Saxony"
Google$Federal.State[grep("orpommern", Google$Federal.State)] = "Mecklenburg"
Google$Federal.State[grep("randenburg", Google$Federal.State)] = "Brandenburg"
Google$Federal.State[grep("aarland", Google$Federal.State)] = "Saarland"
Google$Federal.State[grep("chleswig", Google$Federal.State)] = "Schleswig-Holstein"

#Finally: Merge both Datasets
Statedata = merge(Statedata, Google, by = c("Federal.State", "Year"))


#save both files
write.csv(file = "df2-indiv.csv", df)
write.csv(file = "df3-States.csv", Statedata)



# Delete everything - Cleanup-Command - BE CAREFUL WITH IT!
# rm(list=ls(all=TRUE))


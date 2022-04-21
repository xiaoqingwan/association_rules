library(tidyverse) #Data Manipulation and Plotting
library(arulesSequences) #Running the Sequence mining algorithm
library(readxl)
library(stringr)
library(ggtext) #Making adding some flair to plots
library(tidygraph)  ## Creating a Graph Structure
library(ggraph) ## Plotting the Network Graph Structure
# library(lubridate) #Date Manipulation

df <- read.csv(file = 'all_rx_drugs.csv')
options(scipen=999)

# data cleaning -----------------------------------------------------------

df1 <- df %>% 
  group_by(patient.identifier) %>% 
  arrange(claim.fillDate) %>% 
  # Create Item ID Within Patient ID
  mutate(item_id = row_number()) %>% 
  select(patient.identifier, claim.fillDate, item_id, claim.moleculeName) %>% 
  ungroup() %>% 
  #Convert Everything to Factor
  mutate(across(.cols = c("patient.identifier", "claim.moleculeName"), .f = as.factor))

df1 <- df1[order(df1$patient.identifier),] # descending
a <- head(df1, 50)
# handle the special case where one patient filled multiple drugs  --------

df2 <- df1

df2$unique<-paste0(as.character(df2$patient.identifier)," ", as.character(df2$claim.fillDate)) # create unique id for each patient-date pair
df2 <- df2 %>% # if a patient filled multiple drugs on the same date, merge these drugs as a "cocktail" in the format of (A,B)
  dplyr::group_by(unique) %>%
  dplyr::summarise(claim.moleculeName = paste(claim.moleculeName, collapse = ","))

df2$patient.identifier <- word(df2$unique, 1) # restore patient id that was lost in the last step
df2$claim.fillDate <- word(df2$unique, 2)  # restore fill date that was lost in the last step

df2 <- df2 %>% 
  group_by(patient.identifier) %>% 
  arrange(claim.fillDate) %>% 
  mutate(item_id = row_number()) %>% #Create Item ID Within Patient ID
  select(patient.identifier, claim.fillDate, item_id, claim.moleculeName) %>% 
  ungroup()

df2 <- df2 %>% arrange(patient.identifier)
  

# view cocktail -----------------------------------------------------------
# show only rows with cocktail
library(data.table)
df3 <- df2[df2$claim.moleculeName %like% ",", ]

df3 <- as.data.frame(table(df3$claim.moleculeName)) #see the frequency of molecules
df3 <- df3[order(-df3$Freq),] # descending
write.csv(x=df3, file="cocktail.csv", row.names=FALSE)


a<-head(df2,50)
length(unique(df3$claim.moleculeName))


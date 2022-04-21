library(tidyverse) #Data Manipulation and Plotting
library(arulesSequences) #Running the Sequence mining algorithm
library(readxl)
library(stringr)

df <- read.csv(file = 'all_rx_drugs.csv')
options(scipen=999)
# demographics ------------------------------------------------------------

# 2411958 observations
length(unique(df$patient.identifier))
# 71574 patients
length(unique(df$product_name))
# 266 products
length(unique(df$claim.moleculeName))
# 80 molecules
length(unique(df$claim.medication_code))
# 2592 ndc id's 
which(is.na(df$claim.moleculeName))
sum(is.na(df$product_name))

a <- head(df, 50)

b <- as.data.frame(table(df$claim.moleculeName)) #see the frequency of molecules
b <- b[order(-b$Freq),] # descending
write.csv(x=b, file="all_molecules.csv", row.names=FALSE)

a <- df[(df$claim.moleculeName=="Soap"),]
unique(df$pharm_classes)

# data cleaning -----------------------------------------------------------

df1 <- df %>% 
  group_by(patient.identifier) %>% 
  arrange(claim.fillDate) %>% 
  #Remove Instances where the same drug appears consecutively
  # filter(claim.moleculeName != lag(claim.moleculeName) | is.na(lag(claim.moleculeName))) %>%
  #Remove Instances where the same drug appears repeatedly
  distinct(patient.identifier, claim.moleculeName, .keep_all = TRUE) %>%
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
  
save(df2,file="df2.Rda")

a<- head(df2, 50)
# c-spade pre-process -----------------------------------------------------

load("df2.Rda")

df2 %>% head(5) %>% knitr::kable()

sessions <-  as(df2 %>% transmute(items = claim.moleculeName), "transactions")
transactionInfo(sessions)$sequenceID <- df2$patient.identifier
transactionInfo(sessions)$eventID <- df2$item_id

itemLabels(sessions) <- str_replace_all(itemLabels(sessions), "items=", "")

inspect(head(sessions,10))


# cspade ------------------------------------------------------------------

itemsets <- cspade(sessions, 
                   parameter = list(support = 0.001), 
                   control = list(verbose = FALSE))
inspect((itemsets))
df3 <- itemsets

#Convert Back to DS
df3 <- as(df3, "data.frame") %>% as_tibble()
df3$pattern <- (str_count(df3$sequence, ",") + 1)
df3 <- df3[order(-df3$support),] # descending
write.csv(x=df3, file="all_results.csv", row.names=FALSE)

c <- df3 %>% group_by(pattern) %>% slice_max(order_by = support, n = 20)
write.csv(x=c, file="top_results.csv", row.names=FALSE)



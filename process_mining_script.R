library(tidyverse)
library(bupaR)

domestic_dec <- read.csv2('DomesticDeclarations.csv', check.names=FALSE)
international_dec <- read.csv2('InternationalDeclarations.csv', check.names=FALSE)
#travel_cost <- read.csv2('PrepaidTravelCost.csv', check.names=FALSE)

# Checking if two timestamp columns are the same
#sum(domestic_dec$`time:timestamp` == domestic_dec$Timestamp)
#sum(international_dec$`time:timestamp` == international_dec$Timestamp)

# Timestamp Adjustment
domestic_dec$Timestamp <- paste0(domestic_dec$Timestamp, ':00')
international_dec$Timestamp <- paste0(international_dec$Timestamp, ':00')

# Simplifying the logs as Signvaio does not require other columns
domestic_dec2 <- domestic_dec %>% select(`Case-Id`, `concept:name`, Timestamp)
international_dec2 <- international_dec %>% select(`Case-id`, `concept:name`, Timestamp)

# Adjustment of caseids
international_dec2$`Case-id` <- international_dec2$`Case-id` + max(domestic_dec2$`Case-Id`)

# Adjustment of different column names
names(international_dec2) <- names(domestic_dec2)

# Merging both logs
merged_logs <- rbind(domestic_dec2, international_dec2)

# Exporting the log
write.csv2(merged_logs, 'MergedDeclarations.csv', quote = FALSE, row.names = FALSE)






#################################################################################### Beginning

domestic_dec <- read.csv2('DomesticDeclarations.csv', check.names=FALSE)
international_dec <- read.csv2('InternationalDeclarations.csv', check.names=FALSE)
#travel_cost <- read.csv2('PrepaidTravelCost.csv', check.names=FALSE)

domestic_dec$Timestamp <- as.POSIXct(domestic_dec$Timestamp, format = "%d.%m.%Y %H:%M")
domestic_dec$status <- "completed"
domestic_dec$eventid <- domestic_dec$`Event-Id`
domestic_dec$caseid <- domestic_dec$`Case-Id`
domestic_dec$caseid <- as.numeric(domestic_dec$caseid)

international_dec$Timestamp <- as.POSIXct(international_dec$Timestamp, format = "%d.%m.%Y %H:%M")
international_dec$status <- "completed"
international_dec$eventid <- international_dec$`Event-Id`
international_dec$caseid <- international_dec$`Case-id`
international_dec$caseid <- as.numeric(international_dec$caseid)

domestic_log <-domestic_dec %>%
  eventlog(
    case_id = "caseid",
    activity_id = "concept:name",
    activity_instance_id = 'eventid',
    timestamp = "Timestamp",
    resource_id = "org:resource",
    lifecycle_id = "status"
  )

international_log <-international_dec %>%
  eventlog(
    case_id = "caseid",
    activity_id = "concept:name",
    activity_instance_id = 'eventid',
    timestamp = "Timestamp",
    resource_id = "org:resource",
    lifecycle_id = "status"
  )


################# Question 1

### Domestic 

domestic_log_flagRej <- domestic_log %>% 
  group_by_case() %>%
  mutate(rejected_case = any(str_detect(`concept:name`, "REJECT")), approved_case = any(str_detect(`concept:name`, "Payment")) ) %>%
  summarise(rejected_flag = any(rejected_case), approved_flag = any(approved_case))

domestic_log_flagRej$caseid <- as.numeric(domestic_log_flagRej$caseid)


# Both flags can never be FALSE at the same time. Indication of faulty log or saved for later declarations.
domestic_log_flagRejClean <- domestic_log_flagRej %>% filter(rejected_flag == TRUE |approved_flag == TRUE)



### International 

international_log_flagRej <- international_log %>% 
  group_by_case() %>%
  mutate(rejected_case = any(str_detect(`concept:name`, "REJECT")), approved_case = any(str_detect(`concept:name`, "Payment")) ) %>%
  summarise(rejected_flag = any(rejected_case), approved_flag = any(approved_case))

international_log_flagRej$caseid <- as.numeric(international_log_flagRej$caseid)


# Both flags can never be FALSE at the same time. Indication of faulty log or saved for later declarations.
international_log_flagRejClean <- international_log_flagRej %>% filter(rejected_flag == TRUE |approved_flag == TRUE)

# Answers

# Number of Rejected Declarations at various points(domestic)
sum(domestic_log_flagRejClean$rejected_flag)

# Number of Never Approved Declarations (domestic)
nrow(domestic_log_flagRejClean)-sum(domestic_log_flagRejClean$approved_flag)

# Number of Rejected Declarations at various points(international)
sum(international_log_flagRejClean$rejected_flag)

# Number of Never Approved Declarations (international)
nrow(international_log_flagRejClean)-sum(international_log_flagRejClean$approved_flag)



################# 2. Question
# Looking at the activity names
unique(domestic_log$`concept:name`)
unique(international_log$`concept:name`)


################# 3. Question

# Question is not answerable for domestic trips as the beginning and end dates of the trips are unknown for domestic trips.
# Rejected Declarations at some point
caseids_rejected <- international_log_flagRejClean %>% 
  filter(rejected_flag == TRUE)

international_log_Rejected <- international_log[international_log$caseid %in% caseids_rejected$caseid,]

# Finding out which cases/ declarations are submitted more than one(resubmited)
international_resubmitted <- international_log_Rejected %>%
  filter(str_detect(`concept:name`, "Declaration SUBMITTED") | str_detect(`concept:name`, "End trip")) %>%
  group_by_case() %>%
  filter(n() >2) %>%
  ungroup_eventlog()

# Filtering out those whose end trip date is later than the submission date of declarations

international_resubmitted_2 <- international_resubmitted %>%
  group_by_case() %>%
  filter(row_number()==1) %>%
  ungroup_eventlog() %>%
  filter(`concept:name` == "End trip")
  

# Calculate the date difference

international_resubmitted_3 <- international_resubmitted %>%
  filter(caseid %in% international_resubmitted_2$caseid) %>%
  group_by_case() %>%
  filter(row_number() == 1 | row_number() ==2) %>%
  summarise(difference_date = Timestamp[`concept:name` == "Declaration SUBMITTED by EMPLOYEE"] - Timestamp[`concept:name` == "End trip"])

international_resubmitted_3$caseid <- as.numeric(international_resubmitted_3$caseid)
international_resubmitted_3$difference_date <- international_resubmitted_3$difference_date / 86400 # seconds to days conversion

# Answer to question 3
nrow(international_resubmitted_3[international_resubmitted_3$difference_date>=60,])

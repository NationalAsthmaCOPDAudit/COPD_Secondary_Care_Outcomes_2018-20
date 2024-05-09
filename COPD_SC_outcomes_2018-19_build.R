#----------------------------------------------------------------------------------------------------------#
#                                                                                                          #
#   C O P D   S C   O U T C O M E S   B U I L D   S C R I P T ::  S C C    C L E A N \\ P E D W \\ H E S   #
#                                                                                                          #
#   Author: Alex Adamson                                                                                   #
#----------------------------------------------------------------------------------------------------------#

# Let's set up something new so that we don't end up overwriting logs accidentally

# Things to note:

# Sink can write to both the console and a text file if you write 'split = TRUE'.
# If you use the 'cat' command instead of 'print' command it doesn't have a number before it
# when it's printed. You can also use /n to signal a new line. Good to use it for when you
# just want to paste something.
# Use sink() to stop the sink.



sink() # Just putting this here so that if I run it over again it doesn't create more and more sinks...

filename <- "C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Analysis/Logs/COPD_SC_Outcomes_2019_build_log"
filedate <- Sys.Date()

sink(file = paste(filename, filedate, ".txt", sep = ""),
     append = FALSE,
     split = TRUE)

cat("\n START \n") # This means that every time I run it it restarts the document instead of getting an
                   # unuseable document at the end

sink()

sink(file = paste(filename, filedate, ".txt", sep = ""),
     append = TRUE,
     split = TRUE)


# Set up the libraries

library(dplyr)
# library(readstata13)
# library(xlsx)
source("H:/My R functions/MySummary.R")
library(janitor)
library(officer)
library(flextable)
# library(tidyverse) # doesn't work :(
library(tidyr)
library(purrr)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(comorbidity)

nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}
CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}



checkSame <- function(newdata, olddata) {
  
  library(dplyr)
  
  newdat <- data.frame(variable = colnames(newdata), in_new = 1, class_new = NA) 

  for (i in 1:ncol(newdata)) {
    newdat$class_new[i] <- paste(class(newdata[[i]]), collapse = " ")
  }
  
  
  olddat <- data.frame(variable = colnames(olddata), in_old = 1, class_old = NA)

  for (i in 1:ncol(olddata)) {
    olddat$class_old[i] <- paste(class(olddata[[i]]), collapse = " ")
  }
 
  
  samesame <- dplyr::full_join(newdat, olddat, by = "variable")
  samesame$in_both <- 0
  samesame$in_both[samesame$in_new == 1 & samesame$in_old == 1] <- 1
  return(samesame)
  
}
  

niceDate <- function(datevar) {
  year <- as.numeric(substr(datevar, 1, 4))
  print("Years:")
  print(summary(year))
  
  month <- as.numeric(substr(datevar, 5, 6))
  print("Months:")
  print(summary(month))
  
  day <- as.numeric(substr(datevar, 7, 8))
  print("Days:")
  print(summary(day))
  
  date <- paste(year, month, day, sep = "-")
  date <- as.Date(date)
  print("Overall:")
  print(summary(date))
  return(date)
}

# Read in the clean data from the analysis script:
# This is what we were working with before
# let's see how it corresponds to what we have now


dat_old <- readRDS("C:/Users/aadamson/Documents/COPD/SC Outcomes 2018/data/tidyData/clean_data_from_SCC_tot_and_BM_section_20190820.RDS")


dat <- readstata13::read.dta13("C:/Users/aadamson/Documents/COPD/SCC 2019/COPD_SCC_2019_build_modified.dta",
                                    generate.factors = TRUE)




# let's remove all the additional attributes for this data frame that has arisen because we read it in from stata



attributes(dat) <- attributes(dat)[c(2, 1, 15)]










nlc("No. audit admissions:")
nrow(dat) 

nlc("No. unique patients in audit:")
dat %>% select(patientid) %>% unique() %>% nrow()

nlc("No. admissions in the audit that shouldn't have been there because they're too young, so we get rid of them")
dat %>% filter(age < 35) %>% nrow()

dat <- dat %>% filter(age > 34)

# Old...

HES1_old <- read.csv("C:/Users/aadamson/Documents/COPD/SC Outcomes 2018/data/rawData/NIC349273_HES_APC_201799.txt",
                sep = "|", stringsAsFactors = FALSE)

# write.csv(HES1, "D:/Alex/COPD/SC Outcomes 2018/data/rawData/viewing/HES1.csv", row.names = FALSE) 

HES2_old <- read.csv("C:/Users/aadamson/Documents/COPD/SC Outcomes 2018/data/rawData/NIC349273_HES_APC_201813.txt",
                 sep = "|", stringsAsFactors = FALSE)

# write.csv(HES2, "D:/Alex/COPD/SC Outcomes 2018/data/rawData/viewing/HES2.csv", row.names = FALSE) 


ONS_old <- read.csv("C:/Users/aadamson/Documents/COPD/SC Outcomes 2018/data/rawData/NIC349273_ONS_MORTALITY.txt", 
                sep = "|", stringsAsFactors = FALSE)

ONS_old$DOD <- as.Date(ONS_old$DOD)


# New...

HES1 <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/FILE0148990_NIC349273_HES_APC_201899.txt",
                     sep = "|", stringsAsFactors = FALSE)

# write.csv(HES1, "D:/Alex/COPD/SC Outcomes 2018/data/rawData/viewing/HES1.csv", row.names = FALSE) 

HES2 <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/FILE0148991_NIC349273_HES_APC_201999.txt",
                     sep = "|", stringsAsFactors = FALSE)

# write.csv(HES2, "D:/Alex/COPD/SC Outcomes 2018/data/rawData/viewing/HES2.csv", row.names = FALSE) 


ONS1 <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/FILE0148992_NIC349273_ONS_Mortality.txt", 
                    sep = "|", stringsAsFactors = FALSE)

ONS2 <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/FILE0148993_NIC349273_HES_ONS_Mortality.txt", 
                 sep = "|", stringsAsFactors = FALSE)



ONS3 <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/FILE0154346_NIC349273_CIVREG_DEATHS_W.txt", 
                 sep = "|", stringsAsFactors = FALSE)

ONS4 <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/FILE0164402_NIC349273_CIVREG_MORT_WALES.txt", 
                 sep = "|", stringsAsFactors = FALSE)



ONS <- left_join(ONS1, ONS2, by = "ID")

ONS <- bind_rows(ONS, ONS3)


ONS4$DOD <- niceDate(ONS4$DOD)



# need to get the column names matched up

colnames(ONS4)[1:2] <- tolower(colnames(ONS4)[1:2])
colnames(ONS4)[3:5] <- toupper(colnames(ONS4)[3:5])

# and the same format

ONS4$dod <- as.character(ONS4$dod)

# and bind these in

ONS <- bind_rows(ONS, ONS4)



# select the appropriate columns
ONS <- ONS %>% select(STUDY_ID, dod, cause_of_death, MATCH_RANK)

# make sure the capitals are consistent with what they were before
colnames(ONS) <- c("study_id", "DOD", "CAUSE_OF_DEATH", "MATCH_RANK")



# old columns are: study_id, DOD, CAUSE_OF_DEATH, MATCH_RANK
# the structure is the same



ONS$DOD <- as.Date(ONS$DOD)


            

# checkSame(HES1, HES1_old) # same
# checkSame(HES2, HES2_old) # same



# Scottish data as well this time.

NRS <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/nrsdeaths-COPD-201810-201909-v104.csv", 
                sep = ",", stringsAsFactors = FALSE)

# NRS %>% select(UNDERLYING_CAUSE_OF_DEATH, CAUSE_OF_DEATH_CODE_0)

# Let's just immediately add it to ONS.



NRS <- NRS %>% select(PID, DATE_OF_DEATH, UNDERLYING_CAUSE_OF_DEATH) %>% mutate(MATCH_RANK = 99) %>%
  rename(study_id = PID, DOD = DATE_OF_DEATH, CAUSE_OF_DEATH = UNDERLYING_CAUSE_OF_DEATH) 

# DOD is in an annoying format. Need to sort it out.
# might as well create a function for this now I see I have to do it again.



NRS$DOD <- niceDate(NRS$DOD)

nlc("No. NRS records")

nrow(NRS)

ONS <- bind_rows(ONS, NRS)


nlc("Total no. ONS/NRS records:")

nrow(ONS)

# Okay - now just included within ONS. 


# And now we read in the Scottish hospital statistics


SMR <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/smr01-COPD-201810-201909-v104.csv", 
                sep = ",", stringsAsFactors = FALSE)



SMR$ADMISSION_DATE <- niceDate(SMR$ADMISSION_DATE)
SMR$DISCHARGE_DATE <- niceDate(SMR$DISCHARGE_DATE)

nlc("Remove duplicate records straight off the bat. This many duplicates:")
SMR %>% filter(duplicated(.)) %>% nrow()

# remove them

SMR <- SMR %>% filter(!duplicated(.))

nlc("Total number of SMR episodes after duplicates in all variables removed:")
nrow(SMR)



# We don't need to do any additional cleaning to the SMR data because we seem to have been given full admissions rather than episodes. 





# First step: bind HES1 and HES2 together and check that each row remains unique

HES <- rbind(HES1, HES2)

# Make the dates dates

HES$ADMIDATE <- as.Date(HES$ADMIDATE)
HES$EPISTART <- as.Date(HES$EPISTART)
HES$DISDATE <- as.Date(HES$DISDATE)


nlc("Total number of HES episodes:")
nrow(HES)

nlc("Remove duplicate records straight off the bat. This many duplicates:")
HES %>% filter(duplicated(.)) %>% nrow()

# remove them

HES <- HES %>% filter(!duplicated(.))

nlc("Total number of HES episodes after duplicates in all variables removed:")
nrow(HES)



nlc("Total number of HES admitting episodes:") 
HES %>% filter(EPIORDER == 1) %>% nrow()


nlc("This is seeing how many people have multiple first episodes on the first day:")
HES %>% filter(EPIORDER == 1) %>% group_by(STUDY_ID, ADMIDATE) %>% summarise(n = n()) %>% ungroup() %>% select(n) %>% table()


nlc("I need to use the first episode of each admission to ensure I get the admitting diagnosis correct. It also
ensures that it's the same method as with the PEDW. The downside to this is that discharge data is only available 
on the discharging episode, so I need to transfer it so that I can see it on the admitting episode as well before
I filter by admitting episodes.")


# Need to mark the discharging episode
# This creates a new variable of maxepi, which is the maximum episode of the admission, and another
# new variable of DISEPI which is a binary variable showing whether the episode is the last episode
# (discharging episode) or not.

# Because people get readmitted on the same day, or there is an error in coding etc., people can have multiple
# discharging episodes, which throws a hammer in the works. 
# To see which people have more than one episode marked as their discharging
# episode, we use the 'add_tally' command.
# wt = weight and is always used - not the name of the column, which is always named 'n' automatically


# Now, what is this command doing?
# Well...

HES <- HES %>% group_by(STUDY_ID, ADMIDATE) %>% arrange(STUDY_ID, ADMIDATE, EPIORDER) %>% # group into admissions
  mutate(maxdis = max(DISDATE)) %>%                      # Within each admission, the discharge date becomes the maximum 
                                                         # date out of all episodes 
  mutate(maxepi = max(EPIORDER)) %>%                     # Within each group, every episode is now marked with the maximum
                                                         # episode number of the admission. This means that using the 
                                                         # following line we can establish
  mutate(DISEPI = ifelse(maxepi == EPIORDER, 1, 0)) %>%  # the discharging episode (DISEPI), by seeing if the maximum episode no.
                                                         # matched the current episode
  add_tally(wt = DISEPI) %>% rename(DISEPInum = n) %>%   # We then need to tally up the discharging episodes per admission
                                                         # because sometimes people have multiple ones that need to be
                                                         # sorted out
  ungroup() %>% 
  group_by(STUDY_ID, ADMIDATE) %>% 
  arrange(STUDY_ID, ADMIDATE, desc(DISDATE)) %>%         # When people have multiple discharging episodes, we take the one
  mutate(dup_decide = row_number()) %>%                  # with the latest date (logically this makes sense) so we order
  mutate(DISEPI2 = ifelse(DISEPInum == 1, DISEPI,        # within each group by latest discharge date using the row number
                   ifelse(dup_decide == 1, 1, 0))) %>%   # command, and then use two ifelse statements to say 'if only one 
  ungroup() %>% arrange(STUDY_ID, ADMIDATE, EPIORDER)    # discharging episode you're fine! keep the discharging episode
                                                         # you've been assigned!'. If there are multiple discharging episodes
                                                         # only the episode with the latest discharge date is assigned as the
                                                         # discharging episode. If there are two that are identical then...
                                                         # there's no way to chose which is actually the correct one so it's
                                                         # just random.

# Once that's out the way, we can create these three 'real' discharging variables that can be applied to everyone within
# one episode because there is only one episode within each episode where DISEPI = 1.

HES <- HES %>% group_by(STUDY_ID, ADMIDATE) %>%
  mutate(DISDATEREAL = DISDATE[DISEPI2 == 1]) %>% 
  mutate(DISDESTREAL = DISDEST[DISEPI2 == 1]) %>%
  mutate(DISMETHREAL = DISMETH[DISEPI2 == 1]) %>%
  ungroup()


# And then, we can filter out HES by admitting episode!

HES <- HES %>% filter(EPIORDER == 1)


# And now, to avoid messing things up (but check this) we need to replace what we had there before
# with what we obtained from the discharging episode




HES$DISDATE <- HES$DISDATEREAL
HES$DISDEST <- HES$DISDESTREAL
HES$DISMETH <- HES$DISMETHREAL

# And then remove the 'real' columns

HES <- HES %>% select(-DISDATEREAL, DISDESTREAL, -DISMETHREAL)


# sorted by discharge now, so don't really need to filter by admidate.

#


# old:
PEDW_old <- readRDS("C:/Users/aadamson/Documents/COPD/SC Outcomes 2018/data/rawData/COPD_outcomes_PEDW_2018_30266_no_pw_v2_RDS_format.RDS")

# new:

PEDW <- read.csv("C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/rawData/Request_31488_NACAP_COPD_Cohort1.csv")

PEDW_compare <- checkSame(PEDW, PEDW_old)


# PEDW <- PEDW %>% select(-Cohort, -PseudoPatientId, -LSOA)

# PEDW_compare %>% filter(in_new == 1 & in_both == 0)
# PEDW_compare %>% filter(in_old == 1 & in_both == 0)

# Mapping across:

# Cohort - drop
# PatientID = STUDYID1
# PseudoPatientId = drop
# ConsultantEpisodeID = drop
# LSOA = drop
# CalendarYear = drop
# Month = drop
# PatientAge = drop
# AdmissionMethodCode = AdmissionMethodDerivedCode
# AdmissionMethodDescription = AdmissionMethodDerivedDescription
# EthnicCode = drop
# EthnicDescription = drop
# LocalHealthBoardCode..Residence.  = drop
#  LocalHealthoard.Residence. = drop
# LocalHealthBoardCode.Provider. = drop
#  LocalHealthBoard..Provider. = drop
#  TreatmentSiteCode = drop
# TreatmentSiteName = drop

# EpisodeEndDate  ?
# HospitalSpellNo_a

# summary(PEDW_old$EpisodeEndDate)
# summary(PEDW_old$HospitalSpellNo_a)



PEDW <- PEDW %>% select(-Cohort, -PseudoPatientId, -ConsultantEpisodeID, -LSOA, -CalendarYear, -Month, -PatientAge,
                        -EthnicCode, -EthnicDescription, -LocalHealthBoardCode..Residence., -LocalHealthoard.Residence.,
                        -LocalHealthBoardCode.Provider., -LocalHealthBoard..Provider., -TreatmentSiteCode, -TreatmentSiteName)

PEDW <- PEDW %>% rename(STUDYID1 = PatientID, AdmissionMethodDerivedCode = AdmissionMethodCode, 
                        AdmissionMethodDerivedDescription = AdmissionMethodDescription)




PEDW$AdmissionDate <- as.Date(PEDW$AdmissionDate, format = "%d/%m/%Y")
PEDW$DischargeDate <- as.Date(PEDW$DischargeDate, format = "%d/%m/%Y")





nlc("Total episodes in the PEDW data:")
nrow(PEDW)

nlc("Remove duplicate records straight off the bat. This many duplicates:")
PEDW %>% filter(duplicated(.)) %>% nrow()

# remove them

PEDW <- PEDW %>% filter(!duplicated(.))

nlc("Total number of PEDW episodes after duplicates in all variables removed:")
nrow(PEDW)

nlc("Total admitting episodes in the PEDW data:")
PEDW %>% filter(CountAdmittingEpisode == 1) %>% nrow()




# We go by admitting episodes for PEDW because all the data is attached to it.
# We go by final episode for HES because this is the one with the discharge data.
# nlc("Number of admitting episodes after those outside the date range are dropped:")


PEDW <- PEDW %>% filter(CountAdmittingEpisode == 1) 


datid <- dat %>% select(patientid) %>% unique()
HESid <- HES %>% select(STUDY_ID) %>% unique()
PEDWid <- PEDW %>% select(STUDYID1) %>% unique()
SMRid <- SMR %>% select(PID) %>% unique()

length(unique(c(PEDW$STUDYID1, HES$STUDY_ID, SMR$PID)))


HESPEDWSMRids <- unique(c(HESid$STUDY_ID, PEDWid$STUDYID1, SMRid$PID))

# People with a DISDEST code of 99 or 98 usually have it because their spell isn't finished (especially '98'), so should 
# always take this value from the last episode

nlc("Total unique patients in PEDW/HES:")
PEDW %>% filter(CountAdmittingEpisode == 1) %>% nrow()




nlc("Number of index admissions from the audit data (same as no. unique IDs):")
dat %>% arrange(patientid, q4_1aadmissiondate, q4_1badmissiontime) %>% group_by(patientid)  %>% slice(1) %>% nrow()

# Filter data so it's only index admissions, and create an indexadmission flag

str(dat$q4_1aadmissiondate)
str(dat$q4_1badmissiontime)

dat <- dat %>% arrange(patientid, q4_1aadmissiondate, q4_1badmissiontime) %>% group_by(patientid)  %>% slice(1) %>% mutate(indexadflag = 1) %>% ungroup()
dat %>% filter(indexadflag == 1) %>% nrow()
nlc("Only index admissions from the audit data kept from this point on.")




nlc("Drop any patients with IDs that do not match IDs in HES or PEDW, as these are impossible ot be matched.
Number of patients to be dropped:")


dat %>% filter(!(patientid %in% HESPEDWSMRids)) %>% nrow()



HESPEDWSMRids %in% dat$patientid %>% sum()

summary(dat$country)
nrow(dat)

dat <- dat %>% filter(patientid %in% HESPEDWSMRids)
nrow(dat)

nlc("Our new audit population is:")
nrow(dat)

length(unique(dat$patientid))


nlc("
# Likewise, we should remove all patients from HES PEDW who are not present in the audit. They might not be
# present because they were cleaned at an earlier stage from the audit etc.

HES records (bear in mind this is in terms of admissions) to be removed:")
HES %>% filter(!(STUDY_ID %in% dat$patientid)) %>% nrow()

HES <- HES %>% filter(STUDY_ID %in% dat$patientid)




nlc("New HES admissions size:")
nrow(HES)


nlc("And we do the same with PEDW:
Admissions removed:")

PEDW %>% filter(!(STUDYID1 %in% dat$patientid)) %>% nrow()


PEDW <- PEDW %>% filter(STUDYID1 %in% dat$patientid)

nlc("New PEDW admissions size:")
nrow(PEDW)


# and for SMR as well

nlc("And we do the same with SMR:
Admissions removed:")

SMR %>% filter(!(PID %in% dat$patientid)) %>% nrow()


SMR <- SMR %>% filter(PID %in% dat$patientid)

nlc("New SMR admissions size:")
nrow(SMR)


# Now we have to attempt to merge these index admissions with the PEDW data...

# make sure the names match

PEDW <- rename(PEDW, patientid = STUDYID1)
dat <- rename(dat, AdmissionDate = q4_1aadmissiondate)
dat <- rename(dat, DischargeDate = q10_2dischargedate)
HES <- rename(HES, patientid = STUDY_ID)
HES <- rename(HES, AdmissionDate = ADMIDATE)
HES <- rename(HES, DischargeDate = DISDATE)


# str(PEDW)
# str(dat)
# str(HES)

# Change the discharge date values of 01/01/1800 or 01/01/1801 to missing


nlc("Some cleaning: how many people in HES have discharge dates of 1800-01-01?:")
HES %>% filter(DischargeDate == "1800-01-01") %>% nrow()

nlc("Some cleaning: how many people in HES have discharge dates of 1801-01-01?:")
HES %>% filter(DischargeDate == "1801-01-01") %>% nrow()

nlc("Is this everyone who is discharged before arrival?:")
HES %>% filter(DischargeDate - AdmissionDate < 0) %>% nrow()
nlc("Yes.")

nlc("Need to recode these people as having a missing discharge date.")
HES %>% filter(DischargeDate == "1801-01-01") %>% nrow()


HES$DischargeDate[(HES$DischargeDate - HES$AdmissionDate) < 0] <- NA



# Okay. The name has been changed from here.



#----------------------------------#
# Let's align the two datasets. Rename the HES data in line with the PEDW data
# Apart from, keep DIAG instead of diagnosis...

# HES <- HES %>% select(AdmissionDate = ADMIDATE)
                      

#------------------------------------------------------#
#                                                      #
# SORTING OUT DIFFERENT DISCHARGE DESTINATIONS: PEDW   #
#                                                      #
#------------------------------------------------------#


# FOR PEDW!!!! (Codes are different)

# we should say if you're admitted to hospital the same day after you're discharged to another LHB, or one day
# afterwards, it doesn't count as a readmission. 
# So we need to get rid of the readmissions that are down to simply being discharged specifically to another hospital.
# Codes used are here: http://www.datadictionary.wales.nhs.uk/#!WordDocuments/dischargedestination.htm

D2HcodesPEDW <- c(51, 52, 53, 55, 56, 57)

nlc("PEDW people discharged to another hospital:")
PEDW %>% filter(DischargeDestinationCode %in% D2HcodesPEDW) %>% nrow()


# Create marker so we know who they are.

PEDW <- PEDW %>% mutate(dis2hosp = ifelse(DischargeDestinationCode %in% D2HcodesPEDW, 1, 0))
# table(PEDW$dis2hosp)
# Now we want to mark admissions that occur because of this sort the data 

# PEDW %>% filter(patientid %in% (filter(PEDW, dis2hosp == 1))$patientid) %>%
#          select(patientid, AdmissionDate, DischargeDate, AdmissionMethodDerivedDescription, 
#                 DischargeDestinationDescription, AdmissionMethodDerivedCode) %>%
#          arrange(patientid, AdmissionDate, DischargeDate)


PEDW %>% filter(DischargeDestinationCode %in% D2HcodesPEDW) %>% select(patientid) -> LHBdischarge



# All need to be in this order

PEDW <- PEDW %>% arrange(patientid, AdmissionDate, DischargeDate)

# This command matches the admission date of the patient to the discharge date in the row above (+/- 1). If the two match, and the patientid matches, and
# the discharge code says that the patient has been discharged to hospital, then 'IgnoreReadmit' is marked as 1, otherwise it is marked as 0 and counted
# as a new admission.

PEDW <- PEDW %>% mutate(., IgnoreReadmit = ifelse((lag(.$DischargeDate) == .$AdmissionDate | lag(.$DischargeDate) == (.$AdmissionDate + 1) |
                                                   lag(.$DischargeDate) == (.$AdmissionDate - 1)) &
                                                  lag(.$patientid) == patientid &
                                                  lag(.$DischargeDestinationCode) %in% D2HcodesPEDW, 1, 0))

#  %>% filter(DischargeDestinationCode %in% D2HcodesPEDW) %>% nrow()

nlc("No. people my code has captured who were readmitted as part of a hospital transfer:")
PEDW %>% filter(IgnoreReadmit == 1) %>% nrow()
nrow(PEDW)



#------------------------------------------------------#
#                                                      #
# SORTING OUT DIFFERENT DISCHARGE DESTINATIONS: HES    #
#                                                      #
#------------------------------------------------------#


# Now we do the same thing with HES data:

D2HcodesHES <- c(48, 49, 50, 51, 52, 53)

nlc("HES people discharged to another hospital:")

HES %>% filter(DISDEST %in% D2HcodesHES) %>% nrow()


# Create marker so we know who they are.

HES <- HES %>% mutate(dis2hosp = ifelse(DISDEST %in% D2HcodesHES, 1, 0))

# Now we want to mark admissions that occur because of this sort the data 

# HES %>% filter(patientid %in% (filter(HES, dis2hosp == 1))$patientid) %>%
#   select(patientid, AdmissionDate, DischargeDate, DISDEST) %>%
#   arrange(patientid, AdmissionDate, DischargeDate)


HES %>% filter(DISDEST %in% D2HcodesHES) %>% select(patientid) -> LHBdischarge



# All need to be in this order

HES <- HES %>% arrange(patientid, AdmissionDate, DischargeDate)

# This command matches the admission date of the patient to the discharge date in the row above (+/- 1). If the two match, and the patientid matches, and
# the discharge code says that the patient has been discharged to hospital, then 'IgnoreReadmit' is marked as 1, otherwise it is marked as 0 and counted
# as a new admission.

HES <- HES %>% mutate(., IgnoreReadmit = ifelse((lag(.$DischargeDate) == .$AdmissionDate | lag(.$DischargeDate) == (.$AdmissionDate + 1) |
                                                     lag(.$DischargeDate) == (.$AdmissionDate - 1)) &
                                                    lag(.$patientid) == patientid &
                                                    lag(.$DISDEST) %in% D2HcodesHES, 1, 0))


# HES %>% filter(DISDEST %in% D2HcodesHES) %>% nrow()

nlc("No. people my code has captured who were readmitted as part of a hospital transfer:")
HES %>% filter(IgnoreReadmit == 1) %>% nrow()






#------------------------------------------------------#
#                                                      #
# SORTING OUT DIFFERENT DISCHARGE DESTINATIONS: SMR    #
#                                                      #
#------------------------------------------------------#


# Now we do the same thing with SMR data:

D2HcodesSMR <- c(12, 13)

nlc("SMR people discharged to another hospital:")

head(SMR)

SMR %>% filter(DISCHARGE_TYPE %in% D2HcodesSMR) %>% nrow()

# 3216 people are discharged to another hospital. Not fair to include them as readmitted.
# Create marker so we know who they are.

SMR <- SMR %>% mutate(dis2hosp = ifelse(DISCHARGE_TYPE %in% D2HcodesSMR, 1, 0))

# Now we want to mark admissions that occur because of this sort the data 

# SMR %>% filter(PID %in% (filter(SMR, dis2hosp == 1))$PID) %>%
#   select(PID, ADMISSION_DATE, DISCHARGE_DATE, DISCHARGE_TYPE) %>%
#   arrange(PID, ADMISSION_DATE, DISCHARGE_DATE)


SMR %>% filter(DISCHARGE_TYPE %in% D2HcodesSMR) %>% select(PID) -> LHBdischarge

# Lag!!!!!! What a great function!!!!!!!! 

# All need to be in this order

SMR <- SMR %>% arrange(PID, ADMISSION_DATE, DISCHARGE_DATE)


SMR %>% select(PID, ADMISSION_DATE, ADMISSION_TYPE, DISCHARGE_DATE, DISCHARGE_TYPE, dis2hosp) %>% head()



# This command matcSMR the admission date of the patient to the discharge date in the row above (+/- 1). If the two match, and the PID matcSMR, and
# the discharge code says that the patient has been discharged to hospital, then 'IgnoreReadmit' is marked as 1, otherwise it is marked as 0 and counted
# as a new admission.

SMR <- SMR %>% mutate(., IgnoreReadmit = ifelse((lag(.$DISCHARGE_DATE) == .$ADMISSION_DATE | lag(.$DISCHARGE_DATE) == (.$ADMISSION_DATE + 1) |
                                                   lag(.$DISCHARGE_DATE) == (.$ADMISSION_DATE - 1)) &
                                                  lag(.$PID) == PID &
                                                  lag(.$DISCHARGE_TYPE) %in% D2HcodesSMR, 1, 0))


# SMR %>% filter(DISCHARGE_TYPE %in% D2HcodesSMR) %>% nrow()

nlc("No. people my code has captured who were readmitted as part of a hospital transfer:")
SMR %>% filter(IgnoreReadmit == 1) %>% nrow()

SMR %>% filter(IgnoreReadmit == 1) %>% select(ADMISSION_TYPE) %>% table()




# SMR$check[SMR$IgnoreReadmit == 1 & SMR$ADMISSION_TYPE != 18] <- 1

# ----------------------------#


# notuniqueHPALL <- HES %>% select(patientid, AdmissionDate, DischargeDate) %>% filter(duplicated(.) == TRUE) %>% 
#   mutate(notunique = 1)
# 
# HPALLtest <- left_join(HES, notuniqueHPALL)
# HPALLtest$notunique[is.na(HPALLtest$notunique)] <- 0
# 
# HPALLtest %>% arrange(patientid, AdmissionDate, DischargeDate) %>% filter(notunique == 1) %>% CP()

# These are all the non-unique people - just looks like duplicates or mis-coding.
# For my purposes, they are identical, so we just need to drop 1 of them.

table(SMR$DISCHARGE_TYPE)
table(SMR$DISCHARGE_TRANSFER_TO)

checkSame(PEDW, PEDW_old)
# Now we need to make HES and PEDW the same, so we can bind them together

colnames(PEDW) <- gsub("Diagnosis", "DIAG_", colnames(PEDW))
PEDW <- PEDW %>% select(patientid,
                        AdmissionDate,
                        DischargeDate,
                        DIAG_01:DIAG_14,
                        DISDEST = DischargeDestinationCode,
                        dis2hosp,
                        IgnoreReadmit) %>% 
                  mutate(DIAG_15 = NA,
                         DIAG_16 = NA,
                         DIAG_17 = NA,
                         DIAG_18 = NA,
                         DIAG_19 = NA,
                         DIAG_20 = NA)
                        

HES <- HES %>% select(patientid,
                      AdmissionDate,
                      DischargeDate,
                      DIAG_01:DIAG_20,
                      DISDEST,
                      dis2hosp,
                      IgnoreReadmit)

# rename the SMR variables.

SMR <- SMR %>% rename(patientid = PID, AdmissionDate = ADMISSION_DATE, DischargeDate = DISCHARGE_DATE,
                      DIAG_01 = MAIN_CONDITION, DIAG_02 = OTHER_CONDITION_1, DIAG_03 = OTHER_CONDITION_2,
                      DIAG_04 = OTHER_CONDITION_3, DIAG_05 = OTHER_CONDITION_4, DIAG_06 = OTHER_CONDITION_5,
                      DISDEST = DISCHARGE_TRANSFER_TO)   # note, this variable isn't necessarily usable because codes could be different


# Select the ones we need

SMR <- SMR %>% select(patientid,
                      AdmissionDate,
                      DischargeDate,
                      DIAG_01:DIAG_06,
                      DISDEST,
                      dis2hosp,
                      IgnoreReadmit) %>% 
  mutate(DIAG_07 = NA,
         DIAG_08 = NA,
         DIAG_09 = NA,
         DIAG_10 = NA,
         DIAG_11 = NA,
         DIAG_12 = NA,
         DIAG_13 = NA,
         DIAG_14 = NA,
         DIAG_15 = NA,
         DIAG_16 = NA,
         DIAG_17 = NA,
         DIAG_18 = NA,
         DIAG_19 = NA,
         DIAG_20 = NA)               


# to bind them together we need to make disdest the same - easiest way to do this is to make it all character.
# also, I'm going to include a variable saying what dataset they came from.

HES$DISDEST <- as.character(HES$DISDEST)
PEDW$DISDEST <- as.character(PEDW$DISDEST)
SMR$DISDEST <- as.character(SMR$DISDEST) # note - this should already be character.

HES$dataset <- "HES"
PEDW$dataset <- "PEDW"
SMR$dataset <- "SMR"

ONS <- rename(ONS, patientid = study_id)

# head(HES)
# head(PEDW)
# colnames(HES)

str(PEDW)
str(HES)
str(SMR)
table(SMR$DISDEST)
# str(PEDW)


HPALL <- bind_rows(HES, PEDW, SMR) %>% arrange(patientid, AdmissionDate, DischargeDate)

nlc("Bind PEDW, SMR and HES datasets together - how many rows is this now:")
nrow(HPALL)



# Now, finally, we need to convert the diagnosis codes to three characters so that we can include them in 
# the top 5 reasons for things.


HPALL <- HPALL %>% mutate_at(vars(contains("DIAG_")), .funs = funs(ifelse(. == "", NA, .)))
ONS <- ONS %>% mutate_at(vars(CAUSE_OF_DEATH), .funs = funs(ifelse(. == "", "Code missing", .)))



nlc("Number of rows in the combined death data:")
nrow(ONS) %>% nlc()

nlc("Number of unique patients:")
unique(ONS$patientid) %>% length() %>% nlc()


nlc("Get rid of duplicates...")


ONS <- ONS %>% arrange(patientid, DOD, CAUSE_OF_DEATH, MATCH_RANK) %>% 
  group_by(patientid) %>% slice(1) %>% ungroup()


nlc("Now we have this many people:")
nrow(ONS) %>% nlc()





# HPALL %>% select(patientid, AdmissionDate, DischargeDate) %>% nrow() 

nlc("How many people are non-unique based on patient ID, admission date, and discharge date?")

HPALL %>% select(patientid, AdmissionDate, DischargeDate) %>% filter(duplicated(.) == TRUE) %>% nrow()

HPALL %>% select(patientid, AdmissionDate, DischargeDate, dataset) %>% filter(duplicated(.) == TRUE) %>% select(dataset) %>% pull() %>% table()




nlc("
# These duplicates just need to be removed. After investigation, it looks like they are a miscoding of an episode,
# as arrival and discharge dates are the same despite being on different days, which wouldn't be possible if it was a true
# readmission.")

 

# non-unique IDs:

notuniqueHPALL <- HPALL %>% select(patientid, AdmissionDate, DischargeDate) %>% filter(duplicated(.) == TRUE) %>% 
                  mutate(notunique = 1)

HPALL <- left_join(HPALL, notuniqueHPALL)
HPALL$notunique[is.na(HPALL$notunique)] <- 0

nlc("
# total admissions where patient ID, Admission date, and discharge date are all the same (so divide by 2 for
# patient numbers:")

HPALL %>% arrange(patientid, AdmissionDate, DischargeDate) %>% filter(notunique == 1) %>% nrow()

nlc("# Of these, truly unique values for every variable still in this dataset is:")
HPALL %>% arrange(patientid, AdmissionDate, DischargeDate) %>% filter(notunique == 1) %>% unique() %>% nrow()


nlc("This shows that a large proportion are true duplicates regardless. So, we get rid of them.")

dupremove <- HPALL %>% arrange(patientid, AdmissionDate, DischargeDate) %>% filter(notunique == 1) %>%
  group_by(patientid, AdmissionDate, DischargeDate) %>% slice(1) %>% ungroup()


nlc("Total number removed is:")
(HPALL %>% arrange(patientid, AdmissionDate, DischargeDate) %>% filter(notunique == 1) %>% nrow()) -
  nrow(dupremove)

HPALL <- HPALL %>% filter(notunique == 0) %>% bind_rows(dupremove)

nlc("This leaves this many admissions remaining:")
nrow(HPALL)


#----------------------
# Now we start matching
#----------------------

# Create a miniature audit dataset only containing the variables we want to match on:

dat.mini <- dat

dat.mini <- dat.mini %>% select(patientid, AdmissionDate, DischargeDate)



# str(unique(dat))
# str(dat)
# head(dat)

# Do the same with unique the HES/PEDW dataset

matchtypeboth <- HPALL %>% select(patientid, AdmissionDate, DischargeDate)

# Create a new column so we know when it's been matched

matchtypeboth$matchboth <- 1

# Then match the HES/PEDW to the audit
dat.mini <- left_join(dat.mini, matchtypeboth, by = c("patientid", "AdmissionDate", "DischargeDate"))

nlc("How many people match on patient ID, date, and time, with no issues?")
sum(dat.mini$matchboth, na.rm = TRUE)

# Now we see how many people match on patient ID and admission:
matchtypeadmiss <- HPALL %>% select(patientid, AdmissionDate) %>% unique()

matchtypeadmiss$matchadmiss <- 1

dat.mini <- left_join(dat.mini, matchtypeadmiss, by = c("patientid", "AdmissionDate"))

# And on discharge:
matchtypedis <- HPALL %>% select(patientid, DischargeDate) %>% unique()

matchtypedis$matchdis <- 1

dat.mini <- left_join(dat.mini, matchtypedis, by = c("patientid", "DischargeDate"))


# Let's look at the missing data pattern to see if we can see who matches on admission, discharge and both
library(mice)
library(dplyr)

# we don't care about missing discharge dates for this

nlc("This is the matching breakdown:")
md.pattern(dplyr::select(dat.mini, -DischargeDate))

nlc("No. matched on patient ID, admission date, and discharge date:")
as.numeric(row.names(md.pattern(select(dat.mini, -DischargeDate)))[1])
nlc("No. matched on patient ID and admission date but not discharge date:")
as.numeric(row.names(md.pattern(select(dat.mini, -DischargeDate)))[3])
nlc("No. matched on patient ID and discharge date but not admission date:")
as.numeric(row.names(md.pattern(select(dat.mini, -DischargeDate)))[4])
nlc("No. matched on patient ID, admission date only, discharge date only, but not both together(!):")
as.numeric(row.names(md.pattern(select(dat.mini, -DischargeDate)))[2])
nlc("No. that remain unmatched:")
as.numeric(row.names(md.pattern(select(dat.mini, -DischargeDate)))[5])



weird <- dat.mini %>% filter(matchdis == 1 & matchadmiss == 1 & is.na(matchboth))


# Matching in admission and discharge but not when done at the same time happens when a discharge has been missed (so there are
# two separate admissions in PEDW but only a single admission in the audit.


# Now... how do we get it all together?!?!?!?!?!?!?

# could subset based on those who aren't in a particular match type when we join...
# but when we do that, we're going to still get the NAs appearing...

# Might need to rebuild the dataset which could be a bit of a nightmare...
# Let's remember though, we're only using this to identify the PEDW index admissions, right?

# Once we've identified the PEDW data index admission, we can just link the audit data by patient ID - don't need to link it
# by anything else.


# Now that we've found the matches, combine/mark this on the full dataset
# Those matching on both:

datboth <- left_join(HPALL, mutate(dat.mini, matchtype = 1), by = c("patientid", "AdmissionDate", "DischargeDate"))
datboth <- select(datboth, -matchboth, -matchadmiss, -matchdis)

datboth %>% group_by(patientid) %>% slice(1) %>% nrow()

# Those matching on Admission

# Only join if not yet matched. 'match type' remains in the datboth dataset, and we change it NA to 2 if the patient
# wasn't matched before but is now.

# If we're matching only on admissions, then if people get admitted twice in the same day they're going to get matched twice.
# Five people have this issue.
# dups.list <- datboth %>% filter(!is.na(matchtype)) %>% select(patientid) %>% filter(duplicated(.) == TRUE)
# DON'T PANIC THAT THIS IS DIFFERENT TO THE MICE MISSING DATA PATTERN!!!!

datboth <- left_join(datboth, rename(dat.mini, DischargeDateAudit = DischargeDate), by = c("patientid", "AdmissionDate"))
head(datboth)

datboth$matchtype[is.na(datboth$matchtype) & datboth$matchadmiss == 1] <- 2

table(datboth$matchtype, useNA = "ifany")
table(datboth$matchtype, datboth$matchadmiss, useNA = "ifany")

datboth <- select(datboth, -matchboth, -matchadmiss, -matchdis)

# Then we do the same thing for those matching on discharge, but we change it to 3
# Over-matching will be more of an issue due to missing discharge dates...
# Those matching on Discharge


datboth <- left_join(datboth, rename(dat.mini, AdmissionDateAudit = AdmissionDate), by = c("patientid", "DischargeDate"))

datboth$matchtype[is.na(datboth$matchtype) & datboth$matchdis == 1] <- 3

table(datboth$matchtype, useNA = "ifany")
table(datboth$matchtype, datboth$matchdis, useNA = "ifany")

datboth <- select(datboth, -matchboth, -matchadmiss, -matchdis)






dat.minier <- dat.mini %>% filter(is.na(matchboth) & is.na(matchadmiss) & is.na(matchdis))

# summary(dat.minier)

dat.minier <- dat.minier %>% select(-matchboth, -matchadmiss, -matchdis) %>% mutate(match3day = 1)

# summary(dat.minier$match3day)


nlc("Now we give people the benefit of the doubt, and see if any people who are one day out either on their
admission or their discharge get added. Note: originally this was up to 3 days, but it only added an extra 
XXX people and was thought to interfere too much with potential readmissions.")



# +1
datboth <- dat.minier %>% rename(DischargeDateAudit3day = DischargeDate) %>% mutate(AdmissionDateAudit3day = AdmissionDate) %>%
  mutate(AdmissionDate = AdmissionDate + 1) %>%
  left_join(datboth, ., by = c("patientid", "AdmissionDate"))

datboth$matchtype[is.na(datboth$matchtype) & datboth$match3day == 1] <- 4

datboth$AdmissionDateAudit[!is.na(datboth$AdmissionDateAudit3day)] <- 
  datboth$AdmissionDateAudit3day[!is.na(datboth$AdmissionDateAudit3day)]
datboth$DischargeDateAudit[!is.na(datboth$DischargeDateAudit3day)] <- 
  datboth$DischargeDateAudit3day[!is.na(datboth$DischargeDateAudit3day)]


datboth <- select(datboth, -match3day, -AdmissionDateAudit3day, -DischargeDateAudit3day)

nlc("Number matched on (audit) admission date + 1:")

as.numeric(table(datboth$matchtype, useNA = "ifany")[4])



# -1
datboth <- dat.minier %>% rename(DischargeDateAudit3day = DischargeDate) %>% mutate(AdmissionDateAudit3day = AdmissionDate) %>%
  mutate(AdmissionDate = AdmissionDate - 1) %>%
  left_join(datboth, ., by = c("patientid", "AdmissionDate"))

datboth$matchtype[is.na(datboth$matchtype) & datboth$match3day == 1] <- 4

datboth$AdmissionDateAudit[!is.na(datboth$AdmissionDateAudit3day)] <- 
  datboth$AdmissionDateAudit3day[!is.na(datboth$AdmissionDateAudit3day)]
datboth$DischargeDateAudit[!is.na(datboth$DischargeDateAudit3day)] <- 
  datboth$DischargeDateAudit3day[!is.na(datboth$DischargeDateAudit3day)]

datboth <- select(datboth, -match3day, -AdmissionDateAudit3day, -DischargeDateAudit3day)

nlc("Number matched on (audit) admission date + 1 and -1:")

as.numeric(table(datboth$matchtype, useNA = "ifany")[4])


# Aaaaaand for Discharge discrepancies...

# +1
datboth <- dat.minier %>% rename(AdmissionDateAudit3day = AdmissionDate) %>% mutate(DischargeDateAudit3day = DischargeDate) %>%
  mutate(DischargeDate = DischargeDate + 1) %>%
  left_join(datboth, ., by = c("patientid", "DischargeDate"))

datboth$matchtype[is.na(datboth$matchtype) & datboth$match3day == 1] <- 4

datboth$AdmissionDateAudit[!is.na(datboth$AdmissionDateAudit3day)] <- 
  datboth$AdmissionDateAudit3day[!is.na(datboth$AdmissionDateAudit3day)]
datboth$DischargeDateAudit[!is.na(datboth$DischargeDateAudit3day)] <- 
  datboth$DischargeDateAudit3day[!is.na(datboth$DischargeDateAudit3day)]

datboth <- select(datboth, -match3day, -AdmissionDateAudit3day, -DischargeDateAudit3day)

nlc("Number matched on (audit) admission date +1 and -1 and discharge date +1:")

as.numeric(table(datboth$matchtype, useNA = "ifany")[4])




# -1
datboth <- dat.minier %>% rename(AdmissionDateAudit3day = AdmissionDate) %>% mutate(DischargeDateAudit3day = DischargeDate) %>%
  mutate(DischargeDate = DischargeDate - 1) %>%
  left_join(datboth, ., by = c("patientid", "DischargeDate"))

datboth$matchtype[is.na(datboth$matchtype) & datboth$match3day == 1] <- 4

datboth$AdmissionDateAudit[!is.na(datboth$AdmissionDateAudit3day)] <- 
  datboth$AdmissionDateAudit3day[!is.na(datboth$AdmissionDateAudit3day)]
datboth$DischargeDateAudit[!is.na(datboth$DischargeDateAudit3day)] <- 
  datboth$DischargeDateAudit3day[!is.na(datboth$DischargeDateAudit3day)]

datboth <- select(datboth, -match3day, -AdmissionDateAudit3day, -DischargeDateAudit3day)

nlc("Number matched on (audit) admission date +1 and -1 and discharge date +1 and -1:")

as.numeric(table(datboth$matchtype, useNA = "ifany")[4])




nlc("Total matching: 1 = both, 2 = admission, 3 = discharge, 4 = admission/discharge plus/minus 1 day:")

datboth %>% select(matchtype) %>% table(useNA = "ifany")

# Let's just have a little look...

matchcheck <- left_join(dat.minier, unique(select(HPALL, patientid, AdmissionDate, DischargeDate)), by = "patientid")

# ended up with multiple because of course, patients are admitted multiple times and we are not sure which one to link with...

matchcheck$addiscrep <- matchcheck$AdmissionDate.x - matchcheck$AdmissionDate.y
matchcheck$disdiscrep <- matchcheck$DischargeDate.x - matchcheck$DischargeDate.y

# Used the absolute funciton here because specifying that a value lay between two other values using & didn't work for some reason.
matchcheck$matched <- 0
matchcheck$matched[abs(matchcheck$addiscrep) < 2] <- 1
matchcheck$matched[abs(matchcheck$disdiscrep) < 2] <- 1

# # We create an index admission flag for the PEDW data. Anything with a match type is an index admission.



datboth$indexadmission <- 0
datboth$indexadmission[!is.na(datboth$matchtype)] <- 1
table(datboth$indexadmission, datboth$matchtype, useNA = "ifany")


# Use the add_tally function to tally up the number of index admissions

datboth <- datboth %>% group_by(patientid) %>% add_tally(wt = indexadmission) %>% rename(reps = n) %>% ungroup()

nlc("This table shows us how many times each audit patient matches for an index admission (1 is what we're after):")
table(datboth$reps[datboth$indexadmission == 1], useNA = "ifany")

nlc("Patients who are well-matched or not matched:")
datbothfine <- filter(datboth, reps %in% c(0, 1))
datbothfine %>% select(patientid) %>% unique() %>% nrow()


datdup <- filter(datboth, reps > 1)

datdupnotindex <- filter(datdup, indexadmission != 1)
datdupindex <- filter(datdup, indexadmission == 1)

nrow(datdupindex)

datdupindex <- datdupindex %>% group_by(patientid) %>% arrange(patientid, matchtype, AdmissionDate, DischargeDate) %>%
               slice(1) %>%  ungroup()

nlc("Duplicated patients who remain after sorting out duplicates:")
nrow(datdupindex)




# now we simply row bind everything back together!!!!

datboth <- bind_rows(datbothfine, datdupnotindex, datdupindex)

rm(datbothfine)
rm(datdupnotindex)
rm(datdupindex)


table(datboth$reps)




nlc("Now - how many patients remain unmatched?")

datboth %>% filter(reps == 0) %>% group_by(patientid) %>% slice(1) %>% nrow()




nlc("These patients must be removed to leave this many patients in the final dataset!:")

datboth <- filter(datboth, reps != 0)
datboth %>% filter(indexadmission == 1) %>% nrow()



# We remove the AdmissionDateAudit/DischargeDateAudit variable used in the matching process to find index admissions,
# and just use the one that is present from the audit dataset now that we are putting it all together simply by patientid.

colnames(datboth)

datboth <- select(datboth, -AdmissionDateAudit, -DischargeDateAudit)


datboth %>% group_by(patientid) %>% add_tally(wt = indexadmission) %>% filter(n > 1) %>% 
  select(patientid, AdmissionDate, DischargeDate, IgnoreReadmit, matchtype, indexadmission) %>% print(n=200)

datboth %>% group_by(patientid) %>% add_tally(wt = indexadmission) %>% ungroup() %>% select(n) %>% table() 
#  select(patientid, AdmissionDate, DischargeDate, IgnoreReadmit, matchtype, indexadmission) %>% print(n=200)

nrow(datboth)

# Everyone just has one index admission!!!! 

dat <- rename(dat, AdmissionDateAudit = AdmissionDate, DischargeDateAudit = DischargeDate)
dat <- select(dat, -indexadflag)

datboth <- left_join(datboth, dat, by = "patientid")


datboth %>% group_by(patientid) %>% add_tally(wt = indexadmission) %>% ungroup() %>% select(n) %>% table() 


# And then, add death data
# first create a 'died in dataset' flag

nlc("How many ONS records do we have?")
nrow(ONS)

# nlc("How many ONS records do we have after filtering out those that occurred outside of our study time period?")



ONS$ONSdeath <- 1


nrow(datboth)

datboth <- left_join(datboth, ONS, by = "patientid")

nrow(datboth)

# Now it's all cool now I've sorted out the ONS duplicates.


datboth$ONSdeath[is.na(datboth$ONSdeath) == TRUE] <- 0




datboth %>% group_by(patientid) %>% add_tally(wt = indexadmission) %>% ungroup() %>% select(n) %>% table() 


nlc("How many of these ultimately match? (Non-matches aren't an issue - unmatched records have been filtered out
earlier on)")

datboth %>% filter(indexadmission == 1) %>% filter(ONSdeath == 1) %>% nrow() 

# And now, we need to drop all hospital admissions that occured before the index admission:
# Here, very usefully, mutate takes into account the grouping beforehand.


# Anyway, first of all we apply the index admission date to all of the patient's admissions:

datboth %>% group_by(patientid) %>% add_tally(wt = indexadmission) %>% filter(n > 1) %>% 
  select(patientid, AdmissionDate, DischargeDate, AdmissionDateAudit, DischargeDateAudit, IgnoreReadmit, matchtype, indexadmission) %>% print(n=200)

datboth %>% select(patientid) %>% unique() %>% nrow()
sum(datboth$indexadmission, na.rm = TRUE)

datboth <- datboth %>% group_by(patientid) %>% mutate(indexadmidate = AdmissionDate[indexadmission == 1]) %>% ungroup()




datboth %>% select(patientid, indexadmidate, indexadmission, AdmissionDate) %>% glimpse()

nlc("# number of people who are missing a discharge date in HES for their admission:")
datboth %>% filter(indexadmission == 1 & is.na(DischargeDate)) %>% nrow()


nlc("# number of these people who have a discharge date in the audit:")
datboth %>% filter(indexadmission == 1 & is.na(DischargeDate)) %>% filter(!is.na(DischargeDateAudit)) %>% nrow()



nlc("# So we use this date for the discharge date instead of the HES date for these people. This many people missing both 
# are dropped:")

datboth %>% filter(indexadmission == 1 & is.na(DischargeDate)) %>% filter(is.na(DischargeDateAudit)) %>% nrow()


# We also need the index admission discharge date for analyses later on
datboth <- datboth %>% group_by(patientid) %>% mutate(indexdisdate = DischargeDate[indexadmission == 1]) %>% ungroup()

# datboth <- datboth %>% group_by(patientid) %>% mutate(indexdisdate = as.Date(ifelse(indexadmission == 1, DischargeDate, NA), 
#                                                                              origin = "1970-01-01")) %>% ungroup()


datboth$indexdisdate[is.na(datboth$indexdisdate)] <- datboth$DischargeDateAudit[is.na(datboth$indexdisdate)]

nrow(datboth)
datboth %>% filter(!is.na(indexdisdate)) %>% nrow()

datboth <- datboth %>% filter(!is.na(indexdisdate))



nlc("Number of admissions that occurred before the index admission and so need to be removed:")
datboth %>% filter(AdmissionDate < indexadmidate) %>% nrow()

nlc(
"Number of admissions that were the index admission or occurred after the index admission so need to be kept:")
datboth %>% filter(AdmissionDate >= indexadmidate) %>% nrow()

datboth <- datboth %>% filter(AdmissionDate >= indexadmidate)

# There are some readmissions that should not be counted as readmissions, which have been labeled previously.
# These should just be dropped from the dataset.
# Need to make sure we don't remove the index admissions when we do this

# Number of index admissions admitted on an 'ignore readmission' that should NOT be removed

nlc("Number of index admissions admitted on an 'ignore readmission' that should NOT be removed:")
datboth %>% filter(IgnoreReadmit == 1 & indexadmission == 1) %>% nrow()


nlc("Number of readmissions that should be ignored:")
datboth %>% filter(IgnoreReadmit == 1 & indexadmission == 0) %>% nrow()

# So, we filter these out of our dataset
nlc("Total number of admissions after removing ignored readmissions:")
datboth %>% filter(IgnoreReadmit == 0 | (IgnoreReadmit == 1 & indexadmission == 1)) %>% nrow()

datboth <- datboth %>% filter(IgnoreReadmit == 0 | (IgnoreReadmit == 1 & indexadmission == 1))

# we should check that no one died before they were admitted according to ONS.

nlc("Number of people who died before they were admitted (according to ONS) and should be excluded:")
datboth %>% filter(indexadmission == 1) %>% filter(DOD < indexadmidate) %>% nrow()

deathremove <- datboth %>% filter(indexadmission == 1) %>% filter(DOD < indexadmidate) %>% select(patientid)
datboth <- datboth %>% filter(!(patientid %in% deathremove$patientid))

datboth$timetodeath <- datboth$DOD - datboth$indexadmidate


# Let's calculate days to readmission (from discharge)

datboth$timetoreadmission <- datboth$AdmissionDate - datboth$indexdisdate

nlc("Index admission arrival date summary:")
summary(datboth$indexadmidate)

nlc("Index admission discharge date summary:")
summary(datboth$indexdisdate)

colnames(datboth)
# Then if the admission is the index admission we say it's missing

datboth$timetoreadmission[datboth$indexadmission == 1] <- NA



nlc("This many people were readmitted before they were discharged, so the admission
is assumed to be an error and the admission is removed:")

datboth %>% filter(indexadmission != 1) %>% filter(timetoreadmission < 0) %>% nrow()

nlc("This many people were readmitted on the same day as they were discharged, so the admission
is assumed to be an error and the admission is removed:")

datboth %>% filter(indexadmission != 1) %>% filter(timetoreadmission == 0) %>% nrow()

datboth <- datboth %>% filter(indexadmission == 1 | (indexadmission != 1 & timetoreadmission > 0))




nlc("Therefore, the final number of people in our dataset is:")


datboth %>% filter(indexadmission == 1) %>% nrow()



nlc("And the final number of admissions in our dataset is:")
datboth %>% nrow()


# Need to create the Charlson Comorbidity Index. 
# We can do this using the 'comorbidity' package.
# We should take the comorbidity at index admission. 

ccicalc <- datboth %>% filter(indexadmission == 1) %>% select(patientid, DIAG_01:DIAG_20)

ccicalc <- ccicalc %>% gather("Position", "code", DIAG_01:DIAG_20) %>% rename(id = patientid) %>%
  select(id, code) %>% arrange(id, code) %>% as.data.frame()

ccicalc <- comorbidity(x = ccicalc, id = "id", code = "code", map = "charlson_icd10_quan", assign0 = TRUE, tidy.codes = TRUE,
                       labelled = FALSE)

ccicalc$CCIweighted <- score(x = ccicalc, weights = "quan", assign0 = TRUE)

# old version of comorbidity
# ccicalc <- comorbidity(x = ccicalc, id = "id", code = "code", score = , assign0 = TRUE, tidy.codes = TRUE,
#                        labelled = FALSE, factorise = FALSE)


# ccicalc <- rename(ccicalc, CCIweighted = wscore)


nlc("Table of weighted Charlson comorbidity index at admission")
table(ccicalc$CCIweighted)


nlc("Because we expect everyone to have had at least COPD, we will group CCI scores of 0 and 1 together")

nlc("This people appear to not have had COPD according to the ICD10 code used for the CCI score calculation ('0' column)")
table(ccicalc$cpd)

# let's just keep the columns that we think are relevant
ccicalc <- select(ccicalc, id, cpd, CCIweighted)

# rename the id column as patientid, and attach it to the main dataset

ccicalc <- ccicalc %>% rename(patientid = id)
datboth <- left_join(datboth, ccicalc, by = "patientid")


# Now... we have to calculate 30 day readmission, 90 day readmission, 30 day mortality, 90 day mortality.


# Now we create the flags for if a readmission/death is within 30 days/90 days
# read30/90 marks if an admission was a readmission within 30/90 days
# read30/90total marks gives the total number of readmissions within 30/90 days for each patient -
# MUST FILTER TO INDEX ADMISSION IF YOU ARE USING THE read30/90total variable!!!!!!
# Important note - anything that is not an index admission is a readmission.
head(datboth$timetoreadmission, 200)
table(as.numeric(datboth$timetoreadmission))


datboth <- datboth %>% group_by(patientid) %>% mutate(read30 = ifelse(indexadmission == 1, 0,
                                                               ifelse(is.na(timetoreadmission) == TRUE, 0,
                                                               ifelse(timetoreadmission < 30, 1, 0)))) %>% ungroup()


nrow(datboth)
table(datboth$read30)
table(datboth$indexadmission)
summary(as.numeric(datboth$timetoreadmission))

datboth <- datboth %>% group_by(patientid) %>% add_tally(wt = read30) %>% rename(read30total = n) %>% ungroup()

nlc("total number of 30 day readmissions per patient:")
datboth %>% filter(indexadmission == 1) %>% select(read30total) %>% table()
nrow(datboth)
table(datboth$read30total)

datboth <- datboth %>% group_by(patientid) %>% mutate(read90 = ifelse(indexadmission == 1, 0,
                                                               ifelse(is.na(timetoreadmission) == TRUE, 0,        
                                                               ifelse(timetoreadmission < 90, 1, 0)))) %>% ungroup()

datboth <- datboth %>% group_by(patientid) %>% add_tally(wt = read90) %>% rename(read90total = n) %>% ungroup()


nlc("total number of 90 day readmissions per patient:")
datboth %>% filter(indexadmission == 1) %>% select(read90total) %>% table()


nlc("x-axis is 30-day readmission, y-axis is 90-day readmission")
table(datboth$read30, datboth$read90, useNA = "ifany")


datboth %>% filter(indexadmission == 1) %>% select(read30total) %>% table()

datboth %>% select(patientid, AdmissionDate, indexadmission, read30, read30total) %>% head(100) %>% print(n=100)

datboth %>% select(read30total) %>% table() %>% sum()


# I will also need the date of the first readmission at some point.
# First, get the total number of admissions for the patient in the dataset

datboth <- datboth %>% group_by(patientid) %>% arrange(patientid, AdmissionDate) %>% 
           add_count(name = "totaladmissions") %>% ungroup()

# !!!! slice doesn't work properly when used like this. It missing out the first observations
# if some groups only have 1 row, and only returns the second row

datboth <- datboth %>% group_by(patientid) %>% arrange(patientid, AdmissionDate) %>% 
           mutate(admino = row_number()) %>% ungroup()

datboth <- datboth %>% group_by(patientid) %>% mutate(firstreaddate = ifelse(totaladmissions == 1, NA,
                                                                      AdmissionDate[admino == 2])) %>% ungroup()

class(datboth$firstreaddate) <- "Date"

nlc("This shows the number of admissions per person:")
datboth %>% filter(indexadmission == 1) %>% select(totaladmissions) %>% table()

nlc("This should be the same as the number of missings in the output below showing readmission dates:")
summary(datboth$firstreaddate[datboth$indexadmission == 1])


# We also need a time to first readmission variable to apply to the whole dataset

datboth$timetofirstread <- datboth$firstreaddate - datboth$indexadmidate




# Death is treated slightly differently - just marked on the index admission. We don't need the tally command.

datboth <- datboth %>% group_by(patientid) %>% mutate(died30 = ifelse(indexadmission != 1, 0,
                                                               ifelse(is.na(timetodeath) == TRUE, 0,
                                                               ifelse(timetodeath < 30, 1, 0)))) %>% ungroup()


# Also create a variable that can be used in the OR
datboth <- datboth %>% group_by(patientid) %>% mutate(died30flag = ifelse(sum(died30) == 1, "Died", "Alive")) %>%
           ungroup()
datboth$died30flag <- factor(datboth$died30flag, levels = c("Alive", "Died")) 


nlc("total number of patients who died within 30 days:")
datboth %>% filter(died30 == 1) %>% nrow()


datboth <- datboth %>% group_by(patientid) %>% mutate(died90 = ifelse(indexadmission != 1, 0,
                                                               ifelse(is.na(timetodeath) == TRUE, 0,
                                                               ifelse(timetodeath < 90, 1, 0)))) %>% ungroup()


# Also create a variable that can be used in the OR
datboth <- datboth %>% group_by(patientid) %>% mutate(died90flag = ifelse(sum(died90) == 1, "Died", "Alive")) %>%
           ungroup()
datboth$died90flag <- factor(datboth$died90flag, levels = c("Alive", "Died")) 



nlc("total number of patients who died within 90 days:")
datboth %>% filter(died90 == 1) %>% nrow()




nlc("x-axis is died within 30 days, y-axis is died within 90 days")
datboth %>% filter(indexadmission == 1) %>% select(died30, died90) %>% table()



# Create the 3-character converted value for DIAG_01 and CAUSEOFDEATH

datboth <- datboth %>% mutate_at(vars("DIAG_01"), .funs = list(DIAG_01_conv = ~substr(., start = 1, stop = 3)))
datboth$DIAG_01_conv <- as.character(datboth$DIAG_01_conv)
datboth$DIAG_01_conv[is.na(datboth$DIAG_01_conv) == TRUE] <- "Missing code"
datboth$DIAG_01_conv <- as.factor(datboth$DIAG_01_conv)


datboth <- datboth %>% mutate_at(vars("CAUSE_OF_DEATH"), .funs = list(CAUSE_OF_DEATH_conv = ~substr(., start = 1, stop = 3)))


# And we have to create grouped variables for some of our data
# Make sure you write (right = FALSE) so that people on the borderline end up in the right group.

datboth$agecat <- cut(datboth$age, breaks = c(35, 45, 55, 65, 75, 85, 1000),
                      labels = c("35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
                      right = FALSE)

datboth$CCIweightedcat <- cut(datboth$CCIweighted, breaks = c(0, 2, 3, 4, 5, 6, 7, 1000),
                              labels = c("0-1", "2", "3", "4", "5", "6", "7+"),
                              right = FALSE)

datboth$read30totalcat <- cut(datboth$read30total, breaks = c(0, 1, 2, 3, 1000),
                              labels = c("0", "1", "2", "3+"),
                              right = FALSE)


datboth$read90totalcat <- cut(datboth$read90total, breaks = c(0, 1, 2, 3, 1000),
                              labels = c("0", "1", "2", "3+"),
                              right = FALSE)

# Also need to create general 30/90 day readmission/died flags

datboth$read30flag <- "Not readmitted"
datboth$read30flag[datboth$read30total > 0] <- "Readmitted"
datboth$read30flag <- factor(datboth$read30flag, levels = c("Not readmitted", "Readmitted"))

datboth$read90flag <- "Not readmitted"
datboth$read90flag[datboth$read90total > 0] <- "Readmitted"
datboth$read90flag <- factor(datboth$read90flag, levels = c("Not readmitted", "Readmitted"))


table(datboth$read30flag)
table(datboth$read30)
table(datboth$read30total)

# we also may as well sort out the order of the factors that we're using for the OR calculations at this point.

# drop genderbin because it is really confusing - male is the base level in the factor variable, but is coded
# as 1 in the binary variable

datboth$genderbin <- NULL

# I'll leave this for now...

# Make IMD quintile a factor
# datboth$IMD.quintile <- factor(datboth$IMD.quintile, levels = 1:5)

# Age categories are fine
# CCI cat is fine

# People in the audit don't have a length of stay if they died. HES is taken as gold standard over audit data
# in terms of admission and discharge anyway, so am going to calculate length of stay using HES admission and
# discharge. This should be used for obtaining OR estimates.



datboth <- datboth %>% rename(LOSAUDITDONTUSE = lengthofstay)

# remove the 'long stay' variable
datboth$longstay <- NULL

# Create your new length of stay
datboth <- datboth %>% mutate(LOS = (datboth$indexdisdate - datboth$indexadmidate))

# and create the new long stay variable
datboth$longstay <- NULL

datboth$longstayHES <- cut(as.numeric(datboth$LOS), breaks = c(-1, 4.5, 1000),
                           labels = c("0-4 days", "5+ days"))


# Make hospital a factor
datboth$hospital <- factor(datboth$hospital)



# We don't worry about this either.
# # Also, create a new factor variable for NIV ever
# 
# summary(datboth$nivtime)
# 
# datboth$NIVgiven <- "NIV not given"
# datboth$NIVgiven[datboth$niveverbin == 1] <- "NIV given"
# datboth$NIVgiven <- factor(datboth$NIVgiven, levels = c("NIV not given", "NIV given"))

sink()

datboth <- rename(datboth, life_status = q10_1dischargelifestatus)

# Get all the column names consistent

datboth <- datboth %>% rename(hosp_code = hospitalcode,
                              hosp_name = hospital,
                              trust_code = trustcode)
                              

# Let's add the general data here as well

general_dat <- read.csv("C:/Users/aadamson/Documents/COPD/SCC_2021-22/data/rawData/COPD-2104-2203-v103-Imperial.csv")


general_dat <- general_dat %>% select(Org, Region, Trust.Now) %>% 
  rename(hosp_code = Org, region = Region, trust_name = Trust.Now) %>% group_by(hosp_code) %>% slice(1)


datboth <- left_join(datboth, general_dat, by = "hosp_code")


# we also need to create our variables that we need for the outcomes analysis.



colnames(datboth)

datboth$IMD_quintile_all <- datboth$imd2019quin
datboth$IMD_quintile_all[is.na(datboth$IMD_quintile_all)] <- datboth$wimd2019quin[is.na(datboth$IMD_quintile_all)]
datboth$IMD_quintile_all[is.na(datboth$IMD_quintile_all)] <- datboth$simd2016quin[is.na(datboth$IMD_quintile_all)]
# datboth$IMD_quintile_all[is.na(datboth$IMD_quintile_all)] <- datboth$simd2020v2quin[is.na(datboth$IMD_quintile_all)]
datboth$IMD_quintile_all[is.na(datboth$IMD_quintile_all)] <- "Missing"

datboth$IMD_quintile_all <- factor(datboth$IMD_quintile_all)

datboth$IMD_combined <- paste("Eng", datboth$imd2019quin, sep = "_")
datboth$IMD_combined[datboth$IMD_combined == "Eng_NA"] <- NA
datboth$IMD_combined[is.na(datboth$IMD_combined)] <- paste("Wal", datboth$wimd2019quin[is.na(datboth$IMD_combined)], sep = "_") 
datboth$IMD_combined[datboth$IMD_combined == "Wal_NA"] <- NA
datboth$IMD_combined[is.na(datboth$IMD_combined)] <- paste("Scot", datboth$simd2016quin[is.na(datboth$IMD_combined)], sep = "_") 
datboth$IMD_combined[datboth$IMD_combined == "Scot_NA"] <- NA
datboth$IMD_combined[is.na(datboth$IMD_combined)] <- "Missing"
datboth$IMD_combined <- factor(datboth$IMD_combined, levels = c("Eng_1",   "Eng_2",   "Eng_3",   "Eng_4",   "Eng_5",  
                                                        "Wal_1",   "Wal_2",   "Wal_3",   "Wal_4",   "Wal_5",
                                                        "Scot_1",  "Scot_2",  "Scot_3",  "Scot_4",  "Scot_5", "Missing" ))



# saveRDS(datboth,
# "C:/Users/aadamson/Documents/COPD/SC_outcomes_2018-19/Data/tidyData/linked_audit_HES_PEDW_ONS_data_2018-19.RDS")


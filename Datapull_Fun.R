############ Set up ############ 

source('~/Desktop/Protect/Startup_2022.R')# make sure that `online = TRUE` in the bsrc.checkdabase2() function

startup()

library("tidyverse") 
library("ggplot2") 
library("eeptools")
library("lubridate")
library("quest")
library("shiny")
library("plotly")
library(DT)

# Get data from each protocol
# pull demographic data from ptcs$masterdemo 
demographics <- bsrc.checkdatabase2(ptcs$masterdemo, batch_size = 500L, online = T)
# select three different types of ID we need 
idMap <- demographics$data %>% select(masterdemoid = registration_redcapid, wpicid = registration_wpicid, soloffid = registration_soloffid)
# pull Protect 3 data 
protect <- bsrc.checkdatabase2(ptcs$protect, batch_size = 500L, online = T)

############ Set up ############################################################ 
# Step 1: pull eligible data
eligible <- demographics$data %>% 
  select(matches("registration_redcapid|ptcstat___pro|ptcstat___sui|term_reason_pro|term_reason_sui|excl_sui|excl_pro")) %>%
  mutate_all(na_if, "") # Transform blank to NA

eligible <- eligible %>% 
  filter(rowSums(eligible[2:6]) >= 1) %>% # Consented for 1 protocol (S1-P3)
  filter_at(vars(contains("excl_")), all_vars(is.na(.)|. != 1)) # Not excluded

IDlist <- eligible$registration_redcapid

any(duplicated(eligible$registration_redcapid)) # Only 1 ID a row

# Tidy database: change ID column as masterdemoid - yama's code
PTdata <- protect$data %>% 
  bsrc.findid(idMap, "registration_redcapid") %>% # Map ID
  select(masterdemoid, everything()) %>%
  filter(ifexist==T) %>% #%>% # Select only existing data
  filter(masterdemoid %in% IDlist)

MDdata <- demographics$data %>% 
  filter(registration_redcapid %in% IDlist) %>% 
  select(masterdemoid = registration_redcapid, everything())

#################################################################################
### Date map for protect
# Get the map of dates for each event
map <- bsrc.getIDDateMap(db = protect)
map <- bsrc.findid(map, idmap = idMap, "registration_redcapid") # map ID
map <- map %>% 
  select(masterdemoid, redcap_event_name, date)
map$masterdemoid <- as.character(map$masterdemoid) # for merging purpose later
#################################################################################

## Step 2: pull demographics
# From MD database
demo <- MDdata %>% 
  select(matches("masterdemoid|registration_groupchange|registration_oggroup|condate_pro|dob|condate_sui|registration_group$|gender|edu|race|hispanic|term_reason_pro|term_reason_sui|excl_sui|excl_pro|reg_term_exclwhy_protect1|reg_term_exclwhy_protect2|reg_term_exclwhy_protect3|reg_term_exclwhy_suicide|reg_term_exclwhy_suicid2")) %>% 
  mutate_all(na_if, "") %>% # make blanks to NA
  mutate( 
    registration_group = ifelse((registration_groupchange == 0 | is.na(registration_groupchange)), registration_group, registration_oggroup),
    registration_gender = recode(registration_gender, `F` = "Female", `M` = "Male")
  )
# Recode reason for terminated for specific reason
demo <- demo %>%
  mutate(reg_term_reason_protect3 = recode(reg_term_reason_protect3, `1`="Completed Study",`2`="Lost to follow-up",`3`="Ineligible after signing consent",`4`="Withdrawn at own/family request", `5`="Withdrawn by PI d/t other reasons (death, unreliability, etc.)"))

demo <- demo %>%
  mutate(reg_term_reason_protect2 = recode(reg_term_reason_protect2, `1`="Completed Study",`2`="Lost to follow-up",`3`="Ineligible after signing consent",`4`="Withdrawn at own/family request", `5`="Withdrawn by PI d/t other reasons (death, unreliability, etc.)", `6`="Didn't want to move to P3"))

demo <- demo %>%
  mutate(reg_term_reason_protect = recode(reg_term_reason_protect, `1`="Completed Study",`2`="Lost to follow-up",`3`="Ineligible after signing consent",`4`="Withdrawn at own/family request", `5`="Withdrawn by PI d/t other reasons (death, unreliability, etc.)"))

demo <- demo %>%
  mutate(reg_term_reason_suicid2 = recode(reg_term_reason_suicid2, `1`="Completed Study",`2`="Lost to follow-up",`3`="Ineligible after signing consent",`4`="Withdrawn at own/family request", `5`="Withdrawn by PI d/t other reasons (death, unreliability, etc.)"))

demo <- demo %>%
  mutate(reg_term_reason_suicide = recode(reg_term_reason_suicide, `1`="Completed Study",`2`="Lost to follow-up",`3`="Ineligible after signing consent",`4`="Withdrawn at own/family request", `5`="Withdrawn by PI d/t other reasons (death, unreliability, etc.)"))


# Race: make one race variable out of checkbox variables
demo <- bsrc.checkbox(variablename = "registration_race", dfx = demo) # create a list
demo <- demo %>%
  mutate(race = lapply(demo$registration_race, "[[", 1)) #%>%
  #mutate(race = ifelse(demo$registration_multirace == 1, 6, demo$race)) # if multirace, race = '6'


# find first CONSENT DATE
demo <- demo %>%
  mutate_at(vars(contains("reg_condate")), ~ymd(.)) %>%
  mutate(mindate = pmin(reg_condate_protect, reg_condate_protect2, reg_condate_protect3,
                        reg_condate_suicide, reg_condate_suicid2, na.rm = T))

# check no missing dob - should be 0
which(is.na(demo$registration_dob))

# calculate baseline age: first consent date - dob
dob = as.Date(demo$registration_dob)
enddate = as.Date(demo$mindate)
demo$age <- eeptools::age_calc(dob = dob, enddate = enddate, units = "years", precise = FALSE)

# check no missing ages - should be 0
which(is.na(demo$age))

######## SPECIFIC ASSESSMENTS ############################################
##########################################################################
########## SUICIDE HISTORY #########################################################

demoMinDate <- demo %>%
  select(masterdemoid, mindate) # get consent date

saHx <- MDdata %>% 
  select(matches("masterdemoid|sadate|lr")) %>% 
  mutate_all(na_if, "") %>%
  pivot_longer(cols = sahx_sadate_at1:sahx_lr_at30,
               names_to = c(".value", "att"),
               names_prefix = "sahx_",
               names_sep = "_at") %>%
  select(masterdemoid, attempt = att, date = sadate, lethality = lr) %>%
  merge(demoMinDate, by = "masterdemoid") %>%
  group_by(masterdemoid) %>%
  filter(date <= mindate) %>% # filter out attempts after consent date
  mutate(blAtt = n()) %>% # calculate number of baseline attempts
  mutate(lethality_max = max(lethality, na.rm = T)) %>% # calculate max lethality
  mutate(lethality_max = ifelse(is.infinite(lethality_max), NA, lethality_max)) %>% # change -inf to NA
  mutate(attempt_mindate = min(date, na.rm = T)) %>% # calculate earliest attempt date
  mutate(attempt_mindate = ifelse(is.infinite(attempt_mindate), NA, attempt_mindate)) # change -inf to NA

# find date of highest lethality attempt
saHx_1 <- saHx %>%
  filter(lethality_max == lethality) %>%
  mutate(date_maxLeth = date) %>%
  select(date_maxLeth, masterdemoid)

saHx <- left_join(saHx, saHx_1, by = "masterdemoid") # merge back with suicide history dataframe

saHx <- saHx %>%
  arrange(date_maxLeth) %>%
  distinct(masterdemoid, .keep_all = TRUE) %>%
  select(masterdemoid, lethality_max, date_maxLeth, attempt_mindate, blAtt)

# check
any(duplicated(saHx$masterdemoid)) # should be false

# for merging purposes
saHx$masterdemoid <- as.character(saHx$masterdemoid)
demo$masterdemoid <- as.character(demo$masterdemoid)

# merge suicide history variables to demographic data frame
demo <- left_join(demo, saHx, by = "masterdemoid")

#################################################################################
############# EXIT ##########################################

exitraw<- PTdata %>% 
  select(masterdemoid, redcap_event_name, starts_with("exit_"))%>%
  mutate_all(~replace(.,.=="",NA)) %>% 
  filter_at(vars(-masterdemoid,-redcap_event_name, -exit_admin, -exit_complete, -exit_miss,-exit_done___1, -exit_done___ni, -exit_done___nask, -exit_done___asku, -exit_done___na, -exit_miss),any_vars(!is.na(.))) %>% 
  as.data.frame() 
exitraw <- exitraw %>% 
  select(-exit_total)
exitraw <- exitraw %>%
  distinct(exitraw$masterdemoid, .keep_all = T)

exitraw <-bsrc.score(df=exitraw, formname="exit")

exitraw$masterdemoid=as.character(exitraw$masterdemoid)
exit_map <- exitraw %>% 
  left_join(map, by = c("masterdemoid", "redcap_event_name"))

demo$mindate <- as.Date(demo$mindate)

exit_map <- exit_map %>%
  mutate(firstconsent = demo$mindate[match(exit_map$masterdemoid,demo$masterdemoid)])



exit_map$firstconsent <- as.Date(exit_map$firstconsent)
exit_map$date <- as.Date(exit_map$date)

exit_final <- exit_map %>% 
  filter(!is.na(exit_total))%>%
  mutate(date_dif = abs(date-firstconsent)) %>%
  group_by(masterdemoid) %>% 
  #filter(date_dif == min(date_dif, na.rm = F)) %>% 
  #mutate(date_dif = ifelse(is.infinite(date_dif), NA, date_dif)) %>% # change -inf to NA
  #filter(date_dif <= 30) %>%
  slice(1) %>% 
  select(masterdemoid, exit_total)

anyDuplicated(exit_final$masterdemoid)
exit_final$masterdemoid=as.character(exit_final$masterdemoid)
demo$masterdemoid=as.character(demo$masterdemoid)
demo <- left_join(demo,exit_final, by="masterdemoid")


#################################################################################

#demo <- demo %>%
#  select(masterdemoid, registration_group, registration_dob, registration_gender, age)

#################################################################################
################ INCOME #########################################

income_1 <- PTdata %>%
  select(masterdemoid, macarthur_6, macarthur_7) %>%
  filter_at(vars(2,3), any_vars(!is.na(.))) %>%
  filter(macarthur_6 != 998) %>%
  distinct()
income_1 <- income_1 %>% # grab the vars of interest and remove empty rows
  mutate(income_household = income_1$macarthur_6) %>%
  # map answers to corresponding income levels
  mutate(income_level =  ifelse(income_1$macarthur_6 == "1", 0,
                                ifelse(income_1$macarthur_6 == "2", 8500,
                                       ifelse(income_1$macarthur_6 == "3", 14000, 
                                              ifelse(income_1$macarthur_6 == "4", 20500,
                                                     ifelse(income_1$macarthur_6 == "5", 30000,
                                                            ifelse(income_1$macarthur_6 == "6", 42500,
                                                                   ifelse(income_1$macarthur_6 == "7", 62500,   
                                                                          ifelse(income_1$macarthur_6 == "8", 87500, 100000)
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
  )
  )
# calculate income per capita
income_1$incomePerCapita = income_1$income_level / income_1$macarthur_7
# change decimal places
income_1$incomePerCapita = round(income_1$incomePerCapita, digits = 1)

#check for duplicates
if(any(duplicated(income_1$masterdemoid))){message("People have difference incomes at different timepoints")}

# use jiazhou's map code to pick the lowest date
income_1 <- merge(income_1, map, by = c("masterdemoid"))
income_1 <- income_1 %>% 
  select(-redcap_event_name) %>% 
  group_by(masterdemoid) %>% 
  filter(date == min(date))
income_1 <- income_1[-which(duplicated(income_1$masterdemoid)), ]
income_1$masterdemoid <- as.character(income_1$masterdemoid)
# Add this into your main dataframe
income_1 <- income_1 %>%
  select(masterdemoid, incomePerCapita, income_household)

income_1$masterdemoid <- as.character(income_1$masterdemoid)
demo$masterdemoid <- as.character(demo$masterdemoid)

demo <- left_join(demo, income_1, by = "masterdemoid", all.x = T)

#################################################################################
############ SIS ###########################################

SISraw <- PTdata %>% 
  # select master., red., and sis_max_1-18, and sis_recent_1-18
  select(masterdemoid, redcap_event_name, paste0("sis_max_", c(1:18)), paste0("sis_recent_", c(1:18))) %>%
  mutate_all(~replace(., .=="", NA)) %>% #idk what this is 
  filter_at(vars(-masterdemoid,-redcap_event_name),any_vars(!is.na(.))) %>% # remove rows where it's all NA - this is the best way to do it
  as.data.frame()

# Scoring using `bsrc.score`
SIS_score <- bsrc.score(SISraw, formname = "sis")

# Map date as the event date
SIS_score <- merge(SIS_score, map, by = c("masterdemoid", "redcap_event_name"))

# First consent is the baseline date then take the closest ones
#first instance of this masterdemoid from SIS_score in demo - then, which mindate corresponds to that masterdemoid
SIS_score$firstconsent <- demo$mindate[match(SIS_score$masterdemoid, demo$masterdemoid)]
SIS_score$firstconsent
SIS_score$date <- as.Date(SIS_score$date)
SIS_score$firstconsent <- as.Date(SIS_score$firstconsent)

SIS_final <- SIS_score %>%
  # difference between SIS_score date and first consent
  mutate(date_dif = abs(date - firstconsent)) %>%
  group_by(masterdemoid) %>%
  # only use scores closest to first consent
  filter(date_dif == min(date_dif, na.rm = T)) %>%
  filter(date_dif <= 30) %>%
  filter(row_number() == 1)

SIS_final <- SIS_final %>%
  select(masterdemoid, sis_max_plan, sis_max_total)

#check for duplicates
any(duplicated(SIS_final$masterdemoid)) 

# Add to original dataframe
SIS_final$masterdemoid <- as.character(SIS_final$masterdemoid)

demo <- left_join(demo, SIS_final, by = "masterdemoid")

###########################################################3
######## SSI ##########################################################

ssiraw<- PTdata %>% 
  select(masterdemoid, redcap_event_name, starts_with("ssi_"))%>%
  mutate_all(~replace(.,.=="",NA)) %>% 
  as.data.frame() 

ssiraw <- ssiraw %>%
  distinct(masterdemoid, .keep_all = T)

ssiraw <-bsrc.score(df=ssiraw, formname="ssi")

ssiraw$masterdemoid=as.character(ssiraw$masterdemoid)
ssi_map <- ssiraw %>% 
  left_join(map, by = c("masterdemoid", "redcap_event_name"))



ssi_map <- ssi_map %>%
  mutate(firstconsent = (demo$mindate[match(ssi_map$masterdemoid, demo$masterdemoid)]))


ssi_map$firstconsent <- as.Date(ssi_map$firstconsent)
ssi_map$date <- as.Date(ssi_map$date)

ssi_final <- ssi_map %>% 
  filter(!is.na(SSI_worst), !is.na(SSI_current), !is.na(date))

ssi_final <- ssi_final %>%
  mutate(date_dif = abs(date-ssi_final$firstconsent)) #%>%
  group_by(ssi_final$masterdemoid) %>% 
  filter(date_dif == min(date_dif, na.rm = F)) %>% 
  mutate(date_dif = ifelse(is.infinite(date_dif), NA, date_dif)) %>% # change -inf to NA
  filter(date_dif <= 30) %>%
  slice(1) %>% 
  select(ssi_final$masterdemoid, SSI_worst, SSI_current)

anyDuplicated(ssi_final$masterdemoid)
ssi_final$masterdemoid=as.character(ssi_final$masterdemoid)
demo$masterdemoid=as.character(demo$masterdemoid)
demo <- left_join(demo,ssi_final, by="masterdemoid")

############################################################
demo$registration_group <- as.factor(demo$registration_group)
demo$registration_gender <- as.factor(demo$registration_gender)
demo$registration_dob <- as.Date(demo$registration_dob)
demo$masterdemoid <- as.integer(demo$masterdemoid)
demo$date_maxLeth <- as.Date(demo$date_maxLeth)
demo$race <- unlist(demo$race)
#################################################################################

#demo <- demo %>%
#  mutate(incomePerCapita = ifelse(is.na(incomePerCapita), -1, incomePerCapita))

#demo <- demo %>%
#  mutate(blAtt = ifelse(is.na(blAtt), 0, blAtt))

#demo <- demo %>%
#  mutate(lethality_max = ifelse(is.na(lethality_max), -1, lethality_max))

#demo <- demo %>%
  #mutate(exit_total = ifelse(is.na(exit_total), -1, exit_total))

#fixups
demo$exit_total <- as.integer(demo$exit_total)

demo <- demo %>%
  mutate(race = recode(race, `1`="American Indian/Alaska Native", `2`="Asian", `3`="Black", `4`="Native Hawaiian/Pacific Islander", `5`="White", `6`="Multirace"))

demo$race <- as.factor(demo$race)

demo <- demo %>%
  mutate(registration_4group = registration_group) %>%
  mutate(registration_3group = recode(registration_group, `IDE`="DNA", `DEP`="DNA", `DNA`="DNA", `ATT`="ATT", `HC`="HC"))

demo <- demo %>%
  select(masterdemoid, registration_3group, registration_4group, registration_dob, registration_gender, mindate, reg_condate_protect3, reg_condate_protect2, reg_condate_protect,reg_condate_suicide, reg_condate_suicid2,  race, age, lethality_max, date_maxLeth, blAtt, incomePerCapita, income_household, exit_total) #%>%
  
demo_protect3 <- demo %>%
  filter(mindate == reg_condate_protect3)

demo_protect2 <- demo %>%
  filter(mindate == reg_condate_protect2)

demo_protect1 <- demo %>%
  filter(mindate == reg_condate_protect)

demo_suicide <- demo %>%
  filter(mindate == reg_condate_suicide)

demo_suicid2 <- demo %>%
  filter(mindate == reg_condate_suicid2)


demo3 <- demo_protect3 %>%
     select(masterdemoid, registration_3group, registration_4group, registration_dob, registration_gender, mindate, reg_condate_protect3, race, age, lethality_max, date_maxLeth, blAtt, incomePerCapita, income_household, exit_total) #%>%
demo2 <- demo_protect2 %>%
  select(masterdemoid, registration_3group, registration_4group, registration_dob, registration_gender, mindate, reg_condate_protect3, reg_condate_protect2, race, age, lethality_max, date_maxLeth, blAtt, incomePerCapita, income_household, exit_total) #%>%
demo1 <- demo_protect1 %>%
  select(masterdemoid, registration_3group, registration_4group, registration_dob, registration_gender, mindate, reg_condate_protect3, reg_condate_protect2, reg_condate_protect, race, age, lethality_max, date_maxLeth, blAtt, incomePerCapita, income_household, exit_total) #%>%
demos <- demo_suicide %>%
  select(masterdemoid, registration_3group, registration_4group, registration_dob, registration_gender, mindate, reg_condate_protect3, reg_condate_protect2, reg_condate_protect,reg_condate_suicide, race, age, lethality_max, date_maxLeth, blAtt, incomePerCapita, income_household, exit_total) #%>%
demos2 <- demo_suicid2 %>%
  select(masterdemoid, registration_3group, registration_4group, registration_dob, registration_gender, mindate, reg_condate_protect3, reg_condate_protect2, reg_condate_protect,reg_condate_suicide, reg_condate_suicid2,  race, age, lethality_max, date_maxLeth, blAtt, incomePerCapita, income_household, exit_total) #%>%
             

###################################################################################################
######### SHINY APP ###################################################

ui <- shinyUI(navbarPage("Data",
                         tabPanel("Protect3",
                                  sidebarPanel(checkboxGroupInput("varCheck3", "Variables to Display:", names(demo3), names(demo3))),
                                  mainPanel(DT::dataTableOutput("protect3table"), downloadButton("download3", "Download .csv"))
                         ),
                         tabPanel("Protect2",
                                  sidebarPanel(checkboxGroupInput("varCheck2", "Variables to Display:", names(demo2), names(demo2))),
                                  mainPanel(DT::dataTableOutput("protect2table"), downloadButton("download2", "Download .csv"))
                         ),
                         tabPanel("Protect1",
                                  sidebarPanel(checkboxGroupInput("varCheck1", "Variables to Display:", names(demo1), names(demo1))),
                                  mainPanel(DT::dataTableOutput("protect1table"), downloadButton("download1", "Download .csv"))
                         ),
                         tabPanel("Suicide",
                                  sidebarPanel(checkboxGroupInput("varChecks", "Variables to Display:", names(demos), names(demos))),
                                  mainPanel(DT::dataTableOutput("suicidetable"), downloadButton("downloads", "Download .csv"))
                         ),
                         tabPanel("Suicid2",
                                  sidebarPanel(checkboxGroupInput("varChecks2", "Variables to Display:", names(demos2), names(demos2))),
                                  mainPanel(DT::dataTableOutput("suicid2table"), downloadButton("downloads2", "Download .csv"))
                         )
))

server <- function(input, output, session) 
{
  
  demo3 <- demo3
  
  output$protect3table = DT::renderDT({
    DT::datatable(demo3[,input$varCheck3, drop = FALSE],filter = "bottom",extensions = 'Buttons', options = list(paging=TRUE, processing=FALSE, buttons = c("copy", "csv", "pdf")), class = "display", rownames= FALSE)
  }, server = FALSE)
  output$download3 = downloadHandler('demo-filtered.csv', content = function(file) {
     write.csv(demo3[input$protect3table_rows_all, , drop = FALSE], file)
     })

  #######################
  
  demo2 <- demo2
  
  output$protect2table = DT::renderDT({
    DT::datatable(demo2[,input$varCheck2, drop = FALSE],filter = "bottom",extensions = 'Buttons', options = list(paging=TRUE, processing=FALSE, buttons = c("copy", "csv", "pdf")), class = "display", rownames= FALSE)
  }, server = FALSE)
  output$download2 = downloadHandler('demo-filtered.csv', content = function(file) {
    write.csv(demo2[input$protect2table_rows_all, , drop = FALSE], file)
  })
  #######################
  
  demo1 <- demo1
  
  output$protect1table = DT::renderDT({
    DT::datatable(demo1[,input$varCheck1, drop = FALSE],filter = "bottom",extensions = 'Buttons', options = list(paging=TRUE, processing=FALSE, buttons = c("copy", "csv", "pdf")), class = "display", rownames= FALSE)
  }, server = FALSE)
  output$download1 = downloadHandler('demo-filtered.csv', content = function(file) {
    write.csv(demo1[input$protect2table_rows_all, , drop = FALSE], file)
  })
  
  #######################
  
  demos <- demos
  
  output$suicidetable = DT::renderDT({
    DT::datatable(demos[,input$varChecks, drop = FALSE],filter = "bottom",extensions = 'Buttons', options = list(paging=TRUE, processing=FALSE, buttons = c("copy", "csv", "pdf")), class = "display", rownames= FALSE)
  }, server = FALSE)
  output$downloads = downloadHandler('demo-filtered.csv', content = function(file) {
    write.csv(demos[input$suicidetable_rows_all, , drop = FALSE], file)
  })
  
  #######################
  
  demos2 <- demos2
  
  output$suicid2table = DT::renderDT({
    DT::datatable(demos2[,input$varChecks2, drop = FALSE],filter = "bottom",extensions = 'Buttons', options = list(paging=TRUE, processing=FALSE, buttons = c("copy", "csv", "pdf")), class = "display", rownames= FALSE)
  }, server = FALSE)
  output$downloads2 = downloadHandler('demo-filtered.csv', content = function(file) {
    write.csv(demos2[input$suicid2table_rows_all, , drop = FALSE], file)
  })
}

shinyApp(ui, server)

# Export
#write_csv(demo_final, file = "/home/bgcampbell/Desktop/sheet",".csv")
#library("writexl")
#df <- as.data.frame(df)

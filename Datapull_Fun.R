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

# check - yama's code
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

#~~~~~~~~~~~~~~~~
### Date map for protect
# Get the map of dates for each event
map <- bsrc.getIDDateMap(db = protect)
map <- bsrc.findid(map, idmap = idMap, "registration_redcapid") # map ID
map <- map %>% 
  select(masterdemoid, redcap_event_name, date)
map$masterdemoid <- as.character(map$masterdemoid) # for merging purpose later
#~~~~~~~~~~~~~~~~

### SEPARATE ####################################################################
# protect1 <- eligible %>%
#   filter_at(vars("registration_ptcstat___protect"), all_vars(. == 1)) %>%
#   select(registration_redcapid, registration_ptcstat___protect) %>%
#   as.data.frame()
# protect2 <- eligible %>%
#   filter_at(vars("registration_ptcstat___protect2"), all_vars(. == 1)) %>%
#   select(registration_redcapid, registration_ptcstat___protect2) %>%
#   as.data.frame()
# protect3 <- eligible %>%
#   filter_at(vars("registration_ptcstat___protect3"), all_vars(. == 1)) %>%
#   select(registration_redcapid, registration_ptcstat___protect3) %>%
#   as.data.frame()
# suicide <- eligible %>%
#   filter_at(vars("registration_ptcstat___suicide"), all_vars(. == 1))
# suicid2 <- eligible %>%
#   filter_at(vars("registration_ptcstat___suicid2"), all_vars(. == 1))
#################################################################################
## Step 2: pull demographics
# From MD database
demo <- MDdata %>% 
  select(matches("masterdemoid|registration_groupchange|registration_oggroup|condate_pro|dob|condate_sui|registration_group$|gender|edu|race|hispanic")) %>% 
  mutate_all(na_if, "") %>% # make blanks to NA
  mutate( 
    registration_group = ifelse((registration_groupchange == 0 | is.na(registration_groupchange)), registration_group, registration_oggroup),
    registration_gender = recode(registration_gender, `F` = "Female", `M` = "Male")
  )

# Race: make one race variable out of checkbox variables
demo <- bsrc.checkbox(variablename = "registration_race", dfx = demo) # create a list
demo <- demo %>%
  mutate(race = lapply(demo$registration_race, "[[", 1)) %>%
  mutate(race = ifelse(demo$registration_multirace == 1, 6, demo$race)) # if multirace, race = '6'

# find first consent date
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

# Step 3: Other demo data: suicide history, income
### Get suicide history

# Get the suicide history variables that correspond to date and lethality
demoMinDate <- demo %>%
  select(masterdemoid, mindate) # get consent date

demo <- demo %>%
  select(masterdemoid, registration_group, registration_dob, registration_gender, age)

#################################################################################
ui <- fluidPage(
  titlePanel("Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageSlider", "Age Range:",min =min(demo$age),max =max(demo$age), value =range(demo$age), step = 1),
      checkboxGroupInput("varChecks", "Variables to Display:", names(demo), names(demo))
    ),
    mainPanel(
        id = 'dataset',
        DT::dataTableOutput("table"),
        downloadButton("download", "Download .csv")
    )
  )
)


server <- function(input, output, session) 
{
  demo2 = demo[sample(nrow(demo), 643),]
  output$table = DT::renderDataTable({
    DT::datatable(demo2[demo$age >= input$ageSlider[1] & demo$age <= input$ageSlider[2], input$varChecks, drop = FALSE],
                  options = list(paging=TRUE, processing=FALSE),
                  class = "display")
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("downloadFile", ".csv")
    },
    content = function(file) {
      write.csv(as.data.frame(demo2), file)
    }
  )
}

shinyApp(ui, server)




# Export
#write_csv(demo_final, file = "/home/bgcampbell/Desktop/sheet",".csv")
#library("writexl")
#df <- as.data.frame(df)
write_xlsx(demo_final, "./file.xlsx")

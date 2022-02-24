#### Preamble ####
#Purpose: Clean data downloaded from the American Economic Journal
#Author: Marco Chau & Chyna Hui
#Date: 18 February 2022
#Contacts: marco.chau@mail.utoronto.ca & chyna.hui@mail.utoronto.ca

#### Workspace Setup ####
#Use R Projects
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(readxl)

#Read in raw income and spending data
raw_iandsdat <- readxl::read_xls("inputs/data/gn_ui_targets2018-10-12.xls")

#Isolate spending data
clean_spendingdat <-
  raw_iandsdat |> 
  filter (
    key == "Spending" & 
      pbd == "6 Months"
    )

#Changing spending data column names
clean_spendingdat <- 
  clean_spendingdat %>% rename(
    "Months Since Start" = "mos_since_start",
    "Ratio to t = -5" = "value"
  )

#Add column indicating UI status
clean_spendingdat <- 
  clean_spendingdat %>% mutate(`UI Status` = 
                                 case_when( `Months Since Start` <= 0 ~ "Pre-UI",
                                            `Months Since Start` > 0 & `Months Since Start` <= 5 ~ "During UI",
                                            `Months Since Start` > 5 ~ "Post-UI")
  )


save (clean_spendingdat, file = "outputs/rda/clean_spendingdat.rda")

#Isolate income data
clean_incomedat <-
  raw_iandsdat |>
  filter (
    key == "Income" &
      pbd == "6 Months"
    )

#Changing income data column names
clean_incomedat <-
  clean_incomedat %>% rename(
    "Months Since Start" = "mos_since_start",
    "Ratio to t = -5" = "value"
  )

#Add column indicating UI Status
clean_incomedat <-
  clean_incomedat %>% mutate(`UI Status` =
                               case_when(`Months Since Start` <= 0 ~"Pre-UI",
                                         `Months Since Start` > 0 & `Months Since Start` <= 5 ~ "During UI",
                                         `Months Since Start` > 5 ~ "Post-UI")
                             )
save (clean_incomedat, file = "outputs/rda/clean_incomedat.rda")

#Combined spending and income data
clean_iandsdat <-
  raw_iandsdat |>
  filter (
    pbd == "6 Months"
  )

#Changing column names
clean_iandsdat <-
  clean_iandsdat %>% rename (
    "Months Since Start" = "mos_since_start",
    "Ratio to t = -5" = "value"
  )

save (clean_iandsdat, file = "outputs/rda/clean_iandsdat.rda")

#Read in raw spending change data
raw_spendchange <- readxl::read_xlsx("inputs/data/tables.xlsx", "t2_color")

#Remove first three rows of dataframe with old titles and legends
clean_spendchange <-
  raw_spendchange[-c(1,
                     2,
                     3,
                     32),
                  ] 

#Rename column titles
clean_spendchange <-
  clean_spendchange %>% rename (
    "Spending Type" = "Table 2: Spending Change at UI Exhaustion", 
    "Category" = "...2",
    "Pre-UI" = "...3",
    "During UI" = "...4",
    "Post-UI" = "...5",
    "Dollar Change Post and During UI" = "...6",
    "Percentage Change Post and During UI" = "...7"
    )

#Changing columns with numbers to numeric class
options(digits = 5)
  clean_spendchange$`Pre-UI` <- 
    as.numeric(clean_spendchange$`Pre-UI`)
  clean_spendchange$`During UI` <- 
    as.numeric(clean_spendchange$`During UI`)
  clean_spendchange$`Post-UI` <- 
    as.numeric(clean_spendchange$`Post-UI`)
  clean_spendchange$`Dollar Change Post and During UI` <- 
    as.numeric(clean_spendchange$`Dollar Change Post and During UI`)
  clean_spendchange$`Percentage Change Post and During UI` <- 
    as.numeric(clean_spendchange$`Percentage Change Post and During UI`)

#Multiply percentage column by "100" to show percentages 
clean_spendchange$`Percentage Change Post and During UI` <-  
  clean_spendchange$`Percentage Change Post and During UI` * 100

save(clean_spendchange, file = "outputs/rda/clean_spendchange.rda")

#Create new dataframe ordering data by spending type 
clean_spendchangtype <-
  clean_spendchange %>% arrange(factor(`Spending Type`, levels = c("Durable", 
                                                                   "Nondurable", 
                                                                   "Strict ND", 
                                                                   "Other ND", 
                                                                   "Other Bank Account Outflows")
                                       )
                                )

save (clean_spendchangtype, file = "outputs/rda/clean_spendchangetype.rda")
  
  





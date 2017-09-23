library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
library(ggmap)
library(jsonlite)
library(scales)
library(tigris)


boarding_cleaning <- read_csv('./data/Boarding_Cleaning_Requests_Rcvd.csv')
parcels <- read_csv('./data/parcel_crosswalk.csv')

unique_addresses <- boarding_cleaning %>% 
  select('CaseLocation') %>% 
  mutate(CaseLocation = paste(CaseLocation, 'LOUISVILLE, KY')) %>% 
  unique() %>% 
  as_tibble()

leins <- read_csv('./data/FinalOrders_History_0.csv') %>% 
  unique() %>% 
  mutate(property_address = paste0(ifelse(is.na(STNO),'',paste0(STNO,' ')),
                                   ifelse(is.na(PREDIR),'',paste0(PREDIR,' ')),
                                   STNAME,' ',
                                   ifelse(is.na(SUFFIX),'',SUFFIX),
                                   ifelse(is.na(STSUB),'',paste0(' ',STSUB))))

leins_unique <- leins %>% 
  select(property_address, FINE_ADDED, FINAL_CITATION_AMOUNT) %>% 
  unique() %>% 
  group_by(property_address) %>% 
  summarize(total_citations = sum(FINAL_CITATION_AMOUNT))

delinquent_taxes <- read_csv('./data/delinquent_taxes.csv') %>% 
  mutate(`September Account Balance` = as.numeric(str_replace(`September Account Balance`,'\\$','') %>% 
                                                    str_replace(',',''))) %>% 
  group_by(ParcelID, Name, `Property Address`) %>% 
  summarize(Account_Balance = sum(`September Account Balance`, na.rm = T)) %>% 
  mutate(joinable_parcel = str_sub(ParcelID, 3, 14))

vacant_abandoned <- boarding_cleaning %>% 
  group_by(CaseLocation) %>% 
  summarize(DateAbandoned = min(as.Date(CompletionDate), na.rm = T),
            Expenses = sum(PenaltyAmount, na.rm = T)) %>%  
  left_join(parcels, by = c('CaseLocation' = 'PROP_ADDRESS')) %>% 
  mutate(LengthAbandoned = ifelse(is.infinite(Sys.Date() - DateAbandoned), NA, Sys.Date() - DateAbandoned)) %>% 
  select(-DateAbandoned) %>% 
  left_join(delinquent_taxes, by = c('PARCELID' = 'joinable_parcel')) %>%
  left_join(leins_unique, by = c('CaseLocation' = 'property_address')) %>% 
  mutate(ZIP = str_sub(ZIP, 1, 5)) %>% 
  select(Location = CaseLocation, Expenses, `Years Abandoned` = LengthAbandoned,
         `Delinquent Tax Amount` = Account_Balance, `Total Leins` = total_citations, ZIP)

vacant_abandoned_zip <- vacant_abandoned %>%
  group_by(ZIP) %>% 
  summarize(`Number of Properties` = n(),
            `Total City Expenses` = sum(Expenses, na.rm = T),
            `Avg. Years Abandoned` = mean(`Years Abandoned`, na.rm = T),
            `Total Delinquent Tax Amt` = sum(`Delinquent Tax Amount`, na.rm = T),
            `Total Leins` = sum(`Total Leins`, na.rm = T)) %>% 
  mutate(`Total City Expenses` = dollar_format()(`Total City Expenses`),
         `Avg. Years Abandoned` = paste(round(`Avg. Years Abandoned` / 365.25, 2),'Years'),
         `Total Delinquent Tax Amt` = dollar_format()(replace(`Total Delinquent Tax Amt`, 
                                                           which(is.na(`Total Delinquent Tax Amt`)), 0)),
         `Total Leins` = dollar_format()(replace(`Total Leins`, which(is.na(`Total Leins`)), 0))) %>% 
  select(ZIP, `Number of Properties`, `Total City Expenses`,`Avg. Years Abandoned`,`Total Delinquent Tax Amt`,`Total Leins`) %>% 
  mutate(keep = str_match(ZIP, '402[\\d]{2}')) %>% 
  filter(!is.na(keep)) %>% 
  select(-keep)

save(vacant_abandoned_zip, file = './data/vacant_abandoned_data.rda')

# Author: Sean Cerulean Johnson
# Initial-Date: 2020-09-05
# Update-Date: 2022-09-29

# Data was collected from CDC mortality tables


# Libraries ----
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(janitor)
library(lubridate)

# Demographic ----
dem<-here::here("data","dem")%>%
  list.files(full.names = T, pattern = ".csv", recursive = T)%>%
  map(~read.csv(.x)%>%
  mutate(File.Source = tools::file_path_sans_ext(basename(.x)))%>%
  rename_with(.cols = c(2:13), ~c("Total_Both","Total_M","Total_F",
                                  "White_Both","White_M","White_F",
                                  "Other_Both","Other_M","Other_F",
                                  "Black_Both","Black_M","Black_F")))%>%

  bind_rows()%>%
  group_by(Age)%>%
  pivot_longer(!c(Age,File.Source), names_to = "Fields", values_to = "Values")%>%
  ungroup()%>%
  mutate(Year = paste0(20,substr(File.Source,5,6),"-12-31"),
         Year = case_when(Year == "2099-12-31" ~"1999-12-31",
                          TRUE ~Year),
         Year = ymd(Year))%>%
  select(Age, Year, Fields, Values, File.Source)
  

# Residents by age ----
res<-here::here("data","res")%>%
  list.files(full.names = T, pattern = ".csv", recursive = T)%>%
  map(~read.csv(.x)%>%
        mutate(File.Source = tools::file_path_sans_ext(basename(.x)),
               across(where(is.numeric), as.character))%>%
        rename_with(.cols = c(1:10), ~c("Birth.Place","1999","2000","2001","2002",
                                        "2003","2004","2005","2006","2007")))%>%
  bind_rows()%>%
  group_by(Birth.Place)%>%
  pivot_longer(!c(Birth.Place,File.Source), names_to = "Year", values_to = "Values")%>%
  filter(!Birth.Place=="")%>%
  ungroup()%>%
  mutate(Year = ymd(paste0(Year,"-12-31")),
         Values = as.numeric(as.factor(Values)),
         Age.Range = gsub(" ","",substr(File.Source,5,7)),
         Age.Range = case_when(Age.Range == "0"~"0",
                               Age.Range == "1"~"1-4",
                               Age.Range == "5"~"5-14",
                               Age.Range == "15"~"15-24",
                               Age.Range == "25"~"25-34",
                               Age.Range == "35"~"35-44",
                               Age.Range == "45"~"45-54",
                               Age.Range == "55"~"55-64",
                               Age.Range == "65"~"65-74",
                               Age.Range == "75"~"75-84",
                               Age.Range == "85"~"85+",
                               Age.Range == "ns"~"not stated"))%>%
  select(Birth.Place, Year, Age.Range, Values, File.Source)
  

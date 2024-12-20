library(rvest)
library(tidyverse)
library(lubridate)
#######################################################################################
# Extracting current senators and identify the ones who will elect for 2024
web_url <- "https://en.wikipedia.org/wiki/List_of_current_United_States_senators"
# Read the HTML page
web_page <- read_html(web_url)
# Extract the list of current senators
tables <- web_page |> html_nodes("table")
senators_table <- html_table(tables[[6]], fill = TRUE)  
colnames(senators_table)[4:5]<-c("NA","Party")
senators_table_clean<-senators_table|>
  select(State,Class,Party,Senator)|>
  mutate(Party=case_when(Party=="Democratic(DFL)[p]"~"DEM",
                         Party=="Democratic"~"DEM",
                         Party=="Republican"~"REP",
                         str_starts(Party, "Independent") ~ "IND"
                         ))
saveRDS(senators_table_clean,"./data/current_senators.rds")
senate_not_run<-senators_table_clean|>
  filter(!startsWith(as.character(Class), "2024C"))
saveRDS(senate_not_run,"./data/senator_not_running_2024.rds")
senate_run<-senators_table_clean|>
  filter(startsWith(as.character(Class), "2024C"))
saveRDS(senate_run,"./data/senator_running_2024.rds")

#######################################################################################
# Extract poll information from Five thirty eight
url<-"https://projects.fivethirtyeight.com/polls-page/data/senate_polls_historical.csv"
url_current<-"https://projects.fivethirtyeight.com/polls/data/senate_polls.csv"
raw_dat <- read_csv(url)
raw_dat_current <- read_csv(url_current)
#######################################################################################
# Identify the two candidate that are the most "popular" according to the polls
popular_candidate<-raw_dat |>
  # Step 1: Filter and clean the population column
  filter(population %in% c("lv", "rv", "a")) |>
  mutate(population = factor(population, levels = c("lv", "rv", "a"))) |>
  filter(!is.na(sample_size))|>
  filter(!is.na(pct))|>
  # Step 2: Group by poll_id and answer (candidate), calculate avg_pct
  group_by(state,cycle,poll_id,sample_size, answer) |>
  summarise(avg_pct = mean(pct,na.rm = TRUE),.groups="drop") |>
  
  # Step 3: Calculate weighted avg_pct by multiplying with sample_size
  mutate(weighted_pct = avg_pct * sample_size) |> 
  group_by( state,cycle,answer) |>
  summarise(popularity = sum(weighted_pct),.groups="drop") |>
  group_by(cycle,state)|>
  arrange(desc(popularity))|> 
  slice_head(n=2)|>
  ungroup()
  # save data
saveRDS(popular_candidate,"./data/popular_candidate.rds")
#######################################################################################
# Extract polls relevant to the two most popular candidate
clean_dat<-raw_dat|>
  filter(population%in% c("lv","rv","a"))|>
  mutate(population=factor(population,levels=c("lv","rv","a")),
         start_date=mdy(start_date))|>
  select(poll_id,
         question_id,
         state,
         pollster,
         numeric_grade,
         sample_size,
         pct,
         answer,
         population,
         party,cycle,start_date)|>
  right_join(popular_candidate,by = c("state", "cycle", "answer"))|> 
  group_by(poll_id,
           state,
           pollster,
           numeric_grade,
           sample_size,
           answer,
           population,
           party,cycle,start_date) |>
  summarise(avg_pct=mean(pct,na.rm = TRUE),.groups = "drop")|>
  group_by(poll_id,
           state,
           cycle,
           population,
           sample_size)|>
    mutate(n=n())|>
  filter(n==2)|> # filter out polls that does not have both candidate
  mutate(
    answer=paste(answer[1]," vs. ",answer[2]),
    spread = avg_pct[1]-avg_pct[2],
    type=paste(party[1],"-",party[2])
  ) |> 
  ungroup() |> 
  select(-c(party, avg_pct))|>
  unique()|>
  mutate(
    spread = ifelse(type == "REP - DEM", spread * -1, spread),
    answer = ifelse(type == "REP - DEM", 
                    paste0(trimws(strsplit(answer, "vs\\.")[[1]][2]), " vs ", 
                           trimws(strsplit(answer, "vs\\.")[[1]][1])), 
                    answer),
    type = ifelse(type == "REP - DEM", "DEM - REP", type)
  )
saveRDS(clean_dat,"./data/clean_dat.rds")
  

  
  
  
  
  
  
  
  
  
  
  
  

    
    

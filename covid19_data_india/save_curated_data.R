setwd("/Users/biplabendudas/Documents/GitHub/homepage/covid19_data_india")

rm(list = ls())
# Curate India’s data -----------------------------------------------------

## Last updated on:
## Data available until
current.date <- "19/04/20"

# Data downloaded from: https://www.kaggle.com/sudalairajkumar/covid19-in-india/version/7
# Download timestamp: 0225 hrs, 20th March 2020
urlfile <- "https://github.com/biplabendu/homepage/raw/master/covid19_data_india/covid_19_india_19Apr20.csv"

india <- read.csv(url(urlfile),
                  header = T, stringsAsFactors = F)
# save(india, file="india_data.csv")
# load(file="india_data.csv")

# india %>% glimpse()


india$Date <- as.Date(india$Date, "%d/%m/%y")

# Incorrect data entry | Fix it
india[india$State.UnionTerritory == "Maharashtra" & 
        india$Date == "2020-03-11",]$total_confirmed <- 5

## Different names for the same state | Fix this
# 1. Pondicherry = Puducherry
# 2. Union Territory of Chandigarh = Chandigarh
# 3. Chattisgarh = Chhattisgarh
# 4. Union Territory of Jammu and Kashmir = Jammu and Kashmir
# 5. Union Territory of Ladakh = Ladakh

india[india$State.UnionTerritory == "Pondicherry",]$State.UnionTerritory <- "Puducherry"
india[india$State.UnionTerritory == "Union Territory of Chandigarh",]$State.UnionTerritory <- "Chandigarh"
india[india$State.UnionTerritory == "Chattisgarh",]$State.UnionTerritory <- "Chhattisgarh"
india[india$State.UnionTerritory == "Union Territory of Jammu and Kashmir",]$State.UnionTerritory <- "Jammu and Kashmir"
india[india$State.UnionTerritory == "Union Territory of Ladakh",]$State.UnionTerritory <- "Ladakh"

library(tidyverse)

allData_india <-
  india %>%
  rename("State_or_Province" = State.UnionTerritory, date = Date) %>%
  mutate("Country" = "India") %>%
  rename(Confirmed = total_confirmed) %>%
  arrange(State_or_Province, date) %>% 
  ## need to fix this - filter to keep the right copy since there are multiple entries
  distinct(State_or_Province, Country, date, .keep_all = T) %>%   
  rename(CumConfirmed = Confirmed,
         CumDeaths = Deaths,
         CumRecovered = Cured) %>%
  dplyr::select(State_or_Province, Country, date, CumConfirmed, CumRecovered, CumDeaths) %>% 
  as_tibble()

# allData_india %>% 
#   filter(date == "2020-03-13") %>% 
#   View()
# 
# allData_india %>% 
#   group_by(State_or_Province,date) %>% 
#   summarise_if(is.numeric, sum, na.rm=TRUE) %>% 
#   View()

# head(allData)


# Format the data for dates 22 Jan 2020 to current --------------------------------------------------------

## Code borrowed from: https://biostats.w.uib.no/7-building-a-dataframe-from-a-bunch-of-vectorsseries/
indian_states <- levels(factor(allData_india$State_or_Province)) %>% as.character()

dates <- rep(seq(as.Date("22/01/20", format = "%d/%m/%y"), 
                 as.Date(current.date, format = "%d/%m/%y"), 
                 by="days"), length(indian_states))
country = rep("India", length(dates))
states = rep(indian_states, each=length(dates)/length(indian_states))
confirmed = as.integer(rep(0, length(dates)))
recovered = as.integer(rep(0, length(dates)))
deaths = as.integer(rep(0, length(dates)))

df <- data.frame(State_or_Province = states, 
                 Country = country,
                 date = dates,
                 CumConfirmed = confirmed,
                 CumRecovered = recovered,
                 CumDeaths = deaths,
                 stringsAsFactors = F)

df <- df %>%
  dplyr::select(State_or_Province:date) %>% 
  left_join(allData_india, by = c("State_or_Province", "Country", "date"))

df[is.na(df)] <- 0  

df <- df %>% 
  filter(!(State_or_Province == ""))

df.curated.india <- df


# Curate JHU data ---------------------------------------------------------

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")


# Run it for every update cycle [1] -------------------------------------------

minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName, columnName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
      dplyr::select(-Lat, -Long) %>%
      pivot_longer(-(1:2), names_to="date", values_to=columnName) %>%
      mutate(
        date=as.Date(date, format="%m/%d/%y"),
        `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
        `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    save(data, file=fileName)
  } else {
    load(file=fileName)
  }
  return(data)
}

# loadData = function(fileName, columnName) {
#   load(file=fileName)
#   return(data)
# }

allData =
  loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(loadData("time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
  inner_join(loadData("time_series_covid19_recovered_global.csv", "CumRecovered"))

df.curated.global <- allData


# Let’s push the updated file to GitHub -----------------------------------

names(df.curated.india) <- names(df.curated.global)

df.curated <-  rbind(df.curated.global %>% filter(!(`Country/Region` == "India")), df.curated.india)

write.csv(df.curated, file="./curated_data/curated_data_BD.csv")
write.csv(df.curated.india, file = "./curated_data/india_curated_data_BD.csv")
write.csv(df.curated.global, file = "./curated_data/global_curated_data_BD.csv")


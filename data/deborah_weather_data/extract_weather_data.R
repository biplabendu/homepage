### Install the following packages
pacman::p_load(rvest, httr, conflicted)
pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(janitor)

# path to repository
path_to_repo <- "/Users/biplabendudas/Documents/05_postdoc/Deborah_Gordon/04_research/2022/03_timecourse_foraging"

# specify function preference
conflict_prefer("GET", "httr")
conflict_prefer("filter", "dplyr")

## specify column names
column.names <- c("time",
                  "temp_F",
                  "dew_point_F",
                  "rH",
                  "wind",
                  "speed_mph",
                  "gust_mph",
                  "pressure_inch",
                  "precip_rate_inch",
                  "precip_accum_inch",
                  "uv",
                  "solar")

# which station?
# station <- "KAZSANSI43" # does not have data for 2015
# station <- "KAZSANSI39" # does not have data for 2015
# station <- "KAZPORTA3" # does not have data for all days in August, 2015 :(
### Look here for other options for stations
# https://www.wunderground.com/wundermap?lat=32.267&lon=-109.227

## Deborah's recommendation
station <- "KNMRODEO1"



# which days?
b.dat <- read.csv(file=paste0(path_to_repo,"/01_data/all_data_pagliara_nova_billu2022.csv"),
                  header=T, stringsAsFactors = F)
days <- b.dat %>% pull(day) %>% unique()
# days <- seq(ymd('2022-08-15'), ymd('2022-08-30'), by = "1 days") %>% as.character()
# get data
dat.list <- list()
for(i in 1:length(days)) {
  theurl <- paste0("https://www.wunderground.com/dashboard/pws/",
                   station,"/table/",
                   days[[i]],"/", days[[i]],"/daily")
  which.day <- days[[i]]
  tables<-
    theurl %>% 
    GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
    read_html() %>% 
    html_nodes("table")
  dat.list[[i]] <-
    html_table(tables[4], fill = TRUE)[[1]] %>% 
    mutate_all(as.character) %>% 
    slice(-1)
  colnames(dat.list[[i]]) <- column.names
  dat.list[[i]] <- 
    dat.list[[i]] %>% 
    mutate(date = days[[i]]) %>% 
    select(date, everything())
}

# put all the data together into a table, row-wise
dat <-
  do.call("rbind", dat.list) %>% 
    as_tibble() %>% 
    mutate_all(as.character) %>% 
    mutate(temp_F = parse_number(temp_F),
           dew_point_F = parse_number(dew_point_F),
           rH = parse_number(rH)) %>% 
    mutate(time = format(strptime(time, "%I:%M %p"), "%H:%M")) %>% 
    mutate_at(c("speed_mph","gust_mph",
                "pressure_inch","precip_rate_inch","precip_accum_inch",
                "uv","solar"), parse_number)
    

# save the data to a csv file
write.csv(dat,
          file = paste0(path_to_repo, "/01_data/all_climate_data_pagliara_nova_billu2022.csv"),
          row.names=F)

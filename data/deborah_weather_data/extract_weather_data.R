### Install the following packages
pacman::p_load(rvest, httr, conflicted)
pacman::p_load(tidyverse)
pacman::p_load(lubridate)

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
# days <- b.dat %>% pull(day) %>% unique()
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

# # save the data to a csv file
# write.csv(dat,
#           file = paste0(path_to_repo, "/01_data/all_climate_data_pagliara_nova_billu2022.csv"),
#           row.names=F)



### Extract more days around the observation days
yrs <- dat3$yr %>% unique()
plusminus <- 15
all.d <- list()
for (i in 1:length(yrs)) {
  d <- dat3 %>% filter(yr==yrs[i]) %>% pull(date) %>% unique() %>% as.Date()
  # get 15 days prior to the start and 15 days after the end of the sampling period
  start.d <- min(d)-plusminus
  end.d <- max(d)+plusminus
  all.d[[i]] <- seq(start.d, end.d, by=1)
}
names(all.d) <- yrs
sapply(all.d, length)

dat.list2 <- list()
k <- 1
for(i in 1:length(all.d)) {
  ds <- all.d[[i]] %>% as.character()
  for(j in 1:length(ds)) {
    theurl <- paste0("https://www.wunderground.com/dashboard/pws/",
                     station,"/table/",
                     ds[[j]],"/", ds[[j]],"/daily")
    tables<-
      theurl %>% 
      GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
      read_html() %>% 
      html_nodes("table")
    if(length(tables)==4) {
      dat.list2[[k]] <-
      html_table(tables[4], fill = TRUE)[[1]] %>% 
        mutate_all(as.character) %>% 
        slice(-1)
      colnames(dat.list2[[k]]) <- column.names
      dat.list2[[k]] <- 
        dat.list2[[k]] %>% 
        mutate(date = ds[[j]]) %>% 
        select(date, everything())
      print(paste("Completed:", ds[[j]]))
      k <- k+1
    }
  }
}

# put all the data together into a table, row-wise
all.dat <-
  do.call("rbind", dat.list2) %>% 
  as_tibble() %>% 
  mutate_all(as.character) %>% 
  mutate(temp_F = parse_number(temp_F),
         dew_point_F = parse_number(dew_point_F),
         rH = parse_number(rH)) %>% 
  mutate(time = format(strptime(time, "%I:%M %p"), "%H:%M")) %>% 
  mutate_at(c("speed_mph","gust_mph",
              "pressure_inch","precip_rate_inch","precip_accum_inch",
              "uv","solar"), parse_number)

## save all the climate data to a SQL database
climate.db <- DBI::dbConnect(RSQLite::SQLite(),
                             paste0(path_to_repo, "/01_data/all_climate_data.db"))
RSQLite::dbWriteTable(climate.db, "all_climate_data", all.dat)
RSQLite::dbWriteTable(climate.db, "obs_day_climate_data", dat)

all.dat2 <- 
  all.dat%>% 
  select(date, time, temp_F, rH) %>% 
  distinct() %>% 
  group_by(date) %>% 
  mutate(yr = str_split(date, "-")[[1]][1]) %>% 
  ungroup() %>% 
  group_by(yr) %>% 
  summarize(
    min_temp = summary(temp_F)[1],
    q1_temp = summary(temp_F)[2],
    median_temp = summary(temp_F)[3],
    mean_temp = summary(temp_F)[4],
    q3_temp = summary(temp_F)[5],
    max_temp = summary(temp_F)[6],
    
    min_rH = summary(rH)[1],
    q1_rH = summary(rH)[2],
    median_rH = summary(rH)[3],
    mean_rH = summary(rH)[4],
    q3_rH = summary(rH)[5],
    max_rH = summary(rH)[6]
  ) %>% 
  mutate_at(vars(matches("temp")), as.numeric) %>% 
  mutate_at(vars(matches("rH")), as.numeric) 

all.dat3 <-
  dat %>% select(date,time,temp_F,rH) %>% 
  
  group_by(date) %>% mutate(yr=str_split(date,"-")[[1]][1]) %>% 
  ungroup() %>% 
  group_by(yr,date) %>% 
  mutate(dmax_temp=max(temp_F),
         dmax_rH=max(rH)) %>% 
  ungroup() %>% 
  select(-time,-temp_F,-rH) %>% 
  distinct() %>% 
  left_join(all.dat2, "yr") %>% 
  select(date,yr,dmax_rH,dmax_temp,q1_rH,q3_rH) %>% 
  group_by(date,yr) %>%
  mutate(rH_group = ifelse(dmax_rH <= q1_rH, "dry-day",
                           ifelse(dmax_rH >= q3_rH, "wet-day", "average-day"))) %>%
  ungroup() %>% 
  mutate(rH_group = as.factor(rH_group))

RSQLite::dbWriteTable(climate.db, "rH_group_climate_data", all.dat3)
DBI::dbDisconnect(climate.db)

# bd.saveplot(
  # name="weather_data",
  # width = 12, height = 25,
  all.dat2 %>% 
    pivot_longer(cols = min_temp:max_rH,
                 names_to = "name",
                 values_to = "value") %>%
    ungroup() %>% 
    group_by(name) %>% 
    mutate(name2 = str_split(name, "_")[[1]][2]) %>% 
    
    ggplot(aes(x=as.factor(yr), y=value)) +
    geom_boxplot(aes(fill=as.factor(name2)), 
                 size=1, alpha=0.75, 
                 outlier.colour = NA) +
    
    geom_jitter(data=all.dat3 %>% pivot_longer(dmax_rH:dmax_temp, names_to = "name", values_to = "value") %>% 
                  group_by(name) %>% mutate(name2 = str_split(name, "_")[[1]][2]),
                col="black",
               size=4, alpha=0.6, width = 0.3) +
    
    facet_wrap(~as.factor(name2), nrow = 2, scales = "free_y") +
    theme_Publication(base_size = 20) +
    scale_color_manual(values = c("orange", "red")) +
    scale_fill_manual(values = c("orange", "red")) +
    scale_shape_manual(values = c(17,17,16, 16,16,16)) +
    # coord_flip() +
    theme(legend.position = "none") +
    labs(x="",
         y="",
         subtitle = "humidity %rH and, \ntemperature ºF [1]",
         caption = "[1] boxplot shows variation over 31-42 days,
         eaach year, during which we sampled our
         timecourse foraging data (points)")
# )

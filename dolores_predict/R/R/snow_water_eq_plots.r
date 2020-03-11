## Libraries
library(tidyverse)
library(lubridate)
library(RNRCS)

theme_set(theme_minimal())

## Get the Data
meta_data<-grabNRCS.meta(ntwrks = c("SNTL", "SNTLT", "SCAN"))

dolores_sites<-meta_data[[1]]%>%
  as_tibble()%>%
  filter(state =="CO", str_detect(huc, "140300"))%>%
  mutate(site_id_num = as.numeric(str_match_all(site_id, "[0-9]+")))

dolores_site_ids<-dolores_sites%>%
  pull(site_id_num)%>%
  unlist()%>%
  as.numeric()

get_snotl_data<-function(site_id){
  grabNRCS.data(network = "SNTL", 
                site_id = site_id, 
                timescale = "daily", 
                DayBgn = '1985-01-01',
                DayEnd = Sys.Date()
  )%>%
    as_tibble()%>%
    mutate(site_id_num = site_id)
}

all_sntl_data<-lapply(dolores_site_ids, get_snotl_data)%>%
  bind_rows()


## Explore the Data
se_site<-all_sntl_data%>%
  left_join(select(dolores_sites, site_id_num, site_id), by= "site_id_num")%>%
  select(Date, Snow.Depth..in..Start.of.Day.Values, Snow.Water.Equivalent..in..Start.of.Day.Values, site_id_num, site_id)%>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date)
  )%>%
  rename(snow_depth = 2, snow_water_eq=3)

all_years<-se_site%>%
  mutate(day = yday(Date),
         plot_date = ifelse(day>225, day-365, day), 
         winter_year= ifelse(plot_date<=0, paste0(Year, "-", Year+1), paste0(Year-1, "-", Year)))

current_year<-all_years%>%
  filter(winter_year == "2019-2020")

past_years<-all_years%>%
  filter(winter_year!= "2019-2020")


ggplot()+
  geom_line(aes(plot_date, snow_water_eq, group = all_years, color = alpha("grey", 0.7)), data = all_years)+
  geom_line(aes(plot_date, snow_water_eq, group = current_year, color = "red", size = 2), data = current_year)+
  facet_wrap("site_id_num")

## code to prepare `pod` dataset goes here

library(tidyverse)
library(tidyselect)
library(lubridate)

path = file.path("inst", "extdata", "Denfinal")

inc_pv_cond = F

# file names have different integers at end depending on batch release
files <- list.files(path)
files <- c("demand", "weather", "pv") %>% 
  set_names() %>% 
  map(~ files[grep(., files)])

demand_df <- read_csv(
  file.path(path, files$demand),
  col_types = cols(
    datetime = col_datetime(),
    demand_MW = col_double()
  )
) %>% 
  rename(
    demand_mw = demand_MW
  )

weather_df <- read_csv(
  file.path(path, files$weather),
  col_types = cols(
    datetime = col_datetime(),
    temp_location3 = col_double(),
    humidity = col_double()
  )
) 
#select(datetime, 
        # matches(paste0("[", paste0(locations, collapse=""), "]{1}$")))



pv_df <- read_csv(
  file.path(path, files$pv),
  col_types = cols(
    datetime = col_datetime(format = ""),
    `irradiance_Wm-2` = col_double(),
    pv_power_mw = col_double(),
    panel_temp_C = col_double()
  )
) %>% 
  rename(
    irradiance_wm2 = `irradiance_Wm-2`,
    panel_temp_c = panel_temp_C
  )

if (!inc_pv_cond) pv_df <- select(pv_df, datetime, pv_power_mw)
 
pod <- demand_df %>% 
  full_join(pv_df, by = "datetime") %>% 
  full_join(weather_df, by = "datetime") %>% 
  arrange(datetime) %>% 
  filter(datetime >= min(demand_df$datetime)) %>%   # remove pre-demand data
  select(datetime, sort(peek_vars()))


usethis::use_data(pod, overwrite = TRUE)

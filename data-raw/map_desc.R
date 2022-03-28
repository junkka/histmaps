# map_desc.R
library(dplyr)

load("data/geom_sp.rda")

d <- slot(geom_sp, "data") %>% tbl_df()

# by type get min start max end and n units n boundaries

units <- d %>% select(admin_id, type_id) %>% 
  distinct() %>% ungroup() %>% 
  count(type_id) %>% 
  rename(units = n)

bounds <- d %>% count(type_id) %>% rename(bounds = n)

# 1600 - 1990
limits <- d %>% 
  group_by(type_id) %>% 
  summarise(
    start = min(start),
    end = max(end)
  ) %>% 
  mutate(
    start = ifelse(start == 0, 1600, start),
    end   = ifelse(end == 9999, 1990, end)
  )

map_desc <- cbind(units, bounds[,2], limits[,2:3])

save(map_desc, file = "data/map_desc.rda")
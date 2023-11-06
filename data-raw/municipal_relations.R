

pdata <- geom_sp %>% filter(type_id == "municipal") %>% 
  filter(start <= 1970, end >= 1970, str_detect(ref_code, "SE/14")) %>% 
  mutate(year = 1970, ind = ifelse(geom_id == 4708, 1, 0))

pdata2 <- geom_sp %>% filter(type_id == "municipal") %>% 
  filter(start <= 1973, end >= 1973, str_detect(ref_code, "SE/14")) %>% 
  mutate(year = 1971, ind = ifelse(geom_id == 7592, 1, 0))

pd2 <- bind_rows(pdata, pdata2)


ggplot(pd2) + 
  geom_sf(aes(fill = ind)) +
  facet_wrap(~year)

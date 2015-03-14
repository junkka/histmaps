<!-- README.md is generated from README.Rmd. Please edit that file -->
Swedish historical administrative maps
======================================

A R data package of Swedish historical administrative boundaries for parishes and counties 1600-1990.

County map
----------

``` r
library(histmaps)
map <- hist_boundaries(1800, "county")
plot(map)
```

![](README_files/figure-markdown_github/county_ex-1.png)

Parish map
----------

``` r
library(ggplot2)
library(dplyr)

p_map <- hist_boundaries("1866-06-06", "parish", "df")

st_map <- p_map %>% filter(county < 3)
ggplot(st_map, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgrey", color = "black") + coord_equal() + 
  theme_minimal()
```

![](README_files/figure-markdown_github/parish_ex-1.png)

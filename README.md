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
library(knitr)
library(ggplot2)
library(dplyr)

p_map <- hist_boundaries("1866-06-06", "parish", "df")

st_map <- p_map %>% filter(county < 3)
ggplot(st_map, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgrey", color = "black") + coord_equal() + 
  theme_minimal()
```

![](README_files/figure-markdown_github/parish_ex-1.png)

Period map
----------

As parishes changes boundaries over the course of history a given map a certain year is not representative of the boundaries another year. To create a map for a period the parishes need to be aggregated to the lowest common denominator for that period. You can do this by supplying a date range to `hist_boundaries`.

``` r
period_map <- hist_boundaries(c(1900, 1920), "df") 
```

The function returns a list where the first object is the map data and the second is a lookup table for aggregating your data to the new artificial parish boundaries.

``` r
plot(period_map$map)
```

![](README_files/figure-markdown_github/period_plot-1.png)

``` r
kable(head(period_map$lookup))
```

|     nadkod|  geomid|
|----------:|-------:|
|  148010000|    1973|
|  148012000|    1976|
|  228102000|     365|
|  228401000|     407|
|  242501000|     290|
|  242502000|     290|

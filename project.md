project
================
2023-04-20

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.1 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(sf)
```

    ## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
library(tmap)

accident <- read.csv("data/dlst_red.csv")
accident %>% group_by(mlong) %>% summarise(n())
```

    ## # A tibble: 20 × 2
    ##    mlong `n()`
    ##    <dbl> <int>
    ##  1 -7.75  3436
    ##  2 -7.25 17234
    ##  3 -6.75 24090
    ##  4 -6.25 34355
    ##  5 -5.75 51477
    ##  6 -5.25 61754
    ##  7 -4.75 65173
    ##  8 -4.25 61761
    ##  9 -3.75 61761
    ## 10 -3.25 78848
    ## 11 -2.75 72140
    ## 12 -2.25 68628
    ## 13 -1.75 68638
    ## 14 -1.25 51324
    ## 15 -0.75 44444
    ## 16 -0.25 41112
    ## 17  0.25 44556
    ## 18  0.75 30846
    ## 19  1.25 17099
    ## 20  1.75 10278

``` r
accident %>% group_by(mlat) %>% summarise(n())
```

    ## # A tibble: 23 × 2
    ##     mlat `n()`
    ##    <dbl> <int>
    ##  1  49.8 27300
    ##  2  50.2 37579
    ##  3  50.8 51270
    ##  4  51.2 44455
    ##  5  51.8 47852
    ##  6  52.2 54693
    ##  7  52.8 54786
    ##  8  53.2 47992
    ##  9  53.8 48006
    ## 10  54.2 44577
    ## # … with 13 more rows

``` r
uk_district <- st_read("_UK_County_Districts/UK_DISTRICTS_COUNTIES_CENSUS2011.shp", quiet = TRUE)
uk_region <- st_read("UK_region/NUTS1_Jan_2018_SGCB_in_the_UK.shp", quiet = TRUE)
epsg_wgs84 <- 4326
epsg_uk <- 27700
uk_region <- uk_region %>% st_transform(epsg_uk)
uk_region$nuts118nm
```

    ##  [1] "North East (England)"     "North West (England)"    
    ##  [3] "Yorkshire and The Humber" "East Midlands (England)" 
    ##  [5] "West Midlands (England)"  "East of England"         
    ##  [7] "London"                   "South East (England)"    
    ##  [9] "South West (England)"     "Wales"                   
    ## [11] "Scotland"                 "Northern Ireland"

``` r
uk_district <- uk_district %>% st_transform(epsg_uk)
ggplot(uk_region) + geom_sf()
```

![](project_files/figure-gfm/UKmap_import-1.png)<!-- -->

``` r
ggplot(uk_district) + geom_sf()
```

![](project_files/figure-gfm/UKmap_import-2.png)<!-- -->

``` r
accident_map <- accident %>% st_as_sf(coords = c("mlong", "mlat")) %>%
 st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk)
accident_uk <- accident_map %>% filter(lengths(st_intersects(., uk_region)) > 0)
ggplot() + geom_sf(data = uk_region) + geom_sf(data = accident_uk, alpha = 0.2)
```

![](project_files/figure-gfm/uk_region_accident-1.png)<!-- -->

``` r
accident_2012 <- accident %>% 
  filter(year == 2012) %>%
  filter((date >= as.Date("2012-03-25") & date <= as.Date("2012-03-31")) | 
           (date >= as.Date("2012-10-28") & date <= as.Date("2012-11-03")))
```

``` r
accident_2012 <- accident_2012 %>% filter(!(all_accid == 0))
accident_2012 <- accident_2012 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_region_2012 <- uk_region %>% st_join(accident_2012) %>% group_by(nuts118nm) %>% mutate(num = n())
tm_shape(uk_region_2012) + 
  tm_polygons(col="num", palette ="-RdYlBu",breaks = c(0, 30, 60, 90, 120, 150, 180), title="Accidents")  +
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2012_map-1.png)<!-- -->

``` r
acci_ratio <- function(x, sum){
  (x/14)/(sum/365)*100
}
```

``` r
acci_2012 <- accident %>% 
  filter(year == 2012)

acci_2012 <- acci_2012 %>% filter(!(all_accid == 0))
acci_2012 <- acci_2012 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_ratio_2012 <- uk_region %>% 
  st_join(acci_2012) %>% 
  group_by(nuts118nm) %>% 
  mutate(total = n()) %>% 
  filter((date >= as.Date("2012-03-25") & date <= as.Date("2012-03-31")) | 
           (date >= as.Date("2012-10-28") & date <= as.Date("2012-11-03"))) %>%
  group_by(nuts118nm) %>% 
  mutate(num = n()) %>%
  mutate(ratio = acci_ratio(num, total))
  
tm_shape(uk_ratio_2012) + 
  tm_polygons(col="ratio", palette ="-RdYlBu", title="Accidents Ratio") + 
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2012_ratio-1.png)<!-- -->

``` r
accident_2013 <- accident %>% 
  filter(year == 2013) %>%
  filter((date >= as.Date("2013-03-31") & date <= as.Date("2013-04-06")) | 
           (date >= as.Date("2013-10-27") & date <= as.Date("2013-11-02")))
```

``` r
accident_2013 <- accident_2013 %>% filter(!(all_accid == 0))
accident_2013 <- accident_2013 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_region_2013 <- uk_region %>% st_join(accident_2013) %>% group_by(nuts118nm) %>% mutate(num = n())
tm_shape(uk_region_2013) + 
  tm_polygons(col="num", palette="-RdYlBu",breaks = c(0, 30, 60, 90, 120, 150, 180), title="Accidents") +
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2013_map-1.png)<!-- -->

``` r
acci_2013 <- accident %>% 
  filter(year == 2013)

acci_2013 <- acci_2013 %>% filter(!(all_accid == 0))
acci_2013 <- acci_2013 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_ratio_2013 <- uk_region %>% 
  st_join(acci_2013) %>% 
  group_by(nuts118nm) %>% 
  mutate(total = n()) %>% 
  filter((date >= as.Date("2013-03-31") & date <= as.Date("2013-04-06")) | 
           (date >= as.Date("2013-10-27") & date <= as.Date("2013-11-02"))) %>%
  group_by(nuts118nm) %>% 
  mutate(num = n()) %>%
  mutate(ratio = acci_ratio(num, total))
  
tm_shape(uk_ratio_2013) + 
  tm_polygons(col="ratio", palette ="-RdYlBu", title="Accidents Ratio") + 
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2013_ratio-1.png)<!-- -->

``` r
accident_2014 <- accident %>% 
  filter(year == 2014) %>%
  filter((date >= as.Date("2014-03-30") & date <= as.Date("2014-04-05")) | 
           (date >= as.Date("2014-10-26") & date <= as.Date("2014-11-01")))
```

``` r
accident_2014 <- accident_2014 %>% filter(!(all_accid == 0))
accident_2014 <- accident_2014 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_region_2014 <- uk_region %>% st_join(accident_2014) %>% group_by(nuts118nm) %>% mutate(num = n())
tm_shape(uk_region_2014) + 
  tm_polygons(col="num", palette="-RdYlBu",breaks = c(0, 30, 60, 90, 120, 150, 180), title="Accidents") +
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2014_map-1.png)<!-- -->

``` r
acci_2014 <- accident %>% 
  filter(year == 2014)

acci_2014 <- acci_2014 %>% filter(!(all_accid == 0))
acci_2014 <- acci_2014 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_ratio_2014 <- uk_region %>% 
  st_join(acci_2014) %>% 
  group_by(nuts118nm) %>% 
  mutate(total = n()) %>% 
  filter((date >= as.Date("2014-03-30") & date <= as.Date("2014-04-05")) | 
           (date >= as.Date("2014-10-26") & date <= as.Date("2014-11-01"))) %>%
  group_by(nuts118nm) %>% 
  mutate(num = n()) %>%
  mutate(ratio = acci_ratio(num, total))
  
tm_shape(uk_ratio_2014) + 
  tm_polygons(col="ratio", palette ="-RdYlBu", title="Accidents Ratio") + 
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2014_ratio-1.png)<!-- -->

``` r
accident_2015 <- accident %>% 
  filter(year == 2015) %>%
  filter((date >= as.Date("2015-03-29") & date <= as.Date("2015-04-04")) | 
           (date >= as.Date("2015-10-25") & date <= as.Date("2015-10-31")))
```

``` r
accident_2015 <- accident_2015 %>% filter(!(all_accid == 0))
accident_2015 <- accident_2015 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_region_2015 <- uk_region %>% st_join(accident_2015) %>% group_by(nuts118nm) %>% mutate(num = n())
tm_shape(uk_region_2015) + 
  tm_polygons(col="num", palette="-RdYlBu",breaks = c(0, 30, 60, 90, 120, 150, 180), title="Accidents") +
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2015_map-1.png)<!-- -->

``` r
acci_2015 <- accident %>% 
  filter(year == 2015)

acci_2015 <- acci_2015 %>% filter(!(all_accid == 0))
acci_2015 <- acci_2015 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_ratio_2015 <- uk_region %>% 
  st_join(acci_2015) %>% 
  group_by(nuts118nm) %>% 
  mutate(total = n()) %>% 
  filter((date >= as.Date("2015-03-29") & date <= as.Date("2015-04-04")) | 
           (date >= as.Date("2015-10-25") & date <= as.Date("2015-10-31"))) %>%
  group_by(nuts118nm) %>% 
  mutate(num = n()) %>%
  mutate(ratio = acci_ratio(num, total))
  
tm_shape(uk_ratio_2015) + 
  tm_polygons(col="ratio", palette ="-RdYlBu", title="Accidents Ratio") + 
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2015_ratio-1.png)<!-- -->

``` r
accident_2016 <- accident %>% 
  filter(year == 2016) %>%
  filter((date >= as.Date("2016-03-27") & date <= as.Date("2016-04-02")) | 
           (date >= as.Date("2016-10-30") & date <= as.Date("2016-11-05")))
```

``` r
accident_2016 <- accident_2016 %>% filter(!(all_accid == 0))
accident_2016 <- accident_2016 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_region_2016 <- uk_region %>% st_join(accident_2016) %>% group_by(nuts118nm) %>% mutate(num = n())
tm_shape(uk_region_2016) + 
  tm_polygons(col="num", palette="-RdYlBu",breaks = c(0, 30, 60, 90, 120, 150, 180), title="Accidents") +
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2016_map-1.png)<!-- -->

``` r
acci_2016 <- accident %>% 
  filter(year == 2016)

acci_2016 <- acci_2016 %>% filter(!(all_accid == 0))
acci_2016 <- acci_2016 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_ratio_2016 <- uk_region %>% 
  st_join(acci_2016) %>% 
  group_by(nuts118nm) %>% 
  mutate(total = n()) %>% 
  filter((date >= as.Date("2016-03-27") & date <= as.Date("2016-04-02")) | 
           (date >= as.Date("2016-10-30") & date <= as.Date("2016-11-05"))) %>%
  group_by(nuts118nm) %>% 
  mutate(num = n()) %>%
  mutate(ratio = acci_ratio(num, total))
  
tm_shape(uk_ratio_2016) + 
  tm_polygons(col="ratio", palette ="-RdYlBu", title="Accidents Ratio") + 
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2016_ratio-1.png)<!-- -->

``` r
accident_2017 <- accident %>% 
  filter(year == 2017) %>%
  filter((date >= as.Date("2017-03-26") & date <= as.Date("2017-04-01")) | 
           (date >= as.Date("2017-10-29") & date <= as.Date("2017-11-04")))
```

``` r
accident_2017 <- accident_2017 %>% filter(!(all_accid == 0))
accident_2017 <- accident_2017 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_region_2017 <- uk_region %>% st_join(accident_2016) %>% group_by(nuts118nm) %>% mutate(num = n())
tm_shape(uk_region_2017) + 
  tm_polygons(col="num", palette="-RdYlBu",breaks = c(0, 30, 60, 90, 120, 150, 180), title="Accidents") +
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2017_map-1.png)<!-- -->

``` r
acci_2017 <- accident %>% 
  filter(year == 2017)

acci_2017 <- acci_2017 %>% filter(!(all_accid == 0))
acci_2017 <- acci_2017 %>% st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0)
uk_ratio_2017 <- uk_region %>% 
  st_join(acci_2017) %>% 
  group_by(nuts118nm) %>% 
  mutate(total = n()) %>% 
  filter((date >= as.Date("2017-03-26") & date <= as.Date("2017-04-01")) | 
           (date >= as.Date("2017-10-29") & date <= as.Date("2017-11-04"))) %>%
  group_by(nuts118nm) %>% 
  mutate(num = n()) %>%
  mutate(ratio = acci_ratio(num, total))
  
tm_shape(uk_ratio_2017) + 
  tm_polygons(col="ratio", palette ="-RdYlBu", title="Accidents Ratio") + 
  tm_shape(uk_district) + 
  tm_borders(lwd = 0.1)
```

![](project_files/figure-gfm/2017_ratio-1.png)<!-- -->

``` r
accident_dls <- accident %>% filter(((year == 2012) & (date >= as.Date("2012-03-25") & date <= as.Date("2012-03-31")) | 
           (date >= as.Date("2012-10-28") & date <= as.Date("2012-11-03"))) |
             (year == 2013) & (date >= as.Date("2013-03-31") & date <= as.Date("2013-04-06")) | 
           (date >= as.Date("2013-10-27") & date <= as.Date("2013-11-02")) |
             (year == 2014) & (date >= as.Date("2014-03-30") & date <= as.Date("2014-04-05")) | 
           (date >= as.Date("2014-10-26") & date <= as.Date("2014-11-01")) |
             (year == 2015) & (date >= as.Date("2015-03-29") & date <= as.Date("2015-04-04")) | 
           (date >= as.Date("2015-10-25") & date <= as.Date("2015-10-31")) |
             (year == 2016) & (date >= as.Date("2016-03-27") & date <= as.Date("2016-04-02")) | 
           (date >= as.Date("2016-10-30") & date <= as.Date("2016-11-05")) |
             (year == 2017) & (date >= as.Date("2017-03-26") & date <= as.Date("2017-04-01")) | 
           (date >= as.Date("2017-10-29") & date <= as.Date("2017-11-04"))
)
accident_dls <- accident_dls %>% filter(!(all_accid == 0)) %>%
  st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0) %>%
    st_join(uk_region)
accident_dls <- accident_dls %>% group_by(nuts118nm, year) %>% mutate(sum_dls = n())

ggplot(accident_dls,aes(x=year, y=sum_dls)) + 
  geom_point(postion = "jitter") + 
  labs(title = "Accidents during DST in Different Regions",
       x = "Year",
       y = "Number of Accidents") +
  geom_smooth() + 
  facet_wrap(~ nuts118nm, nrow = 2)
```

    ## Warning in geom_point(postion = "jitter"): Ignoring unknown parameters:
    ## `postion`

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](project_files/figure-gfm/sum_dls-1.png)<!-- -->

``` r
ggplot(accident_dls, aes(x = year, y = sum_dls)) + 
  geom_point(aes(color = nuts118nm)) + 
  labs(title = "Accidents during DST in Different Regions",
       x = "Year",
       y = "Number of Accidents") +
  geom_smooth(aes(color = nuts118nm)) + 
  labs(color = "Regions")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](project_files/figure-gfm/sum_dls-2.png)<!-- -->

``` r
accident_total <- accident %>% filter(year == 2017) %>%
  st_as_sf(coords = c("mlong", "mlat")) %>%
  st_set_crs(epsg_wgs84) %>% st_transform(epsg_uk) %>%
  filter(lengths(st_intersects(., uk_region)) > 0) %>%
  st_join(uk_region) %>%
  mutate(month = substr(date, 6, 7)) %>%
  group_by(month, nuts118nm) %>%
  mutate(sum_month = n())
  

ggplot(accident_total,aes(x=month, y=sum_month)) + 
  geom_point(postion = "jitter") + 
  labs(title = "2017 Accidents in Different Regions",
       x = "Month",
       y = "Number of Accidents") +
  geom_smooth() + 
  facet_wrap(~ nuts118nm, nrow = 2)
```

    ## Warning in geom_point(postion = "jitter"): Ignoring unknown parameters:
    ## `postion`

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](project_files/figure-gfm/sum_total_2017-1.png)<!-- -->

``` r
ggplot(accident_total, aes(x = month, y = sum_month)) + 
  geom_point(aes(color = nuts118nm)) + 
  labs(title = "2017 Accidents in Different Regions",
       x = "Month",
       y = "Number of Accidents") +
  geom_smooth(aes(color = nuts118nm)) + 
  labs(color = "Regions")
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](project_files/figure-gfm/sum_total_2017-2.png)<!-- -->

Land cover analysis
================
Millie Chapman
3/12/2019

Pull in frequency histogram from google earth engine

``` r
library(splitstackshape)
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
fhist<-read_csv("data/landcover_fire_histogram.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `system:index` = col_character(),
    ##   AGENCY = col_character(),
    ##   ALARM_DATE = col_double(),
    ##   CAUSE = col_integer(),
    ##   COMPLEXNM = col_character(),
    ##   CONT_DATE = col_double(),
    ##   FIRE_NAME = col_character(),
    ##   GIS_ACRES = col_double(),
    ##   INCOMPLEX = col_character(),
    ##   MAPMETHOD = col_character(),
    ##   YEAR = col_integer(),
    ##   YEARn = col_integer(),
    ##   histogram = col_character(),
    ##   .geo = col_character()
    ## )

``` r
landclass <- read_csv("data/landclass.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   histogram_1 = col_integer(),
    ##   Landcover = col_character(),
    ##   Category = col_character()
    ## )

``` r
library(splitstackshape)
#separate at =
fh <- fhist %>%
  cSplit("histogram", sep = ",", direction = "long") %>%
  cSplit("histogram", sep = "=", direction = "wide") %>%
  left_join(landclass) 
```

    ## Joining, by = "histogram_1"

``` r
land_fire<- fh %>%
  group_by(Landcover) %>%
  summarise(area = sum(histogram_2)) %>% na.omit() %>%
  ggplot(aes(x=Landcover, y= area )) +
  geom_bar(stat="identity") + coord_flip()
land_fire
```

![](land-cover_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
catagory_fire<- fh %>%
  group_by(Category) %>%
  summarise(area = sum(histogram_2) *0.078 *2.47105) %>%
  na.omit() %>%
  ggplot(aes(x=Category, y= area )) +
  geom_bar(stat="identity") + coord_flip()
catagory_fire
```

![](land-cover_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
fire_perc<- fh %>%  
  mutate(YEARn = as.numeric(YEAR)) %>%
  filter(YEARn>30,
         GIS_ACRES>75000) %>%
  group_by(FIRE_NAME, Category, GIS_ACRES, YEARn) %>%
  summarise(area = sum(histogram_2)) %>%
  mutate(area = area * .078 * 2.47) %>%
  na.omit() 

cols <- c("Forest" = "darkgreen", "Grassland" = "#799479", "Shrub" = "#DBA901", "Urban/Agriculture" = "gray") 

ggplot(fire_perc, aes(x=reorder(FIRE_NAME, GIS_ACRES), y=area, fill= Category)) + 
  geom_bar(stat="identity") + 
  scale_colour_manual(values = cols, aesthetics = c("colour", "fill")) +
  theme_minimal() +coord_flip() +
  labs(y= "Area Burned in California (Hectares)", x= "Fire Name") +
  theme(legend.title=element_blank(), legend.position = c(0.8, 0.2)) +
  geom_hline(yintercept=100000, linetype="dashed", 
                color = "red", size=1)
```

![](land-cover_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
fire_yrs<- fh %>%  
  mutate(YEARn = as.numeric(YEAR)) %>%
  filter(YEARn>30) %>%
  group_by(FIRE_NAME, Category, GIS_ACRES, YEARn) %>%
  summarise(area = sum(histogram_2)) %>%
  mutate(area = area * .078 * 2.47) %>%
  na.omit() 

ggplot(fire_yrs, aes(x=reorder(YEARn, YEARn), y=area, fill= Category)) + 
  geom_bar(stat="identity") + 
  scale_colour_manual(values = cols, aesthetics = c("colour", "fill")) +
  theme_minimal() +coord_flip() +
  labs(y= "Area Burned in California (Hectares)", x= "Year") +
  theme(legend.title=element_blank(), legend.position = c(0.8, 0.2)) 
```

![](land-cover_files/figure-markdown_github/unnamed-chunk-4-2.png)
A Tidyverse Primer
================
Mubarak Ganiyu
5/31/2022

## A Tidyverse Primer

### Package Installation

``` r
library(tidymodels)
```

    ## -- Attaching packages -------------------------------------- tidymodels 0.2.0 --

    ## v broom        0.8.0     v recipes      0.2.0
    ## v dials        0.1.1     v rsample      0.1.1
    ## v dplyr        1.0.9     v tibble       3.1.6
    ## v ggplot2      3.3.5     v tidyr        1.2.0
    ## v infer        1.0.0     v tune         0.2.0
    ## v modeldata    0.1.1     v workflows    0.2.6
    ## v parsnip      0.2.1     v workflowsets 0.2.1
    ## v purrr        0.3.4     v yardstick    0.0.9

    ## -- Conflicts ----------------------------------------- tidymodels_conflicts() --
    ## x purrr::discard() masks scales::discard()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()
    ## x recipes::step()  masks stats::step()
    ## * Search for functions across packages at https://www.tidymodels.org/find/

### Re-sampling data sets

``` r
boot_samp <- rsample::bootstraps(mtcars, times = 3)
boot_samp
```

    ## # Bootstrap sampling 
    ## # A tibble: 3 x 2
    ##   splits          id        
    ##   <list>          <chr>     
    ## 1 <split [32/12]> Bootstrap1
    ## 2 <split [32/16]> Bootstrap2
    ## 3 <split [32/11]> Bootstrap3

``` r
class(boot_samp)
```

    ## [1] "bootstraps" "rset"       "tbl_df"     "tbl"        "data.frame"

### Tidyverse sample

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v readr   2.1.1     v forcats 0.5.1
    ## v stringr 1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x readr::col_factor() masks scales::col_factor()
    ## x purrr::discard()    masks scales::discard()
    ## x dplyr::filter()     masks stats::filter()
    ## x stringr::fixed()    masks recipes::fixed()
    ## x dplyr::lag()        masks stats::lag()
    ## x readr::spec()       masks yardstick::spec()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
url <- "http://bit.ly/raw-train-data-csv"

all_stations <-
  # Step 1: Read in the data.
  read_csv(url) %>%
  # Step 2: filter columns and rename stationname
  dplyr::select(station = stationname, date, rides) %>%
  # Step 3: Convert the character date field to a date encoding.
  # Also, put the data in units of 1K rides
  mutate(date = mdy(date), rides = rides / 1000) %>%
  # Step 4: Summarize the multiple records using the maximum.
  group_by(date, station) %>%
  summarize(rides = max(rides), .groups = "drop")
```

    ## Rows: 1101828 Columns: 5

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (3): stationname, date, daytype
    ## dbl (1): station_id
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

---
title: "Harmful Effects of Severe Weather"
author: "Andrew Luyt"
output: 
    html_document:
        theme: united
        highlight: pygments
        keep_md: true
---


*Most recent update: 2021-12-27*

## Synopsis
*Immediately after the title, there should be a synopsis which describes and 
summarizes your analysis in at most 10 complete sentences.*

## Data Processing
*There should be a section titled Data Processing which describes (in words and 
code) how the data were loaded into R and processed for analysis. In particular, 
your analysis must start from the raw CSV file containing the data. You cannot 
do any preprocessing outside the document. If preprocessing is time-consuming 
you may consider using the `cache = TRUE` option for certain code chunks.*


```r
# data.table is required for one function only, "fread". We won't load the
# entire package because its namespace overlaps some other functions we use.
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
```


```r
df <- as_tibble(data.table::fread(file = "repdata_data_StormData.csv.bz2"))
df <- rename(df, state_num = STATE__, longitude_e = LONGITUDE_)
names(df) <- tolower(names(df))
# these variables are all 0 and all NA, respectively
df <- select(df, -c(county_end, countyendn))

# standardize `evtype` to lowercase, removing spaces at beginning and end
# and removing extra internal spaces
df$evtype <- tolower(str_squish(df$evtype))
```

### Correcting `propdmgexp` and `cropdmgexp`

Damage amounts are represented in an unusual way. They are expressed in
scientific notation, with the base and exponent stored in separate variables.

The exponents, `propdmgexp` and `cropdmgexp`,  are supposed to
represent a power of 10, which is multiplied by `propdmg` or `cropdmg`
respectively to express a final damage amount. However, they actually
contain unusual values:


```r
sort(union(unique(df$propdmgexp), unique(df$cropdmgexp)))
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "k" "K" "m"
## [20] "M"
```

After some investigation, it appears varying data entry standards or data
entry error has resulted in this state of affairs. A power of 10 will be
assigned to replace each of these symbols.
[*For a more complete discussion, see [this Rpubs document](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html).*]

* `empty string`: 0
* `+` or `?`: 1
* `0-9`: 10
* `hH`: 100   "hundred"
* `kK`: 1000  "kilo"
* `mM`: 10^6  "million"
* `bB`: 10^9  "billion"
* `-`: NA 


```r
# Perform the substitution described above
old <- c("-", "?", "+", "0", "1", "2", "3", "4", "5", "6", "7", "8", "B", "H", "K", "M", "")
new <- c(NA,   1,   1,   10,  10,  10,  10,  10,  10,  10,  10,  10, 1e9, 1e2, 1e3, 1e6, 0)

ctmp <- toupper(df$cropdmgexp)
ptmp <- toupper(df$propdmgexp)
cnew <- rep(NA_integer_, length(ctmp)) # new vectors will be integers
pnew <- rep(NA_integer_, length(ptmp))

for (i in 1:length(old)) {
    cnew[ctmp == old[[i]]] <- new[[i]]
    pnew[ptmp == old[[i]]] <- new[[i]]
}

df$cropdmgexp <- cnew
df$propdmgexp <- pnew
rm(ctmp, ptmp, cnew, pnew, old, new, i)
```

### Fixing `evtype`

`evtype` may be the most important variable in our dataset. Later we'll be
aggregating human and financial damage via the values in `evtype` and it's
important that they reflect what we think they do.

In the NOAA documentation there are 48 official `evtype` values, corresponding
to the various weather events they wish to record data for.

In the dataset however, almost 1000 unique values exist. An examination
shows this to have probably resulted from manual data entry. For example:


```r
df %>% 
    count(evtype) %>% 
    arrange(desc(n)) %>% 
    filter(str_detect(evtype, "blizzard")) %>% 
    head(n=7)
```

```
## # A tibble: 7 × 2
##   evtype                             n
##   <chr>                          <int>
## 1 blizzard                        2719
## 2 high wind/blizzard                 6
## 3 heavy snow/blizzard                3
## 4 blizzard and extreme wind chil     2
## 5 blizzard/heavy snow                2
## 6 ground blizzard                    2
## 7 blizzard and heavy snow            1
```

We can see one value labeling most of the observations, with a number of
similar-sounding events labeling other observations that should probably be
a part of `blizzard`. We'd like to know if this is going to be a problem. 
**For what proportion of records does `evtype` have a non-standard value?**


```r
types <- tolower(readLines("evtypes.txt")) # a vector of standard values
df %>% filter(! evtype %in% types) %>% nrow() / nrow(df)
```

```
## [1] 0.2958472
```
About 30% of the data is using some other label for `evtype`. This is
a significant number and could certainly skew our analysis of the major causes
of human or financial damage.  e.g. if one large event type
were split into many smaller types it would no longer appear to be a major
cause.

Let's count how many non-standard `evtype` strings have 100 or more observations
in the dataset. As a reminder, there are 902297 observations in total.


```r
fixtypes <- df %>% 
    filter(! evtype %in% types) %>%
    count(evtype) %>% 
    arrange(desc(n)) %>% 
    filter(n >= 100) %>% 
    pull(evtype)
length(fixtypes)
```

```
## [1] 32
```

This is good news, we won't have to fix over 900 possible errors. Fixing 
the 32 most common should ensure a much more accurate analysis.

What we will do is work through these 32 types systematically and assign them
the correct `evtype`s, using the NOAA documentation and some common sense. 
The rest of the non-standard labels we will disregard for having too
small an effect on the analysis to matter. Let's first look at the labels
we'll fix.


```r
fixtypes
```

```
##  [1] "tstm wind"              "thunderstorm winds"     "marine tstm wind"      
##  [4] "urban/sml stream fld"   "high winds"             "wild/forest fire"      
##  [7] "winter weather/mix"     "tstm wind/hail"         "flash flooding"        
## [10] "extreme cold"           "flood/flash flood"      "snow"                  
## [13] "landslide"              "fog"                    "wind"                  
## [16] "rip currents"           "storm surge"            "freezing rain"         
## [19] "urban flood"            "heavy surf/high surf"   "extreme windchill"     
## [22] "strong winds"           "dry microburst"         "coastal flooding"      
## [25] "light snow"             "hurricane"              "river flood"           
## [28] "record warmth"          "unseasonably warm"      "flooding"              
## [31] "astronomical high tide" "moderate snowfall"
```

Now we'll fix these incorrect labels by reassigning the observation to the
correct `evtype`.


```r
# map official evtypes to a vector of some incorrect names for that type
fixlist <- list(
    "thunderstorm wind" = c("tstm wind", "thunderstorm winds", "tstm wind/hail", 
                            "wind", "dry microburst"),
    "marine thunderstorm wind" = c("marine tstm wind"),
    "flood" = c("urban/sml stream fld", "flood/flash flood", "urban flood",
                "river flood", "flooding"),
    "flash flood" = c("flash flooding"),
    "coastal flood" = c("coastal flooding"),
    "high wind" = c("high winds"),
    "strong wind" = c("strong winds"),
    "wildfire" = c("wild/forest fire"),
    "winter weather" = c("winter weather/mix"),
    "extreme cold/wind chill" = c("extreme cold", "extreme windchill"),
    "heavy snow" = c("snow", "light snow", "moderate snowfall"),
    "debris flow" = c("landslide"),
    "dense fog" = c("fog"),
    "rip current" = c("rip currents"),
    "excessive heat" = c("unseasonably warm", "record warmth"),
    # from docs: "..caused by any combination of...high astronomical tide..."
    "storm surge/tide" = c("storm surge", "astronomical high tide"),  
    "high surf" = c("heavy surf/high surf"),
    "winter weather" = c("freezing rain"),
    "hurricane (typhoon)" = c("hurricane")
)

tmp <- df$evtype

# replace all the bad evtypes with official evtypes
for (officialtype in names(fixlist)) {
    for (badtype in fixlist[[officialtype]]) {
        tmp[grep(pattern = badtype, x = tmp)] <- officialtype
    }
}
df$evtype <- tmp
rm(tmp)
```

Previously, almost 30% of records had an incorrect `evtype`. Let's recalculate 
that proportion.

```r
df %>% filter(! evtype %in% types) %>% nrow() / nrow(df)
```

```
## [1] 0.003188529
```

Excellent! Now only 0.3% of records have some unusual `evtype` from data
entry errors. We'll continue the analysis with this imperfect but
much-improved dataset, 
assuming that this tiny proportion of error won't significantly skew our results.

## Results
*There should be a section titled Results in which your results are presented.*

### Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?


Here we sum the fatalities and injuries across `evtype`s, select the ten largest,
and then plot the results.


```r
top_fatalities <- df %>% 
    group_by(evtype) %>% 
    summarise(fatalities = sum(fatalities)) %>% 
    arrange(desc(fatalities)) %>% 
    slice_head(n = 10)

top_injuries <- df %>% 
    group_by(evtype) %>% 
    summarise(injuries = sum(injuries)) %>% 
    arrange(desc(injuries)) %>% 
    slice_head(n = 10)
```


```r
par(mfrow = c(1,2), mar=c(c(9, 4, 4, 2) + 0.1))

barplot(top_fatalities$fatalities, names.arg = top_fatalities$evtype, las=2,
        main="Fatalities: top causes")
barplot(top_injuries$injuries, names.arg = top_injuries$evtype, las=2,
        main="Injuries: top causes")
```

![](harmful_weather_files/figure-html/graph_injuries_fatalities-1.png)<!-- -->

```r
par(mfrow = c(1,1), mar=c(5, 4, 4, 2) + 0.1)
```

In terms of both fatalities and injuries, **tornadoes are the most destructive
events to human health** in this dataset. 

#### Hurricanes?

There appears to be something missing in the above graphs. Knowing something of 
destructive US weather events one must ask **"Where are the hurricanes?"**

There is an important note in the FAQ:

>The fatalities, injuries, and damage amounts appearing in tropical cyclone events are
attributed only to wind damage experienced in the coastal counties/parishes listed. Other
tropical cyclone related events such as tornadoes and flooding are listed within their
separate event types.

**i.e. the deaths and damage from hurricanes have deliberately been split up into 
multiple evtypes** and will show up as part of floods, lightning, wind, etc. 
Hurricane-spawned tornadoes are also included in the tornado events graphed above.
From the point of view of this dataset, hurricanes are a sort of 'meta-event'
that **causes** other events.

It is also worth noting that some similar events have different official
`evtype`s, e.g. **excessive heat** and **heat**, both of which show up in
the graphs above.

### Across the United States, which types of events have the greatest economic consequences?


```r
df %>% 
    group_by(evtype) %>% 
    summarise(property = sum(propdmg * propdmgexp),
              crops = sum(cropdmg * cropdmgexp)) %>% 
    arrange(desc(property))
```

```
## # A tibble: 489 × 3
##    evtype                  property       crops
##    <chr>                      <dbl>       <dbl>
##  1 flood               150751456324 10853820100
##  2 hurricane (typhoon)  84656180010  5505292800
##  3 tornado              56937162897   414954710
##  4 storm surge/tide     47974149000      855000
##  5 flash flood          16140865011  1421317100
##  6 hail                 15732269877  3025954650
##  7 wildfire              7766963500   402281630
##  8 tropical storm        7703890550   678346000
##  9 winter storm          6688497260    26944000
## 10 ice storm             3944928310  5022113500
## # … with 479 more rows
```


To aid reproducibility, here is the session environment at time of publishing.

```r
sessionInfo()
```

```
## R version 4.1.2 (2021-11-01)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.3 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/atlas/libblas.so.3.10.3
## LAPACK: /usr/lib/x86_64-linux-gnu/atlas/liblapack.so.3.10.3
## 
## locale:
##  [1] LC_CTYPE=en_CA.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_CA.UTF-8        LC_COLLATE=en_CA.UTF-8    
##  [5] LC_MONETARY=en_CA.UTF-8    LC_MESSAGES=en_CA.UTF-8   
##  [7] LC_PAPER=en_CA.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_CA.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] lubridate_1.8.0 forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7    
##  [5] purrr_0.3.4     readr_2.1.1     tidyr_1.1.4     tibble_3.1.6   
##  [9] ggplot2_3.3.5   tidyverse_1.3.1
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.1 xfun_0.29        haven_2.4.3      colorspace_2.0-2
##  [5] vctrs_0.3.8      generics_0.1.1   htmltools_0.5.2  yaml_2.2.1      
##  [9] utf8_1.2.2       rlang_0.4.12     jquerylib_0.1.4  pillar_1.6.4    
## [13] glue_1.6.0       withr_2.4.3      DBI_1.1.2        dbplyr_2.1.1    
## [17] modelr_0.1.8     readxl_1.3.1     lifecycle_1.0.1  munsell_0.5.0   
## [21] gtable_0.3.0     cellranger_1.1.0 rvest_1.0.2      evaluate_0.14   
## [25] knitr_1.37       tzdb_0.2.0       fastmap_1.1.0    fansi_0.5.0     
## [29] highr_0.9        broom_0.7.10     Rcpp_1.0.7       backports_1.4.1 
## [33] scales_1.1.1     jsonlite_1.7.2   fs_1.5.2         hms_1.1.1       
## [37] digest_0.6.29    stringi_1.7.6    grid_4.1.2       cli_3.1.0       
## [41] tools_4.1.2      magrittr_2.0.1   crayon_1.4.2     pkgconfig_2.0.3 
## [45] ellipsis_0.3.2   xml2_1.3.3       reprex_2.0.1     rstudioapi_0.13 
## [49] assertthat_0.2.1 rmarkdown_2.11   httr_1.4.2       R6_2.5.1        
## [53] compiler_4.1.2
```


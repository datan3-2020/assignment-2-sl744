Data analysis assignment 2
================
Simone Long\_135288
11/02/2020

## Read data

``` r
library(tidyverse)

setwd("C:/Users/simon/OneDrive/Documents/datan3_2019")
Egoalt8 <- read_tsv("data/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Hh8 <- read_tsv("data/UKDA-6614-tab/tab/ukhls_w8/h_hhresp.tab")
```

## Filter household roster data (10 points)

``` r
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv == 1 | h_relationship_dv == 2) %>%
        select(pidp, apidp, h_hidp, h_relationship_dv, h_sex, h_asex)
```

``` r
Hetero8 <- Partners8 %>%
        filter(h_sex == 2 & h_asex == 1) %>%
        filter(pidp[h_sex == 2], apidp[h_asex == 1]
        )
```

## Recode data on ethnicity (10 points)

``` r
Stable2 <- Stable %>%
        select(pidp, racel_dv)
```

``` r
Stable2 <- Stable2 %>%
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))
```

``` r
Stable2 <- Stable2 %>%
        mutate(race = recode(racel_dv,
                `1` = "White",
                `2` = "White",
                `3` = "White",
                `4` = "White",
                .default = "non-White")
        )
```

## Join data (30 points)

``` r
JoinedEthn <- Hetero8 %>%
        select(pidp, apidp, h_hidp) %>%
        left_join(Stable2, by = "pidp")
```

``` r
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv) %>%
        rename(egoRace = race)
```

``` r
JoinedEthn <- JoinedEthn %>%
        left_join(Stable2, by = c("apidp" = "pidp"))
```

``` r
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv) %>%
        rename(alterRace = race)
```

## Explore probabilities of racial endogamy (20 points)

``` r
TableRace <- JoinedEthn %>%
        filter(egoRacel_dv > 0, alterRacel_dv > 0) %>%
        count(egoRace, alterRace)
TableRace
```

    ## # A tibble: 4 x 3
    ##   egoRace   alterRace     n
    ##   <chr>     <chr>     <int>
    ## 1 non-White non-White  1790
    ## 2 non-White White       326
    ## 3 White     non-White   266
    ## 4 White     White      9694

``` r
TableRace %>%

        group_by(egoRace) %>%
        mutate(egoRacetot = 
                sum(n)) %>%
        mutate(prob = n/egoRacetot)
```

    ## # A tibble: 4 x 5
    ## # Groups:   egoRace [2]
    ##   egoRace   alterRace     n egoRacetot   prob
    ##   <chr>     <chr>     <int>      <int>  <dbl>
    ## 1 non-White non-White  1790       2116 0.846 
    ## 2 non-White White       326       2116 0.154 
    ## 3 White     non-White   266       9960 0.0267
    ## 4 White     White      9694       9960 0.973

## Join with household data and calculate mean and median number of children by ethnic group (30 points)

``` r
JoinedEthn2 <- Hh8 %>%
        select(h_hidp, h_nkids_dv) %>%
        full_join(JoinedEthn, by = "h_hidp")

JoinedEthn2 <- JoinedEthn2 %>%
        mutate(ethnendo = case_when(
                egoRacel_dv == 1 & alterRacel_dv == 1 ~ "White",
                egoRacel_dv == 9 & alterRacel_dv == 9 ~ "Indian",
                egoRacel_dv == 10 & alterRacel_dv == 10 ~ "Pakistani"
                
        )) %>%
        filter(ethnendo == "White" | ethnendo == "Indian" | ethnendo == "Pakistani")

tabkidrac <- JoinedEthn2 %>%
        select(h_hidp, h_nkids_dv, ethnendo) %>%
        group_by(ethnendo) %>%
        summarise(
                meankids = mean(h_nkids_dv, na.rm = TRUE),
                medkids = median(h_nkids_dv, na.rm = TRUE))

knitr::kable(
        tabkidrac,
        col.names = c("Family Ethnicity", "Average No. Kids", "Median No. Kids")
)
```

| Family Ethnicity | Average No. Kids | Median No. Kids |
| :--------------- | ---------------: | --------------: |
| Indian           |        0.9553753 |               1 |
| Pakistani        |        1.8108747 |               2 |
| White            |        0.5651314 |               0 |

According to the table produced above, it would seem that Pakistani
families of a heteronormative structure (i.e. mother and father) tend to
have more children, with the average number of kids per household being
twice that of Indian households, and over three times larger than the
White British household average. But this result should be approached
with caution, as within an sample, there inevitably exists some error,
primarily due to the sampling technique used here. Simply looking at the
total number of White British vs. Indian/Pakistani housholds (with
available data pertaining to children per household), an overwhelming
majority of the sample are White. This obviously creates problems in
terms of measurement accuracy, as there is a clear gap in representation
for racial minorities. This is likely due to non-response, given the
accessibility of this survey, the amount of time it takes to complete
(in addition to the annual commitment), and the personal questions it
asks. So it is important to keep in mind the likelihood of innacuracy,
especially when looking at data for Pakistani and Indian households.

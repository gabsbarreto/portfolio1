NHANES
================

## NHANES (complex survey) data analysis

This study aimed to evaluate the association between habitual caffeine
consumption and markers of insulin sensitivity (HOMA-IR, HbA1C, and
plasma glucose during a two-hour oral glucose tolerance test OGTT2H) in
the USA population. The data was obtained from NHANES, a biannual survey
designed to assess the health and nutritional status of adults and
children in the United States. For more information on NHANES, [click
here](https://www.cdc.gov/nchs/nhanes/index.htm).

## Step-by-step index

1.  [Obtaining data with the NHANES
    API.](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#obtaining-data-from-nhanes-api)
2.  [Obtaining dietary data separate by assessment
    day](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#dietary-data)
3.  [Calculating caffeine consumption separate by food
    source](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#caffeine-by-food-source)
4.  [Calculating physical activity
    levels](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#physical-activity)
5.  [Using chatGPT to categorise participans’ prescribed
    medication](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#using-gpt-for-medication-data)
6.  [Merging all
    DFs](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#merging-all-dataframes)
7.  [Final adjusments on
    data](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#final-data-wrangling-procedures)
8.  [Creating the survey
    design](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#survey-design)
9.  [Exploratory
    analysis](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#exploratory-analysis)
10. [Fixing
    skewness](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#fixing-skewness)
11. [Fitting the Gamma
    Model](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#fitting-the-gamma-model)
12. [Stepwise
    Backwards](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#stepwise-procedure)
13. [Estimate marginal means and
    plot](https://github.com/gabsbarreto/portfolio1/blob/main/NHANES%20project/NHANES-Git.md#estimated-means-and-plot)

### Packages used

``` r
library(nhanesA)
library(tidyverse)
library(survey)
library(emmeans)
library(openai)
library(car)
```

### Obtaining data from NHANES API

I used the NHANES API to obtain the datasets that interest me, one by
one, and merged them into a single df. Below there is an example using
the NHANES demographic data.

``` r
DEMOG0506 <- nhanes('DEMO_D') ##DEMOGRAPHICS 05-06
DEMOG0506.2 <- DEMOG0506 %>% ##SELECTING VARIABLES OF INTEREST
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG,  RIDRETH1, DMDEDUC3, DMDEDUC2, INDFMINC, SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) 
rm(DEMOG0506)

DEMOG0708 <- nhanes('DEMO_E') ##DEMOGRAPHICS 07-08
DEMOG0708.2 <- DEMOG0708 %>% ##SELECTING VARIABLES OF INTEREST
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG,  RIDRETH1, DMDEDUC3, DMDEDUC2, INDFMIN2, #VARIABLE NAME DIFFERENT FROM LAST CYCLE 
         SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) %>%
  rename(INDFMINC = INDFMIN2) ##RENAME VARIABLE TO MATCH LAST CYCLE
rm(DEMOG0708)

DEMOG0910 <- nhanes('DEMO_F') ##DEMOGRAPHICS 09-10
DEMOG0910.2 <- DEMOG0910 %>% ##SELECTING VARIABLES OF INTEREST
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG,  RIDRETH1, DMDEDUC3, DMDEDUC2, INDFMIN2, #VARIABLE NAME DIFFERENT FROM LAST CYCLE 
         SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) %>%
  rename(INDFMINC = INDFMIN2) ##RENAME VARIABLE TO MATCH LAST CYCLE
rm(DEMOG0910)

DEMOG1112 <- nhanes('DEMO_G') ##DEMOGRAPHICS 11-12
DEMOG1112.2 <- DEMOG1112 %>% ##SELECTING VARIABLES OF INTEREST
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG,  RIDRETH1, DMDEDUC3, DMDEDUC2, INDFMIN2, #VARIABLE NAME DIFFERENT FROM LAST CYCLE 
         SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) %>%
  rename(INDFMINC = INDFMIN2) ##RENAME VARIABLE TO MATCH LAST CYCLE
rm(DEMOG1112)


DEMOG1314 <- nhanes('DEMO_H') ##DEMOGRAPHICS 13-14
DEMOG1314.2 <- DEMOG1314 %>% ##SELECTING VARIABLES OF INTEREST
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG,  RIDRETH1, DMDEDUC3, DMDEDUC2, INDFMIN2, #VARIABLE NAME DIFFERENT FROM LAST CYCLE 
         SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) %>%
  rename(INDFMINC = INDFMIN2) ##RENAME VARIABLE TO MATCH LAST CYCLE
rm(DEMOG1314)

DEMOG1516 <- nhanes('DEMO_I') ##DEMOGRAPHICS 15-16
DEMOG1516.2 <- DEMOG1516 %>% ##SELECTING VARIABLES OF INTEREST
  select(SEQN, RIAGENDR, RIDAGEYR, RIDEXPRG,  RIDRETH1, DMDEDUC3, DMDEDUC2, INDFMIN2, #VARIABLE NAME DIFFERENT FROM LAST CYCLE 
         SDMVPSU, SDMVSTRA, WTINT2YR, WTMEC2YR) %>%
  rename(INDFMINC = INDFMIN2) ##RENAME VARIABLE TO MATCH LAST CYCLE
rm(DEMOG1516)

DEMOG0416 <- bind_rows(DEMOG0506.2,
                       DEMOG0708.2,
                       DEMOG0910.2,
                       DEMOG1112.2,
                       DEMOG1314.2,
                       DEMOG1516.2)
rm(DEMOG0506.2,
   DEMOG0708.2,
   DEMOG0910.2,
   DEMOG1112.2,
   DEMOG1314.2,
   DEMOG1516.2)
```

#### General data

Data on body composition (BMX_D - I), the results of the 2-h oral
glucose tolerance test (OGTT_D - I), fasting glucose and insulin
(GLU_D - I or INS_H - I \[they were separated after the 13-14 cycle\]),
glycohemoglobine (GHB_D - I), and blood pressure (BPX_D - I) were also
obtained.

#### Dietary data

For dietary data, NHANES has two days of data collection, one during the
examination phase, and another one via phone call. I pulled the total
results for macronutrients and caffeine separate for both days
(DR1TOT_D - I and DR2TOT_D - I), binding them together.

Example:

``` r
DIET0506.1 <- nhanes('DR1TOT_D')
DIET0506.1.2 <- DIET0506.1 %>%
  select(SEQN, WTDRD1, WTDR2D,  DR1DAY, DR1TKCAL, DR1TPROT,  DR1TCARB,DR1TTFAT,DR1TMFAT,DR1TPFAT, DR1TSFAT, DR1TCAFF,DR1TALCO, DR1TSUGR, DR1TFIBE )

DIET0506.2 <- nhanes('DR2TOT_D')
DIET0506.2.2 <- DIET0506.2 %>%
  select(SEQN, DR2DAY, DR2TKCAL, DR2TPROT,  DR2TCARB,DR2TTFAT,DR2TMFAT,DR2TPFAT, DR2TSFAT, DR2TCAFF,DR2TALCO, DR2TSUGR, DR2TFIBE )

DIET0506FULL <- merge(DIET0506.1.2,DIET0506.2.2, by = 'SEQN', all = T)

[...]

DIETALLFULL <- bind_rows(DIET0506FULL,
                         DIET0708FULL,
                         DIET0910FULL,
                         DIET1112FULL,
                         DIET1314FULL,
                         DIET1516FULL)
```

Visualising:

``` r
head(DIETALLFULL)
```

    ##    SEQN    WTDRD1    WTDR2D   DR1DAY DR1TKCAL DR1TPROT DR1TCARB DR1TTFAT
    ## 1 31127  4718.873  5906.472 Saturday     1421    50.42   171.45    61.28
    ## 2 31128  3933.789  4250.890   Friday     1402    32.51   180.88    65.05
    ## 3 31129 26719.806 26719.806   Monday     4110   139.89   684.22    94.73
    ## 4 31130        NA        NA     <NA>       NA       NA       NA       NA
    ## 5 31131 13595.896 14822.479   Friday     1458    27.43   233.73    50.86
    ## 6 31132 40484.136 45660.639  Tuesday     2168    75.81   269.93    60.16
    ##   DR1TMFAT DR1TPFAT DR1TSFAT DR1TCAFF DR1TALCO DR1TSUGR DR1TFIBE    DR2DAY
    ## 1   20.509    8.098   25.994        0      0.0   139.02      4.6    Sunday
    ## 2   17.128   17.755   23.381       33      0.0    88.69     10.0    Monday
    ## 3   37.018   12.180   38.599        7      0.0   498.82     14.5    Sunday
    ## 4       NA       NA       NA       NA       NA       NA       NA      <NA>
    ## 5   20.745    7.641   18.274       13      0.0   146.01      6.4 Wednesday
    ## 6   17.494   19.114   17.608      260     40.8   111.54     20.3    Sunday
    ##   DR2TKCAL DR2TPROT DR2TCARB DR2TTFAT DR2TMFAT DR2TPFAT DR2TSFAT DR2TCAFF
    ## 1     1302    46.11   165.96    51.85   17.254    5.816   23.509        0
    ## 2     1797    94.60   158.68    87.39   31.070   17.140   31.164        0
    ## 3     3092    79.47   420.63   125.23   49.236   31.040   38.181        0
    ## 4       NA       NA       NA       NA       NA       NA       NA       NA
    ## 5     2044    58.33   239.08   100.15   32.824   17.284   42.619       19
    ## 6     1446    54.53   183.59    42.75   13.705    5.343   20.341      302
    ##   DR2TALCO DR2TSUGR DR2TFIBE
    ## 1      0.0   103.67      5.6
    ## 2      0.0    87.13      5.5
    ## 3      0.0   195.93     19.4
    ## 4       NA       NA       NA
    ## 5      0.0   118.39      9.9
    ## 6     21.2   109.35     18.6

#### Caffeine by food source

Besides the total calculated nutrients, NHANES also makes available the
full dietary records of their participants, separating them by food
product (DR1IFF_D and DR2IFF_D - I). The codes and names for the food
products are also listed separately (DRXFCD_D - I).

I aimed to separate caffeine from coffee and tea from other food
sources. This is how I managed:

``` r
#### DAY 1

oodcode0506 <- nhanes('DRXFCD_D') ## obtaining DF with food codes
FOOD0506.1 <- nhanes('DR1IFF_D') ## obtaining DF with food products on day 1

FOOD0506.1.2 <- FOOD0506.1 %>%
  rename(DRXFDCD = DR1IFDCD ) %>% ### changing the name of the food code variable
  merge(.,foodcode0506, .by = 'DRXFDCD', all = T) %>% ##merging dfs
  filter(DR1ICAFF > 0 ) %>% ### kept only caffeine food sources
  mutate(coffeeandtea = ifelse(grepl('COFFEE', DRXFCSD) | grepl('TEA', DRXFCSD) & !grepl('STEAK', DRXFCSD), 1,0))   ##signaling the variables that have coffee  OR tea in their product name; it would include products with 'STEAK', so I removed those 


SUMMCOFFEE0506.1 <- FOOD0506.1.2 %>%   
  group_by(SEQN, coffeeandtea) %>%
  summarise_at(vars(DR1ICAFF), funs(sum(.,na.rm= T))) %>%     ##obtaining the total amount of caffeine from each source (coffee and tea/others)
  pivot_wider(values_from = DR1ICAFF, names_from = coffeeandtea, names_sep = '_')

#### DAY 2
FOOD0506.2 <- nhanes('DR2IFF_D') ## obtaining DF with food products on day  2

FOOD0506.2.2 <- FOOD0506.2 %>%
  rename(DRXFDCD = DR2IFDCD ) %>% ### changing the name of the food code variable
  merge(.,foodcode0506, .by = 'DRXFDCD', all = T) %>% ##merging dfs
  filter(DR2ICAFF > 0 ) %>% ### kept only caffeine food sources
  mutate(coffeeandtea = ifelse(grepl('COFFEE', DRXFCSD) | grepl('TEA', DRXFCSD) & !grepl('STEAK', DRXFCSD),  1,0))   ##signaling the variables that have coffee  OR tea in their product name; it would include products with 'STEAK', so I removed those 


SUMMCOFFEE0506.2 <- FOOD0506.2.2 %>%   
  group_by(SEQN, coffeeandtea) %>%
  summarise_at(vars(DR2ICAFF), funs(sum(.,na.rm= T))) %>%     ##obtaining the total amount of caffeine from each source (coffee and tea/others)
  pivot_wider(values_from = DR1ICAFF, names_from = coffeeandtea, names_sep = '_')


#### JOINING BOTH DAYS
summ0506join <- full_join(SUMMCOFFEE0506.1,SUMMCOFFEE0506.2, by = 'SEQN') %>%
  rename(CAFFother1 = '0.x', CAFFother2 = "0.y", CAFFcoffeetea1 = '1.x', CAFFcoffeetea2 = "1.y") ###when joined, it is ideal to differentiate which one is from day 1 and day 2
```

Visualising

``` r
head(summ0506join)
```

    ## # A tibble: 6 × 5
    ## # Groups:   SEQN [6]
    ##    SEQN CAFFother1 CAFFcoffeetea1 CAFFother2 CAFFcoffeetea2
    ##   <dbl>      <dbl>          <dbl>      <dbl>          <dbl>
    ## 1 31128         33             NA         NA             NA
    ## 2 31129          7             NA         NA             NA
    ## 3 31131          5              8          8             11
    ## 4 31132         NA            260         NA            302
    ## 5 31133         71             NA         32             NA
    ## 6 31134         NA            142        103              2

#### Physical activity

Regarding participants’ physical activity, the extracted data was
available separated by type of physical activity (e.g., commuting,
moderate or vigorous work-related or non work-related). For commuting
and moderate physical activities, a MET score of 4 was given, and of 8
for vigorous. This is how I handled it:

``` r
PAQ0708 <- nhanes('PAQ_E')
PAQ07082 <- PAQ0708 %>%
  select(SEQN, PAQ605, PAQ610, PAD615, PAQ620,PAQ625,PAD630,PAQ635,PAQ640,PAD645,PAQ650,
         PAQ655,PAD660,PAQ665,PAQ670,PAD675) %>% ##selecting variables of interest
  mutate(vigorouswork = case_when(PAQ610 <=7 & PAD615 <= 960 ~ PAQ610 * PAD615, 
                                     PAQ610 >7 | PAD615 > 960 ~0,
                                     is.na(PAQ610) | is.na(PAD615) ~ 0 )) %>%     ### multiplying days of activity (PAQ) by the average time of activity(PAD) for each of the activities
  mutate(moderatework = case_when(PAQ625 <=7 & PAD630 <= 1440 ~ PAQ625 * PAD630, 
                                  PAQ625 >7 | PAD630 > 1440 ~0,
                                  is.na(PAQ625) | is.na(PAD630) ~ 0 )) %>%
  mutate(bicyclewalk = case_when(PAQ640 <=7 & PAD645 <= 600 ~ PAQ640 * PAD645, 
                                  PAQ640 >7 | PAD645 > 600 ~0,
                                  is.na(PAQ640) | is.na(PAD645) ~ 0 )) %>%
  mutate(vigrec = case_when(PAQ655 <=7 & PAD660 <= 990 ~ PAQ655 * PAD660, 
                                 PAQ655 >7 | PAD660 > 990 ~0,
                                 is.na(PAQ655) | is.na(PAD660) ~ 0 )) %>%
  mutate(modrec = case_when(PAQ670 <=7 & PAD675 <= 720 ~ PAQ670 * PAD675, 
                            PAQ670 >7 | PAD675 > 720 ~0,
                            is.na(PAQ670) | is.na(PAD675) ~ 0 )) %>%
  mutate(totalMET = ( (vigorouswork + vigrec) * 8) + ((moderatework + bicyclewalk + modrec) * 4)    )  #### multiplying the total time (average-time * days) by the MET provided by the NHANES organisers
```

#### Using GPT for medication data

Each participant has provided a list of prescribed medication consumed
in the 30 days preceding the interview. These datasets are available as
‘RXQ_RX_E - I’.

My idea was to categorise those participants between those who were (or
were not) taking oral hypoglycemic drugs and blood pressure medication,
and include those in the statistical models.

I used the openAI API for that, obtaining “Yes” or “No” answers to the
classes of drugs I was interested in:

``` r
### First I established the system and specific prompts
systemprompt <- 'You have all knowledge about pharmacology, medications, their purpose and prescription'
diabetesmed <- "Is this medication classified as an oral antihyperglycemic drug? The only acceptable answers are: 'Yes' or 'No'"
pressuremed <- "Is this medication prescribed for the control of high blood pressure? The only acceptable answers are: 'Yes' or 'No'"

####Then I created a function to send the requests to the API
get_gpt_response <- function(prompt) {
  if (is.null(prompt) || is.na(prompt)) {
    return(NA)
  }
  response <- create_chat_completion(  ### THIS FUNCTION GENERATES A GPT RESPONSE
    model = "gpt-4o",   ### 4o WAS THE MODEL I CHOSE THIS TIME
    messages = list(
      list("role" = "system", "content" = systemprompt),
      list("role" = "user", "content" = prompt)
    ),
    n = 1,   ### ONLY ONE RESPONSE REQUESTED
    max_tokens = 300,   ### LIMITING THE MAX TOKENS GENERATED
    stop = NULL,
    openai_api_key = "----", #### Making sure not to show everyone my OPENAI key.
    temperature = 0.1    #### TEMPERATURE REFERS TO THE OBJECTIVENESS, and when Temperature ~ 0, objectiveness increases.
  )
  # Extract the message content from the response
  content <- response[["choices"]][["message.content"]]
  return(content)
}


#### THEN I CREATED THE FUNCTION TO SEND THE API THE DATA I WANTED A RESPONSE TO.
process_dataframe <- function(df) { ### used my PRESCRIBED DRUG df

  
  df <- df %>%   ### creating the columns 
  mutate(GPT_glycemic = as.numeric(NA),
         GPT_bloodpressure = as.numeric(NA))

  for (i in seq_along(df$RXDDRUG)) {    #### sending the request
    message <-  paste("Medication: ", df$RXDDRUG[i],". ",diabetesmed, sep = '')
    response <- get_gpt_response(message)
    df$GPT_glycemic[i] <- response ### storing the response
  }
  
  for (i in seq_along(df$RXDDRUG)) {     #### sending the request
    message <-  paste("Medication: ", df$RXDDRUG[i],". ",pressuremed, sep = '')
    response <- get_gpt_response(message)
    df$GPT_bloodpressure[i] <- response   ### storing the response
  }
  
  return(df)
}


GPTDFprescriptions <- process_dataframe(DFprescriptions)
```

My DFprescriptions table was a list of unique(medication) used by the
NHANES participants with the response columns:

``` r
head(GPTDFprescriptions)
```

    ##               RXDDRUG GPT_glycemic GPT_bloodpressure
    ## 1            ATENOLOL           No              Yes.
    ## 2 HYDROCHLOROTHIAZIDE           No               Yes
    ## 3       LEVOTHYROXINE           No                No
    ## 4          LOVASTATIN           No                No
    ## 5   MUPIROCIN TOPICAL           No                No
    ## 6           GLIPIZIDE          Yes                No

The obtained responses (GPTDFprescriptions) were merged with the
dataframe containing the full prescriptions, which was then summarised,
keeping only a single row for each participant.

``` r
medsfull2 <- PMedication %>%
  merge(GPTDFprescriptions,., by = 'RXDDRUG', all = T) 

medsfull3 <- medsfull2 %>%
  mutate(GPT_glycemic = ifelse(is.na(GPT_glycemic), 'No',GPT_glycemic)) %>%
  mutate(GPT_bloodpressure = ifelse(is.na(GPT_bloodpressure), 'No',GPT_bloodpressure)) %>%
  filter(RXDUSE == "Yes") %>%
  mutate(GPT_insulin = ifelse(grepl("INSULIN", RXDDRUG), "Yes", "No") )   ### I ALSO ADDED A COLUMN FOR INSULIN USERS

medsfull4 <- medsfull3 %>% ### SUMMARISING AND MAKING SURE EACH PARTICIPANT (SEQN is the participant ID) appeared only once
  group_by(SEQN) %>%
  summarise(
    GPT_glycemic = ifelse(any(GPT_glycemic == "Yes"), "Yes", "No"),
    GPT_bloodpressure = ifelse(any(GPT_bloodpressure == "Yes"), "Yes", "No"),
    GPT_insulin = ifelse(any(GPT_insulin == "Yes"), "Yes", "No")
  )
```

Visualising:

``` r
head(medsfull4)
```

    ## # A tibble: 6 × 4
    ##    SEQN GPT_glycemic GPT_bloodpressure GPT_insulin
    ##   <dbl> <chr>        <chr>             <chr>      
    ## 1 41475 No           Yes               No         
    ## 2 41476 No           No                No         
    ## 3 41477 Yes          Yes               No         
    ## 4 41482 No           Yes               No         
    ## 5 41483 No           Yes               No         
    ## 6 41486 No           Yes               No

#### Merging all dataframes

All data sets were merged together, forming a single df:

``` r
FULLALL <- DEMOGRAPHICS %>%
  merge(.,BODYCOMP, by = 'SEQN', all = T) %>%
  merge(.,OGTT2H, by = 'SEQN', all = T) %>%
  merge(.,GLUCINSULIN, by = 'SEQN', all = T) %>%
  merge(.,TOTALDIET, by = 'SEQN', all = T) %>%
  merge(.,COFFEEANDTEA, by = 'SEQN', all = T) %>%
  merge(.,PHYSICALACT, by = 'SEQN', all = T) %>%
  merge(.,GLUCOHEMOGL, by = 'SEQN',all = T) %>%
  merge(.,MEDSFULL4, by = 'SEQN', all = T)
```

``` r
head(FULLALL)
```

    ##    SEQN                    BMDSTATS BMXWT BMIWT BMXRECUM BMIRECUM BMXHEAD
    ## 1 31127 Complete data for age group  10.2  <NA>     73.6     <NA>      NA
    ## 2 31128 Complete data for age group  40.1  <NA>       NA     <NA>      NA
    ## 3 31129 Complete data for age group  74.6  <NA>       NA     <NA>      NA
    ## 4 31130  No body measures exam data    NA  <NA>       NA     <NA>      NA
    ## 5 31131          Other partial exam  75.2  <NA>       NA     <NA>      NA
    ## 6 31132 Complete data for age group  69.5  <NA>       NA     <NA>      NA
    ##   BMIHEAD BMXHT BMIHT BMXBMI BMXLEG BMILEG BMXCALF BMICALF BMXARML BMIARML
    ## 1      NA    NA  <NA>     NA     NA     NA      NA      NA    16.0      NA
    ## 2      NA 151.6  <NA>  17.45   37.6     NA    29.3      NA    34.3      NA
    ## 3      NA 167.7  <NA>  26.53   42.7     NA    40.6      NA    36.5      NA
    ## 4      NA    NA  <NA>     NA     NA     NA      NA      NA      NA      NA
    ## 5      NA 156.0  <NA>  30.90   38.0     NA    36.6      NA    35.0      NA
    ## 6      NA 167.6  <NA>  24.74   40.4     NA    35.6      NA    37.5      NA
    ##   BMXARMC BMIARMC BMXWAIST BMIWAIST BMXTHICR BMITHICR BMXTRI           BMITRI
    ## 1    15.5      NA       NA       NA       NA       NA   12.8             <NA>
    ## 2    21.7      NA     62.8       NA     39.5       NA   10.4             <NA>
    ## 3    32.6      NA     97.8       NA     55.9       NA   18.8             <NA>
    ## 4      NA      NA       NA       NA       NA       NA     NA             <NA>
    ## 5    35.8      NA     96.0       NA     53.7       NA     NA Exceeds capacity
    ## 6    31.2      NA     96.5       NA     48.0       NA   10.4             <NA>
    ##   BMXSUB           BMISUB BMDBMIC BMXSAD1 BMXSAD2 BMXSAD3 BMXSAD4 BMDAVSAD
    ## 1   10.0             <NA>    <NA>      NA      NA      NA      NA       NA
    ## 2    8.4             <NA>    <NA>      NA      NA      NA      NA       NA
    ## 3   17.6             <NA>    <NA>      NA      NA      NA      NA       NA
    ## 4     NA             <NA>    <NA>      NA      NA      NA      NA       NA
    ## 5     NA Could not obtain    <NA>      NA      NA      NA      NA       NA
    ## 6   22.2             <NA>    <NA>      NA      NA      NA      NA       NA
    ##   BMDSADCM RIAGENDR RIDAGEYR                RIDEXPRG           RIDRETH1
    ## 1     <NA>     Male        0                    <NA> Non-Hispanic White
    ## 2     <NA>   Female       11 SP not pregnant at exam Non-Hispanic Black
    ## 3     <NA>     Male       15                    <NA> Non-Hispanic Black
    ## 4     <NA>   Female       85                    <NA> Non-Hispanic White
    ## 5     <NA>   Female       44 SP not pregnant at exam Non-Hispanic Black
    ## 6     <NA>     Male       70                    <NA> Non-Hispanic White
    ##     DMDEDUC3                  DMDEDUC2           INDFMINC SDMVPSU SDMVSTRA
    ## 1       <NA>                      <NA> $15,000 to $19,999       2       44
    ## 2  4th Grade                      <NA> $20,000 to $24,999       1       52
    ## 3 10th Grade                      <NA> $65,000 to $74,999       1       51
    ## 4       <NA> Some College or AA degree $15,000 to $19,999       2       46
    ## 5       <NA> Some College or AA degree   $75,000 and Over       1       48
    ## 6       <NA> College Graduate or above   $75,000 and Over       2       52
    ##    WTINT2YR  WTMEC2YR       TUA        AMA ARMCFATCIRC ARMFATINDEX WAISTHEIGHT
    ## 1  6434.950  6571.396  188.6919 48.5981226    130.0938   0.6894508          NA
    ## 2  9081.701  8987.042  369.8361  9.5809009    353.7552   0.9565188   0.4142480
    ## 3  5316.895  5586.719  834.6898 55.7228805    768.9669   0.9212607   0.5831843
    ## 4 29960.840 34030.995        NA         NA          NA          NA          NA
    ## 5 26457.708 26770.585 1006.5977         NA          NA          NA   0.6153846
    ## 6 32961.510 35315.539  764.5380  0.1725593    754.3654   0.9866945   0.5757757
    ##     BFPRED WTSOG2YR LBXGLT WTSAF2YR LBXGLU LBXIN    WTDRD1    WTDR2D   DR1DAY
    ## 1       NA       NA     NA       NA     NA    NA  4718.873  5906.472 Saturday
    ## 2 25.85972       NA     NA       NA     NA    NA  3933.789  4250.890   Friday
    ## 3 30.39665       NA     NA       NA     NA    NA 26719.806 26719.806   Monday
    ## 4       NA       NA     NA     0.00     NA    NA        NA        NA     <NA>
    ## 5       NA 71634.17    126 67556.81     90 10.03 13595.896 14822.479   Friday
    ## 6 27.97084 80226.50     NA 80193.96    157  8.99 40484.136 45660.639  Tuesday
    ##   DR1TKCAL DR1TPROT DR1TCARB DR1TTFAT DR1TMFAT DR1TPFAT DR1TSFAT DR1TCAFF
    ## 1     1421    50.42   171.45    61.28   20.509    8.098   25.994        0
    ## 2     1402    32.51   180.88    65.05   17.128   17.755   23.381       33
    ## 3     4110   139.89   684.22    94.73   37.018   12.180   38.599        7
    ## 4       NA       NA       NA       NA       NA       NA       NA       NA
    ## 5     1458    27.43   233.73    50.86   20.745    7.641   18.274       13
    ## 6     2168    75.81   269.93    60.16   17.494   19.114   17.608      260
    ##   DR1TALCO DR1TSUGR DR1TFIBE    DR2DAY DR2TKCAL DR2TPROT DR2TCARB DR2TTFAT
    ## 1      0.0   139.02      4.6    Sunday     1302    46.11   165.96    51.85
    ## 2      0.0    88.69     10.0    Monday     1797    94.60   158.68    87.39
    ## 3      0.0   498.82     14.5    Sunday     3092    79.47   420.63   125.23
    ## 4       NA       NA       NA      <NA>       NA       NA       NA       NA
    ## 5      0.0   146.01      6.4 Wednesday     2044    58.33   239.08   100.15
    ## 6     40.8   111.54     20.3    Sunday     1446    54.53   183.59    42.75
    ##   DR2TMFAT DR2TPFAT DR2TSFAT DR2TCAFF DR2TALCO DR2TSUGR DR2TFIBE CAFFother1
    ## 1   17.254    5.816   23.509        0      0.0   103.67      5.6         NA
    ## 2   31.070   17.140   31.164        0      0.0    87.13      5.5         33
    ## 3   49.236   31.040   38.181        0      0.0   195.93     19.4          7
    ## 4       NA       NA       NA       NA       NA       NA       NA         NA
    ## 5   32.824   17.284   42.619       19      0.0   118.39      9.9          5
    ## 6   13.705    5.343   20.341      302     21.2   109.35     18.6         NA
    ##   CAFFcoffeetea1 CAFFother2 CAFFcoffeetea2 PAQ605 PAQ610 PAD615 PAQ620 PAQ625
    ## 1             NA         NA             NA   <NA>     NA     NA   <NA>     NA
    ## 2             NA         NA             NA   <NA>     NA     NA   <NA>     NA
    ## 3             NA         NA             NA   <NA>     NA     NA   <NA>     NA
    ## 4             NA         NA             NA   <NA>     NA     NA   <NA>     NA
    ## 5              8          8             11   <NA>     NA     NA   <NA>     NA
    ## 6            260         NA            302   <NA>     NA     NA   <NA>     NA
    ##   PAD630 PAQ635 PAQ640 PAD645 PAQ650 PAQ655 PAD660 PAQ665 PAQ670 PAD675 LBXGH
    ## 1     NA   <NA>     NA     NA   <NA>     NA     NA   <NA>     NA     NA    NA
    ## 2     NA   <NA>     NA     NA   <NA>     NA     NA   <NA>     NA     NA    NA
    ## 3     NA   <NA>     NA     NA   <NA>     NA     NA   <NA>     NA     NA    NA
    ## 4     NA   <NA>     NA     NA   <NA>     NA     NA   <NA>     NA     NA    NA
    ## 5     NA   <NA>     NA     NA   <NA>     NA     NA   <NA>     NA     NA    NA
    ## 6     NA   <NA>     NA     NA   <NA>     NA     NA   <NA>     NA     NA    NA
    ##   GPT_glycemic GPT_bloodpressure GPT_insulin
    ## 1         <NA>              <NA>        <NA>
    ## 2         <NA>              <NA>        <NA>
    ## 3         <NA>              <NA>        <NA>
    ## 4         <NA>              <NA>        <NA>
    ## 5         <NA>              <NA>        <NA>
    ## 6         <NA>              <NA>        <NA>

As you can see, there are plenty of missing values and variables to
adjust.

### Final data wrangling procedures

Most of the categories I have created, when merged into the FULL
demographic dataset, will generate missing values. First, since the
method for the assessment of physical activity changed between the 05-06
cycle and the next, I decided to not use the 05-06 cycle. I used the
SEQN variable to exclude those participants:

``` r
FULLDATA2 <- FULLDATA %>%
  filter(SEQN >= 41475) 
```

Then, I corrected the information on medication:

1.  first, if the value was NA, it meant that the participant did NOT
    take medication. Therefore, I imputed “No” for all these cases.

2.  GPT sometimes replied with final stops. I removed those with gsub().

``` r
FULLDATA3 <- FULLDATA2 %>%
  mutate(GPT_glycemic = ifelse(is.na(GPT_glycemic), "No", GPT_glycemic),
         GPT_bloodpressure = ifelse(is.na(GPT_bloodpressure), "No", GPT_bloodpressure),
         GPT_insulin = ifelse(is.na(GPT_insulin), "No", GPT_insulin)) %>%
  mutate(GPT_glycemic = gsub("No.", "No", GPT_glycemic),
         GPT_glycemic = gsub("Yes.", "Yes", GPT_glycemic)) %>%
  mutate(GPT_bloodpressure = gsub("No.", "No", GPT_bloodpressure),
         GPT_bloodpressure = gsub("Yes.", "Yes", GPT_bloodpressure)) 
```

Then, I decided to use MEAN values of caffeine data based on the food
records. Since the first day of data collections was done in person,
only the second day (DR2) could have non-responders (NA). Therefore, I
only calculated mean values for those participants that responded to
both food records. Otherwise, only the values from DAY1 were used.

For the coffee and tea specific caffeine data, NA values were considered
zero (0 mg) when participants responded to ANY food record (DAY 1 or
DAY1/DAY2), but did not consume any caffeine sources.

See it below:

``` r
FULLDATA4 <- FULLDATA3 %>%
  mutate(totalcaff = case_when(!is.na(DR1TCAFF) &  is.na(DR2TCAFF) ~ DR1TCAFF,
                              !is.na(DR1TCAFF) &  !is.na(DR2TCAFF) ~ (DR1TCAFF + DR2TCAFF)/2)) %>%
  mutate(coffeetea = case_when(!is.na(CAFFcoffeetea1) &  !is.na(CAFFcoffeetea2) ~ (CAFFcoffeetea1 + CAFFcoffeetea2)/2,
                               !is.na(CAFFcoffeetea1) &  is.na(CAFFcoffeetea2) & !is.na(DR2TCAFF) ~ CAFFcoffeetea1/2,
                               !is.na(CAFFcoffeetea1) &  is.na(CAFFcoffeetea2) & is.na(DR2TCAFF) ~ CAFFcoffeetea1,
                               is.na(CAFFcoffeetea1) &  !is.na(CAFFcoffeetea2) ~ CAFFcoffeetea2/2,
                               totalcaff == 0 ~ 0
                               ) ) %>%
  mutate(coffeetea = ifelse(is.na(coffeetea) & !is.na(totalcaff),0, coffeetea)) %>%
```

For other dietary information (macro nutrients, fiber, sugar, calories),
I also calculated a mean value between food records 1 and 2, and divided
that value by the participant’s body weight. This way, we can look at
food ingestion in a more ‘individual’ level.

``` r
FULLDATA5 <- FULLDATA4 %>%
  mutate(SUGARbw = rowMeans(.[c('DR1TSUGR', 'DR2TSUGR')], na.rm=T) / BMXWT)%>%
  mutate(CHObw = rowMeans(.[c('DR1TCARB', 'DR2TCARB')], na.rm=T) / BMXWT) %>%
  mutate(KCALbw = rowMeans(.[c('DR1TKCAL', 'DR2TKCAL')], na.rm=T) / BMXWT)  %>%
  mutate(FIBERbw = rowMeans(.[c('DR1TFIBE', 'DR2TFIBE')], na.rm=T) / BMXWT)%>%
  mutate(FATbw = rowMeans(.[c('DR1TTFAT', 'DR2TTFAT')], na.rm=T) / BMXWT) %>%
  mutate(PTNbw = rowMeans(.[c('DR1TPROT', 'DR2TPROT')], na.rm=T) / BMXWT) 
```

I categorised participants by exercise levels (tertiles), age
(quartiles), caffeine consumption from any source (totalcaff) or from
coffee and tea (quintiles), and pregnancy status.

``` r
df %>%
  mutate(exercisecat = case_when(totalMET <= 240 ~ 1,
                                 totalMET > 240 & totalMET <= 2480 ~ 2 ,
                                 totalMET > 2480  ~ 3 ) ) %>%
  mutate(exercisecat = factor(exercisecat, labels = c('low', 'moderate', 'high'))) %>%
  mutate(agecat = case_when(RIDAGEYR <= 32 ~ 1,
                            RIDAGEYR >32 & RIDAGEYR <= 48 ~ 2,
                            RIDAGEYR > 48 & RIDAGEYR <= 63 ~3,
                            RIDAGEYR >63~ 4) ) %>%
  mutate(agecat = factor(agecat)) %>%
   mutate(totalcaffcat = case_when(totalcaff <= 22.5      ~ 1,
                                  totalcaff >22.5      & totalcaff <= 82.0    ~2,
                                  totalcaff >82.0    & totalcaff <= 154.0     ~3,
                                  totalcaff >154.0    & totalcaff <= 267.0     ~4,
                                  totalcaff > 267.0    ~ 5)) %>%
  mutate(totalcaffcat = factor(totalcaffcat)) %>%
  mutate(coffeeteacat = case_when(coffeetea <= 0 ~ 1,
                                  coffeetea >0 & coffeetea <= 31 ~2,
                                  coffeetea >31 & coffeetea <= 95.5   ~3,
                                  coffeetea >95.5   & coffeetea <= 195.0    ~4,
                                  coffeetea > 195.0   ~ 5)) %>%
  mutate(coffeeteacat = factor(coffeeteacat))
  mutate(pregnant = case_when(RIDEXPRG == 'Yes, positive lab pregnancy test or self-reported pregnant at exam' ~ "Yes",
                              RIDEXPRG != 'Yes, positive lab pregnancy test or self-reported pregnant at exam'~'No',
                              is.na(RIDEXPRG) ~ 'No'))
```

Finally, the CDC recommends that participant weights (sample weights)
are divided by the total number of cycles that are merged together.

Thus:

``` r
df %>% 
  mutate_at(vars(WTDR2D,
         WTINT2YR,
         WTMEC2YR,
         WTDRD1,
        WTSOG2YR,
        WTSAF2YR), funs (./5))
```

## Survey Design

#### Define Survey Design

The survey design is defined using the NHANES dataset, considering
appropriate weights, stratification, and cluster variables. This step
ensures that the survey data is properly adjusted for complex sampling.

#### OGTT2H

For the creation of this specific survey design, we are using the sample
weights calculated for those individuals for who we have the results of
the OGTT2H test (WTSOG2YR). Those who did not get tested have an empty
weight (NA). To solve that, we have two choices:

1.  We remove the observations with NA for weights, as I did below.
2.  We replace NA values with zero (0)

``` r
# Define the survey design
dataset <- FULLDATA2[!is.na(FULLDATA2$WTSOG2YR), ]
survey1 <- svydesign(
  ids = ~SDMVPSU, ### Cluster variable
  strata = ~SDMVSTRA, ### Strata
  nest = TRUE,  # signaling that we are considering the values nested by cluster and strata
  weights = ~WTSOG2YR,
  data = dataset
)
```

It was decided that this analysis would include only individuals older
than 18 years of age (RIDAGEYR) and that were not pregnant at the time
of data collection.

It is recommended by the creators of the ‘survey’ package that a subset
is done after the survey design is already created, as shown below:

``` r
# Subset for analysis
surveysub1 <- subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
```

## Exploratory analysis

Let’s check the distribution of our variable of interest:

``` r
svyhist(~LBXGLT, design = surveysub1) 
```

![](NHANES-Git_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

It seems that these values are positively skewed. I will first run the
svyglm and check the residuals for the model.

``` r
# Fit the generalized linear model
GLM1 <- svyglm(LBXGLT ~ 1,design = surveysub1)

# Model summary
logLik(GLM1)
```

    ## Warning in logLik.svyglm(GLM1): svyglm not fitted by maximum likelihood.

    ## [1] -12530759

``` r
summary(GLM1)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ 1, design = surveysub1)
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 115.0449     0.6099   188.6   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 2547.42)
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
hist(resid(GLM1))
```

![](NHANES-Git_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

The residuals follow the same distribution.

## Fixing Skewness

### Gamma distribution

Next, I’ll use the Gamma distribution to fit the data and see if the
residuals look closer to a normal distribution.

``` r
# Fit the generalized linear model
GLM2 <- svyglm(LBXGLT ~ 1,design = surveysub1, family = Gamma(link = 'identity' ))

# Model summary
logLik(GLM2)
```

    ## Warning in logLik.svyglm(GLM2): svyglm not fitted by maximum likelihood.

    ## [1] -755.0488

``` r
summary(GLM2)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ 1, design = surveysub1, family = Gamma(link = "identity"))
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 115.0449     0.6099   188.6   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.1924714)
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
hist(resid(GLM2))
```

![](NHANES-Git_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

It does seem better.

### Box-Cox Transformation

Another option for the skewness would be a Box-Cox transformation of the
data. This helps in stabilizing the variance and making the data more
normally distributed.

#### Find Optimal Lambda for Box-Cox Transformation

A Box-Cox transformation is applied to determine the optimal lambda for
transforming the response variable (LBXGLT).

``` r
# Apply Box-Cox transformation to find optimal lambda
box1 <- boxCox(glm(LBXGLT ~ 1, data = surveysub1$variables, weights =surveysub1$variables$WTSOG2YR ))
```

![](NHANES-Git_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
lambda <- box1$x[box1$y == max(box1$y)]
lambda
```

    ## [1] 1.111111

Using the optimal lambda, a generalized linear model (GLM) is fitted to
assess the relationship between LBXGLT (OGTT2H) and several predictors,
such as demographic and health-related variables.

``` r
# Create the transformation object
tran1 <- make.tran('boxcox', lambda)

# Fit the generalized linear model
GLM3 <- with(tran1, svyglm(linkfun(LBXGLT) ~ 1,
                           design = surveysub1))

# Model summary
logLik(GLM3)
```

    ## Warning in logLik.svyglm(GLM3): svyglm not fitted by maximum likelihood.

    ## [1] -38518201

``` r
summary(GLM3)
```

    ## 
    ## Call:
    ## svyglm(formula = linkfun(LBXGLT) ~ 1, design = surveysub1)
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  176.081      1.058   166.5   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 7830.494)
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
anova(GLM3)
```

    ## NULL

``` r
hist(resid(GLM3))
```

![](NHANES-Git_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

Using the Gamma distribution to fit our data had an apparent better
performance. We chose model 2, then.

## Fitting the Gamma Model

``` r
# Fit the generalized linear model
GLM2.2 <- svyglm(LBXGLT ~ totalcaff, design = surveysub1, family = Gamma(link = 'identity' ))

# Model summary
logLik(GLM2.2)
```

    ## [1] -734.0867

``` r
summary(GLM2.2)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ totalcaff, design = surveysub1, family = Gamma(link = "identity"))
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 116.705493   0.902136 129.366   <2e-16 ***
    ## totalcaff    -0.011363   0.004334  -2.622   0.0105 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.1921386)
    ## 
    ## Number of Fisher Scoring iterations: 5

It seems that total habitual caffeine consumption does have an effect on
the results of an OGTT2h test. However, other factors could affect this
test, that reflects an individual’s insulin sensitivity.

Factors could be: - Physical activity level. - Sex. - Race/Ethnicity. -
Age. - BMI. - The waist / height ratio (reflects an augmented central
obesity). - Dietary habits. - Medication usage.

I’ve performed a backwards stepwise method, which consists of including
all potential confounding variables in the model, removing the least
significant one until an ideal model is reached.

## Stepwise Procedure

``` r
# Fit the generalized linear model
GLM2.3 <- svyglm(LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1  + RIDAGEYR+  BMXBMI + WAISTHEIGHT + CHObw + FATbw + FIBERbw + GPT_glycemic + GPT_bloodpressure + totalcaff, design = surveysub1, family = Gamma(link = 'identity' ))

# Model summary
logLik(GLM2.3)
```

    ## [1] -560.2228

``` r
summary(GLM2.3)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1 + 
    ##     RIDAGEYR + BMXBMI + WAISTHEIGHT + CHObw + FATbw + FIBERbw + 
    ##     GPT_glycemic + GPT_bloodpressure + totalcaff, design = surveysub1, 
    ##     family = Gamma(link = "identity"))
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  34.69508    4.90928   7.067
    ## exercisecatmoderate                          -2.51605    1.19021  -2.114
    ## exercisecathigh                              -6.40064    1.21219  -5.280
    ## RIAGENDRFemale                               -2.35794    1.13091  -2.085
    ## RIDRETH1Other Hispanic                       -3.83993    2.02743  -1.894
    ## RIDRETH1Non-Hispanic White                   -6.79365    1.47376  -4.610
    ## RIDRETH1Non-Hispanic Black                   -7.12240    1.70025  -4.189
    ## RIDRETH1Other Race - Including Multi-Racial   1.14517    2.47570   0.463
    ## RIDAGEYR                                      0.58246    0.03315  17.570
    ## BMXBMI                                       -0.87325    0.18574  -4.701
    ## WAISTHEIGHT                                 162.15542   14.45845  11.215
    ## CHObw                                         0.33603    0.53201   0.632
    ## FATbw                                        -1.60042    1.01887  -1.571
    ## FIBERbw                                     -11.59472    4.27474  -2.712
    ## GPT_glycemicYes                              57.30320   37.58919   1.524
    ## GPT_bloodpressureYes                          6.15375    1.81415   3.392
    ## totalcaff                                    -0.01961    0.00469  -4.180
    ##                                             Pr(>|t|)    
    ## (Intercept)                                 1.53e-09 ***
    ## exercisecatmoderate                           0.0385 *  
    ## exercisecathigh                             1.70e-06 ***
    ## RIAGENDRFemale                                0.0411 *  
    ## RIDRETH1Other Hispanic                        0.0628 .  
    ## RIDRETH1Non-Hispanic White                  2.02e-05 ***
    ## RIDRETH1Non-Hispanic Black                  8.88e-05 ***
    ## RIDRETH1Other Race - Including Multi-Racial   0.6453    
    ## RIDAGEYR                                     < 2e-16 ***
    ## BMXBMI                                      1.45e-05 ***
    ## WAISTHEIGHT                                  < 2e-16 ***
    ## CHObw                                         0.5299    
    ## FATbw                                         0.1212    
    ## FIBERbw                                       0.0086 ** 
    ## GPT_glycemicYes                               0.1324    
    ## GPT_bloodpressureYes                          0.0012 ** 
    ## totalcaff                                   9.17e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.1445321)
    ## 
    ## Number of Fisher Scoring iterations: 8

``` r
Anova(GLM2.3, test = "F", type = 'III')
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: LBXGLT
    ##                   Df        F    Pr(>F)    
    ## (Intercept)        1  49.9459 1.526e-09 ***
    ## exercisecat        2  16.7850 1.434e-06 ***
    ## RIAGENDR           1   4.3472  0.041128 *  
    ## RIDRETH1           4  12.4663 1.575e-07 ***
    ## RIDAGEYR           1 308.7199 < 2.2e-16 ***
    ## BMXBMI             1  22.1027 1.452e-05 ***
    ## WAISTHEIGHT        1 125.7823 < 2.2e-16 ***
    ## CHObw              1   0.3989  0.529921    
    ## FATbw              1   2.4674  0.121242    
    ## FIBERbw            1   7.3570  0.008602 ** 
    ## GPT_glycemic       1   2.3240  0.132399    
    ## GPT_bloodpressure  1  11.5063  0.001203 ** 
    ## totalcaff          1  17.4719 9.165e-05 ***
    ## Residuals         63                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### REMOVE THE VARIABLE CHObw

``` r
GLM2.4 <- svyglm(LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1  + RIDAGEYR+  BMXBMI + WAISTHEIGHT 
                 + FATbw + FIBERbw + GPT_glycemic + GPT_bloodpressure + totalcaff, design = surveysub1, family = Gamma(link = 'identity' ))

# Model summary
logLik(GLM2.4)
```

    ## [1] -560.2848

``` r
summary(GLM2.4)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1 + 
    ##     RIDAGEYR + BMXBMI + WAISTHEIGHT + FATbw + FIBERbw + GPT_glycemic + 
    ##     GPT_bloodpressure + totalcaff, design = surveysub1, family = Gamma(link = "identity"))
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                  35.515326   4.707736   7.544
    ## exercisecatmoderate                          -2.567567   1.194248  -2.150
    ## exercisecathigh                              -6.419076   1.214110  -5.287
    ## RIAGENDRFemale                               -2.414867   1.130730  -2.136
    ## RIDRETH1Other Hispanic                       -3.791620   2.014385  -1.882
    ## RIDRETH1Non-Hispanic White                   -6.843518   1.486882  -4.603
    ## RIDRETH1Non-Hispanic Black                   -7.077140   1.693138  -4.180
    ## RIDRETH1Other Race - Including Multi-Racial   1.126410   2.475975   0.455
    ## RIDAGEYR                                      0.577664   0.034897  16.553
    ## BMXBMI                                       -0.895898   0.185266  -4.836
    ## WAISTHEIGHT                                 162.829220  14.507092  11.224
    ## FATbw                                        -1.151239   0.844632  -1.363
    ## FIBERbw                                     -10.173986   3.605010  -2.822
    ## GPT_glycemicYes                              57.358324  37.541045   1.528
    ## GPT_bloodpressureYes                          6.169442   1.815550   3.398
    ## totalcaff                                    -0.019263   0.004939  -3.900
    ##                                             Pr(>|t|)    
    ## (Intercept)                                 2.05e-10 ***
    ## exercisecatmoderate                         0.035345 *  
    ## exercisecathigh                             1.60e-06 ***
    ## RIAGENDRFemale                              0.036538 *  
    ## RIDRETH1Other Hispanic                      0.064347 .  
    ## RIDRETH1Non-Hispanic White                  2.03e-05 ***
    ## RIDRETH1Non-Hispanic Black                  9.03e-05 ***
    ## RIDRETH1Other Race - Including Multi-Racial 0.650695    
    ## RIDAGEYR                                     < 2e-16 ***
    ## BMXBMI                                      8.69e-06 ***
    ## WAISTHEIGHT                                  < 2e-16 ***
    ## FATbw                                       0.177657    
    ## FIBERbw                                     0.006348 ** 
    ## GPT_glycemicYes                             0.131469    
    ## GPT_bloodpressureYes                        0.001172 ** 
    ## totalcaff                                   0.000233 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.1446388)
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
Anova(GLM2.4, test = "F", type = 'III')
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: LBXGLT
    ##                   Df        F    Pr(>F)    
    ## (Intercept)        1  56.9125 2.053e-10 ***
    ## exercisecat        2  16.8275 1.341e-06 ***
    ## RIAGENDR           1   4.5611 0.0365382 *  
    ## RIDRETH1           4  12.5859 1.299e-07 ***
    ## RIDAGEYR           1 274.0089 < 2.2e-16 ***
    ## BMXBMI             1  23.3845 8.685e-06 ***
    ## WAISTHEIGHT        1 125.9807 < 2.2e-16 ***
    ## FATbw              1   1.8578 0.1776569    
    ## FIBERbw            1   7.9647 0.0063476 ** 
    ## GPT_glycemic       1   2.3344 0.1314691    
    ## GPT_bloodpressure  1  11.5472 0.0011717 ** 
    ## totalcaff          1  15.2126 0.0002331 ***
    ## Residuals         64                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### REMOVE THE VARIABLE FATbw

``` r
GLM2.5 <- svyglm(LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1  + RIDAGEYR+  BMXBMI + WAISTHEIGHT 
                 + FIBERbw + GPT_glycemic + GPT_bloodpressure + totalcaff, 
                 design = surveysub1, family = Gamma(link = 'identity' ))

# Model summary
logLik(GLM2.5)
```

    ## [1] -560.4111

``` r
summary(GLM2.5)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1 + 
    ##     RIDAGEYR + BMXBMI + WAISTHEIGHT + FIBERbw + GPT_glycemic + 
    ##     GPT_bloodpressure + totalcaff, design = surveysub1, family = Gamma(link = "identity"))
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                  33.963843   4.298156   7.902
    ## exercisecatmoderate                          -2.511360   1.195477  -2.101
    ## exercisecathigh                              -6.415187   1.211889  -5.294
    ## RIAGENDRFemale                               -2.274807   1.112223  -2.045
    ## RIDRETH1Other Hispanic                       -3.788567   2.018372  -1.877
    ## RIDRETH1Non-Hispanic White                   -6.879290   1.490931  -4.614
    ## RIDRETH1Non-Hispanic Black                   -7.203922   1.721339  -4.185
    ## RIDRETH1Other Race - Including Multi-Racial   1.190745   2.461245   0.484
    ## RIDAGEYR                                      0.582891   0.035068  16.622
    ## BMXBMI                                       -0.872403   0.184313  -4.733
    ## WAISTHEIGHT                                 162.650246  14.536853  11.189
    ## FIBERbw                                     -12.110665   3.352573  -3.612
    ## GPT_glycemicYes                              57.300015  37.486922   1.529
    ## GPT_bloodpressureYes                          6.164999   1.814887   3.397
    ## totalcaff                                    -0.019680   0.004951  -3.975
    ##                                             Pr(>|t|)    
    ## (Intercept)                                 4.36e-11 ***
    ## exercisecatmoderate                         0.039549 *  
    ## exercisecathigh                             1.52e-06 ***
    ## RIAGENDRFemale                              0.044879 *  
    ## RIDRETH1Other Hispanic                      0.065002 .  
    ## RIDRETH1Non-Hispanic White                  1.91e-05 ***
    ## RIDRETH1Non-Hispanic Black                  8.74e-05 ***
    ## RIDRETH1Other Race - Including Multi-Racial 0.630155    
    ## RIDAGEYR                                     < 2e-16 ***
    ## BMXBMI                                      1.24e-05 ***
    ## WAISTHEIGHT                                  < 2e-16 ***
    ## FIBERbw                                     0.000592 ***
    ## GPT_glycemicYes                             0.131232    
    ## GPT_bloodpressureYes                        0.001167 ** 
    ## totalcaff                                   0.000179 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.1446127)
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
Anova(GLM2.5, test = "F", type = 'III')
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: LBXGLT
    ##                   Df        F    Pr(>F)    
    ## (Intercept)        1  62.4409 4.359e-11 ***
    ## exercisecat        2  16.8478 1.274e-06 ***
    ## RIAGENDR           1   4.1832 0.0448794 *  
    ## RIDRETH1           4  12.9585 8.160e-08 ***
    ## RIDAGEYR           1 276.2787 < 2.2e-16 ***
    ## BMXBMI             1  22.4038 1.237e-05 ***
    ## WAISTHEIGHT        1 125.1897 < 2.2e-16 ***
    ## FIBERbw            1  13.0491 0.0005917 ***
    ## GPT_glycemic       1   2.3364 0.1312323    
    ## GPT_bloodpressure  1  11.5390 0.0011672 ** 
    ## totalcaff          1  15.8017 0.0001791 ***
    ## Residuals         65                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### REMOVE THE VARIABLE GPT_glycemic

``` r
GLM2.6 <- svyglm(LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1  + RIDAGEYR+  BMXBMI + WAISTHEIGHT 
                 + FIBERbw + GPT_bloodpressure + totalcaff, 
                 design = surveysub1, family = Gamma(link = 'identity' ))

# Model summary
logLik(GLM2.6)
```

    ## [1] -560.6909

``` r
summary(GLM2.6)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1 + 
    ##     RIDAGEYR + BMXBMI + WAISTHEIGHT + FIBERbw + GPT_bloodpressure + 
    ##     totalcaff, design = surveysub1, family = Gamma(link = "identity"))
    ## 
    ## Survey design:
    ## subset(survey1, RIDAGEYR >= 18 & pregnant == "No")
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  33.87822    4.29782   7.883
    ## exercisecatmoderate                          -2.48748    1.19616  -2.080
    ## exercisecathigh                              -6.38655    1.21324  -5.264
    ## RIAGENDRFemale                               -2.29105    1.11185  -2.061
    ## RIDRETH1Other Hispanic                       -3.78734    2.01938  -1.875
    ## RIDRETH1Non-Hispanic White                   -6.87880    1.49287  -4.608
    ## RIDRETH1Non-Hispanic Black                   -7.21888    1.72503  -4.185
    ## RIDRETH1Other Race - Including Multi-Racial   1.18125    2.46316   0.480
    ## RIDAGEYR                                      0.58383    0.03504  16.662
    ## BMXBMI                                       -0.87447    0.18453  -4.739
    ## WAISTHEIGHT                                 162.88597   14.54779  11.197
    ## FIBERbw                                     -12.17105    3.35484  -3.628
    ## GPT_bloodpressureYes                          6.13151    1.81642   3.376
    ## totalcaff                                    -0.01971    0.00495  -3.982
    ##                                             Pr(>|t|)    
    ## (Intercept)                                 4.29e-11 ***
    ## exercisecatmoderate                         0.041455 *  
    ## exercisecathigh                             1.65e-06 ***
    ## RIAGENDRFemale                              0.043289 *  
    ## RIDRETH1Other Hispanic                      0.065149 .  
    ## RIDRETH1Non-Hispanic White                  1.91e-05 ***
    ## RIDRETH1Non-Hispanic Black                  8.62e-05 ***
    ## RIDRETH1Other Race - Including Multi-Racial 0.633121    
    ## RIDAGEYR                                     < 2e-16 ***
    ## BMXBMI                                      1.19e-05 ***
    ## WAISTHEIGHT                                  < 2e-16 ***
    ## FIBERbw                                     0.000558 ***
    ## GPT_bloodpressureYes                        0.001238 ** 
    ## totalcaff                                   0.000173 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.1447183)
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
Anova(GLM2.6, test = "F", type = 'III')
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: LBXGLT
    ##                   Df       F    Pr(>F)    
    ## (Intercept)        1  62.136 4.295e-11 ***
    ## exercisecat        2  16.734 1.322e-06 ***
    ## RIAGENDR           1   4.246 0.0432891 *  
    ## RIDRETH1           4  12.934 7.855e-08 ***
    ## RIDAGEYR           1 277.609 < 2.2e-16 ***
    ## BMXBMI             1  22.457 1.186e-05 ***
    ## WAISTHEIGHT        1 125.364 < 2.2e-16 ***
    ## FIBERbw            1  13.162 0.0005577 ***
    ## GPT_bloodpressure  1  11.395 0.0012378 ** 
    ## totalcaff          1  15.858 0.0001727 ***
    ## Residuals         66                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Some evidence has shown a U-shaped relationship between caffeine
consumption and health variables [(see
here)](https://dmsjournal.biomedcentral.com/articles/10.1186/s13098-024-01417-6).

Therefore, a polynomial model will be tested. \### Polynomial model

``` r
#### POLYNOMIAL MODEL
GLM2.7 <- svyglm(LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1  + RIDAGEYR+  BMXBMI + WAISTHEIGHT 
                 + FIBERbw + GPT_bloodpressure + poly(totalcaff,2), 
                 design = subset(surveysub1, !is.na(totalcaff)), family = Gamma(link = 'identity' ))

# Model summary
logLik(GLM2.7)
```

    ## [1] -540.1787

``` r
summary(GLM2.7)
```

    ## 
    ## Call:
    ## svyglm(formula = LBXGLT ~ exercisecat + RIAGENDR + RIDRETH1 + 
    ##     RIDAGEYR + BMXBMI + WAISTHEIGHT + FIBERbw + GPT_bloodpressure + 
    ##     poly(totalcaff, 2), design = subset(surveysub1, !is.na(totalcaff)), 
    ##     family = Gamma(link = "identity"))
    ## 
    ## Survey design:
    ## subset(surveysub1, !is.na(totalcaff))
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                   30.13561    4.27113   7.056
    ## exercisecatmoderate                           -2.49194    1.20107  -2.075
    ## exercisecathigh                               -6.47650    1.22250  -5.298
    ## RIAGENDRFemale                                -2.46382    1.12921  -2.182
    ## RIDRETH1Other Hispanic                        -3.67327    1.98009  -1.855
    ## RIDRETH1Non-Hispanic White                    -6.05651    1.46083  -4.146
    ## RIDRETH1Non-Hispanic Black                    -7.70121    1.73480  -4.439
    ## RIDRETH1Other Race - Including Multi-Racial    1.45906    2.44481   0.597
    ## RIDAGEYR                                       0.60244    0.03446  17.484
    ## BMXBMI                                        -0.80588    0.18625  -4.327
    ## WAISTHEIGHT                                  159.59970   14.53838  10.978
    ## FIBERbw                                      -11.82300    3.36183  -3.517
    ## GPT_bloodpressureYes                           6.07652    1.82953   3.321
    ## poly(totalcaff, 2)1                         -420.68004   65.20227  -6.452
    ## poly(totalcaff, 2)2                          273.43684   92.34005   2.961
    ##                                             Pr(>|t|)    
    ## (Intercept)                                 1.38e-09 ***
    ## exercisecatmoderate                         0.041971 *  
    ## exercisecathigh                             1.49e-06 ***
    ## RIAGENDRFemale                              0.032739 *  
    ## RIDRETH1Other Hispanic                      0.068117 .  
    ## RIDRETH1Non-Hispanic White                  0.000100 ***
    ## RIDRETH1Non-Hispanic Black                  3.58e-05 ***
    ## RIDRETH1Other Race - Including Multi-Racial 0.552715    
    ## RIDAGEYR                                     < 2e-16 ***
    ## BMXBMI                                      5.33e-05 ***
    ## WAISTHEIGHT                                  < 2e-16 ***
    ## FIBERbw                                     0.000802 ***
    ## GPT_bloodpressureYes                        0.001473 ** 
    ## poly(totalcaff, 2)1                         1.59e-08 ***
    ## poly(totalcaff, 2)2                         0.004275 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 0.137737)
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
Anova(GLM2.7, test = "F", type = 'III')
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: LBXGLT
    ##                    Df        F    Pr(>F)    
    ## (Intercept)         1  49.7822 1.383e-09 ***
    ## exercisecat         2  17.4964 8.331e-07 ***
    ## RIAGENDR            1   4.7607  0.032739 *  
    ## RIDRETH1            4  11.9083 2.565e-07 ***
    ## RIDAGEYR            1 305.6948 < 2.2e-16 ***
    ## BMXBMI              1  18.7216 5.325e-05 ***
    ## WAISTHEIGHT         1 120.5125 < 2.2e-16 ***
    ## FIBERbw             1  12.3681  0.000802 ***
    ## GPT_bloodpressure   1  11.0314  0.001473 ** 
    ## poly(totalcaff, 2)  2  33.0835 1.231e-10 ***
    ## Residuals          65                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

This is the model we’re keeping (GLM2.7).

## Estimated Means and Plot

#### Calculate Estimated Means and Plot Observed Data

Estimated means for different levels of caffeine intake are calculated
using the ‘emmeans’ package. A plot is created to visualize the
relationship between caffeine intake and LBXGLT (OGTT2H), including
confidence intervals.

``` r
# Calculate estimated means for totalcaff
means1 <- data.frame(emmeans(GLM2.7, ~totalcaff, data = surveysub1$variables, at = list(totalcaff = seq(0, 2000, 100) )))

# Plot observed data and fitted values
ggplot(data =  subset(surveysub1$variables, WTSOG2YR > 0), aes(x = totalcaff, y = LBXGLT)) +
  geom_point(pch = 21, aes(alpha = WTSOG2YR)) +
  geom_line(data = means1, size = 1, aes(x = totalcaff, y = emmean)) +
  geom_ribbon(data = means1, alpha = 0.3, aes(y = emmean, ymin = lower.CL, ymax = upper.CL)) +
  theme_classic() +
  scale_x_continuous(limits = c(0,2000))
```

![](NHANES-Git_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

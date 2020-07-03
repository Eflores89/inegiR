# InegiR

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/inegiR)](http://cran.r-project.org/package=inegiR) ![downloads](http://cranlogs.r-pkg.org/badges/grand-total/inegiR)

# v3
Version 3 of *inegiR* is now on [CRAN](https://cran.r-project.org/web/packages/inegiR/index.html). This is a *big* update that breaks with most functionality in previous versions. 

## New API Compatibility

Most importantly, this version is now (finally) compatible with INEGI's updated API (v2.0). Similar to previous versions, you can use the **inegi_series** function to download the data series you want. Now, you just need to add the indicador code. 
 
~~~~~~~
library(inegiR)

token_inegi <- "your-own-token"
gdp <- inegi_series(serie = "381016", token = token_inegi)
tail(gdp)
~~~~~~~

   |date       |date_shortcut |   values|notes |
|:----------|:-------------|--------:|:-----|
|2016-01-01 |Q1            | 14080629|NA    |
|2016-04-01 |Q2            | 14333716|NA    |
|2016-07-01 |Q3            | 14421873|NA    |
|2016-10-01 |Q4            | 15012430|NA    |
|2017-01-01 |Q1            | 14471824|NA    |
|2017-04-01 |Q2            | 14586819|NA    |


There is an added option to download the data in tibbletime format.
 
~~~~~~~
gdp <- inegi_series(serie = "381016", token = token_inegi, as_tt = TRUE)
class(gdp)
[1] "tbl_time"   "tbl_df"     "tbl"        "data.frame"
~~~~~~~


Like previous version, you can also download metadata. However, INEGI's new API does not explicitly serve the "human" metadata in the call. Instead, you get some codes and access to catalogs to describe those codes. For example, for the same data series...

~~~~~~~
gdp <- inegi_series(series_id = "381016", token = token_inegi, metadata = TRUE)
t(gdp$metadata)
~~~~~~~

|                |                                                                                                                                                         |
|:---------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------|
|source          |17                                                                                                                                                       |
|topic           |1160165000100010                                                                                                                                         |
|notes           |2830                                                                                                                                                     |
|last_update     |22/08/2017 02:36:48 p. m.                                                                                                                                |
|region          |00                                                                                                                                                       |
|units           |278                                                                                                                                                      |
|indicator_ID    |381016                                                                                                                                                   |
|frequency       |6                                                                                                                                                        |
|call_local_time |2019-07-06 14:56:02                                                                                                                                      |
|call_unmasked   |https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/381016/en/00/false/BIE/2.0/your-token-will-be-here?type=json |

Beware, not all the date formats are supported (only anual, trimestral, monthly and biweekly data) and there is a new date_shortcut column, which makes it easier to see the frequency of the series. 

## Catalogs
To obtain the code descriptions, there are some new functions, which begin with *incat_*:

~~~~~~~
# obtain the frequency codes.
frequencies <- incat_freq(token_inegi)
~~~~~~~


## Indicator functions
All of specific indicator functions (i.e. unemployment rate) have been rewritten and now start with *inind_*. 


## Multiple series
There is a new function for downloading a vector of more than one series in one go. They do not have to be the same frequency. For example, GDP and unemployment. 


~~~~~~~
series_needed <- c("381016", "444612")
series_names <- c("GDP - old series", "Unemployment rate")

data_for_project <- inegi_series_multiple(series_id = series_needed, 
                                          token = token_inegi, 
                                          names = series_names)
~~~~~~~


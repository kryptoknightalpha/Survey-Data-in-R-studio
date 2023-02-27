# these are the packages used: 
library(foreign)
library(haven)
library(magrittr)
library(survey)
library(tidyverse)
# If you haven't installed them you can install the packages by
# executing : 
# install.packages(c("haven","tidyverse","magrittr","survey"))

# this string needs to match the location/path 
# where you saved the four data sets on your computer:
#dataPath <- "/Users/Admin/Downloads/Yetzenia Assignment" 
# remember R uses slash '/' instead of backslash '\', as windows does, 
# to separate directories. 

ess6 <- read.dta("/C:/Users/Admin/Downloads/Yetzenia Assignment/ESS6e02_4.dta")
sddf_es <- read.dta("/C:/Users/Admin/Downloads/Yetzenia Assignment/ESS6_ES_SDDF.dta")
sddf_dk <- read.dta("/C:/Users/Admin/Downloads/Yetzenia Assignment/ESS6_DK_SDDF.dta")


calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


ess6 %>% 
  filter(cntry == "DK") %>%
  replace_na(list(
    edulvlb =  median(.$edulvlb, na.rm = TRUE),
    agea    =  median(.$agea, na.rm = TRUE),
    gndr    =  median(.$gndr, na.rm = TRUE),
    region  =  calculate_mode(.$region)
  )) %>%
  mutate(
    gndr_c  = as.character(gndr),
    age_c   = as.character(cut(
      agea,
      breaks = c(15, 35, 55, Inf),
      include.lowest = TRUE
    )),
    edulvlb_c =
      case_when((edulvlb >= 0   & edulvlb <= 229) | edulvlb > 800  ~ "low",
                (edulvlb >= 311 & edulvlb <= 423) ~ "medium",
                (edulvlb >= 510 & edulvlb <= 800) ~ "high",
                TRUE ~ as.character(edulvlb) ) ,
    region_c  = as.factor(region),
    gae_c     = as.factor(str_c(gndr_c, age_c, edulvlb_c)),
    pspwght_p = pspwght * pweight * 10000,
    dweight_p = dweight * pweight * 10000,
    trstplt_c =
      case_when((trstplt >= 0   & trstplt <= 3)  ~ "low",
                (trstplt >= 4   & trstplt <= 7)  ~ "medium",
                (trstplt >= 8   & trstplt <= 10) ~ "high",
                TRUE ~ as.character(trstplt) ),
    stfeco_c =
      case_when((stfeco >= 0   & stfeco <= 3)  ~ "low",
                (stfeco >= 4   & stfeco <= 7)  ~ "medium",
                (stfeco >= 8   & stfeco <= 10) ~ "high",
                TRUE ~ as.character(stfeco) )  
  ) %>% 
  left_join(sddf_dk %>% select(idno, psu, stratify), by="idno") %>% 
  mutate(psu = as.factor(psu),
         stratify = as.factor(stratify) )  -> 
  ess_dk

ess6 %>%
  filter(cntry == "ES") %>%
  replace_na(list(
    edulvlb =  median(.$edulvlb, na.rm = TRUE),
    agea    =  median(.$agea, na.rm = TRUE),
    gndr    =  median(.$gndr, na.rm = TRUE),
    region  =  calculate_mode(.$region)
  )) %>%
  mutate(
    gndr_c  = as.character(gndr),
    age_c   = as.character(cut(
      agea,
      breaks = c(15, 35, 55, Inf),
      include.lowest = TRUE
    )),
    edulvlb_c =
      case_when((edulvlb >= 0   & edulvlb <= 229) | edulvlb > 800  ~ "low",
                (edulvlb >= 311 & edulvlb <= 423) ~ "medium",
                (edulvlb >= 510 & edulvlb <= 800) ~ "high",
                TRUE ~ as.character(edulvlb) ) ,
    region_c  = as.factor(str_sub(region, 1, 3)),
    gae_c     = as.factor(str_c(gndr_c, age_c, edulvlb_c)),
    pspwght_p = pspwght * pweight * 10000,
    dweight_p = dweight * pweight * 10000,
    trstplt_c =
      case_when((trstplt >= 0   & trstplt <= 3)  ~ "low",
                (trstplt >= 4   & trstplt <= 7)  ~ "medium",
                (trstplt >= 8   & trstplt <= 10) ~ "high",
                TRUE ~ as.character(trstplt) ),
    stfeco_c =
      case_when((stfeco >= 0   & stfeco <= 3)  ~ "low",
                (stfeco >= 4   & stfeco <= 7)  ~ "medium",
                (stfeco >= 8   & stfeco <= 10) ~ "high",
                TRUE ~ as.character(stfeco) )  
  ) %>% 
  left_join(sddf_es %>% select(idno, psu, stratify), by="idno") %>% 
  mutate(psu = as.factor(psu),
         stratify = as.factor(stratify) )   ->
  ess_es

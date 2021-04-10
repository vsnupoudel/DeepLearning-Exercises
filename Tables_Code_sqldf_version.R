# Taking only the required columns from data_stats
## Load Packages
rm(list=ls())
library(readr)
library(dplyr)
library(stargazer)
library(MASS)
library(statar)
library(sandwich)
library(lmtest)
library(sqldf)
library(tidyverse)
library(qwraps2)
library(htmlTable)
library(pastecs)
library(tidyr)
# Load the dataset
data_stats = read.csv('C:\\Users\\admin\\Downloads\\\\data_all_nutrition_intermediates_13112020_sample2 (1).csv'
                    , na.strings=c("",NA)
                    ,  stringsAsFactors = F
                    , header = T)

just_cols <- data_stats %>% dplyr::select(country, year, population, c_underw2,	c_stunt2,	c_wast2
                            ,	c_underw3	,c_stunt3	,c_wast3, gdp_pc_ppp, gdp_pc_growth)



################################################### Data Cleaning ################################################
############################################### Dietary Diversity Score ###########################################

# Recode Yes to 1 and all other values to 0 for Dietary Diversity Score 
data_stats$v414e <- ifelse(as.character(data_stats$v414e=="yes"),1,0)
data_stats$v414f <- ifelse(as.character(data_stats$v414f=="yes"),1,0)
data_stats$v414g <- ifelse(as.character(data_stats$v414g=="yes"),1,0)
data_stats$v414h <- ifelse(as.character(data_stats$v414h=="yes"),1,0)
data_stats$v414i <- ifelse(as.character(data_stats$v414i=="yes"),1,0)
data_stats$v414j <- ifelse(as.character(data_stats$v414j=="yes"),1,0)
data_stats$v414k <- ifelse(as.character(data_stats$v414k=="yes"),1,0)
data_stats$v414l <- ifelse(as.character(data_stats$v414l=="yes"),1,0)
data_stats$v414m <- ifelse(as.character(data_stats$v414m=="yes"),1,0)
data_stats$v414n <- ifelse(as.character(data_stats$v414n=="yes"),1,0)
data_stats$v414o <- ifelse(as.character(data_stats$v414o=="yes"),1,0)
data_stats$v414p <- ifelse(as.character(data_stats$v414p=="yes"),1,0)


# Create Dietary Diversity Score (DDS)
data_stats$dds <- rowSums(cbind(data_stats$v414e,data_stats$v414f,data_stats$v414g,data_stats$v414h,data_stats$v414i,
                               data_stats$v414j,data_stats$v414k,data_stats$v414l,data_stats$v414m,data_stats$v414n,
                               data_stats$v414o,data_stats$v414p))

# Create quintile for DDS
data_stats$dds_quintile <- xtile(data_stats$dds,5)



############################################### Breastfeeding initiation #########################################

# Recode the values into 0 and 1: It is now a dummy variable that takes the value of 1 if breastfeeding was on time and 0 if delayed. 
data_stats$v426 <- ifelse(data_stats$v426=="hours: number missing" | data_stats$v426=="days: number missing",NA,
                         ifelse(data_stats$v426=="immediately" | data_stats$v426=="within first hour" 
                                |as.numeric(data_stats$v426)==100 |as.numeric(data_stats$v426)==101,1,0))



############################################### Full vaccination #################################################

# Recode the values into Yes and No
data_stats$h2 <- ifelse(data_stats$h2=="don't know" | data_stats$h2=="9",NA,ifelse(data_stats$h2=="no","No","Yes"))
data_stats$h3 <- ifelse(data_stats$h3=="don't know" | data_stats$h3=="9",NA,ifelse(data_stats$h3=="no","No","Yes"))
data_stats$h4 <- ifelse(data_stats$h4=="don't know" | data_stats$h4=="9",NA,ifelse(data_stats$h4=="no","No","Yes"))
data_stats$h5 <- ifelse(data_stats$h5=="don't know" | data_stats$h5=="9",NA,ifelse(data_stats$h5=="no","No","Yes"))
data_stats$h6 <- ifelse(data_stats$h6=="don't know" | data_stats$h6=="9",NA,ifelse(data_stats$h6=="no","No","Yes"))
data_stats$h7 <- ifelse(data_stats$h7=="don't know" | data_stats$h7=="9",NA,ifelse(data_stats$h7=="no","No","Yes"))
data_stats$h8 <- ifelse(data_stats$h8=="don't know" | data_stats$h8=="9",NA,ifelse(data_stats$h8=="no","No","Yes"))
data_stats$h9 <- ifelse(data_stats$h9=="don't know" | data_stats$h9=="9",NA,ifelse(data_stats$h9=="no","No","Yes"))

# Create full vaccination variable  ( Changed to 1 and 0 )
data_stats$full.vaccination <- ifelse(data_stats$h2=="Yes" & data_stats$h3=="Yes" &
                                       data_stats$h4=="Yes" & data_stats$h5=="Yes" &
                                       data_stats$h6=="Yes" & data_stats$h7=="Yes" &
                                       data_stats$h8=="Yes" & data_stats$h9=="Yes",1,0)

########################################### Diarrhea in past 2 weeks- infection.two.weeks ##########################

# Changed to  0 and 1 instead of Yes and No
data_stats$h11 <- ifelse(data_stats$h11=="don't know" | data_stats$h11=="9",NA,ifelse(data_stats$h11=="no",0,1))
# Create infection.two.weeks variable
data_stats$infection.two.weeks <- data_stats$h11


#################################### Oral rehydration therapy for children diarrhea ##################################

# Recode the values into Yes and No
data_stats$v416 <- ifelse(data_stats$v416=="3" | data_stats$v416=="9",NA,
                         ifelse(data_stats$v416=="used ors" | data_stats$v416=="heard of ors",1 ,0 ))


############################################## Indoor pollution ######################################################

# Recode the values into Low and High
data_stats$v161 <- ifelse(data_stats$v161=="88" | data_stats$v161=="99" | data_stats$v161=="12" | 
                           data_stats$v161=="no food cooked in house" | data_stats$v161=="not a dejure resident",NA,
                         ifelse(data_stats$v161=="electricity" | data_stats$v161=="lpg",0 , 1 ))



############################################## Antenatal care #########################################################

# Change the default factor type to character type
data_stats$m14 <- as.character(data_stats$m14)

# Change no antenatal visits to 0
data_stats$m14[data_stats$m14=="no antenatal visits"] <- "0"

# Recode values into Adequate and Inadequate ( 1 and 0)
data_stats$m14 <- ifelse(data_stats$m14=="don't know" | as.numeric(data_stats$m14)>=40,NA,
                        ifelse(as.numeric(data_stats$m14)<=7,0 ,1 ))


# =============================================================================================
# ============================== v190, wasn't there in cleaning ==================================

data_stats$v190 <- as.numeric (  factor(data_stats$v190, order = TRUE, 
                       levels = c("poorest", "poorer", "middle","richer","richest")) )

# ===================================================================================================
# ======================================== Create Tables =============================================
# ===================================================================================================

# Collect the column that need descriptive statistics - Table 2
mean_sd_cols <-  data_stats %>% dplyr::select( dds_quintile
                                      , v426
                                     , full.vaccination
                                    , infection.two.weeks
                                    , v416
                                    , v161
                                    , v190
                                    , mo_low
                                    , water_improved_total
                                    , sani_improved_total
                                    , m14 )


# Descriptive stats for table2


summary_df <- round( stat.desc(mean_sd_cols , basic = FALSE )  , digits = 5)
write.table(summary_df, "table2.csv", sep = ",")


# Percentages with Confidence Intervals for latest year for each country

cols <- c("c_underw2", "c_stunt2", "c_wast2")
country_latest_with_data <- just_cols[!is.na(just_cols$gdp_pc_ppp) &
                                        !is.na(just_cols$gdp_pc_growth) ,]


# max_year <- 
max_year <- sqldf(" SELECT A.country, A.latest_year, B.gdp_pc_ppp, B.gdp_pc_growth 
FROM
( SELECT  country, max(year) as latest_year
                    FROM country_latest_with_data 
                  GROUP BY country ) as A
                  INNER JOIN (
                  SELECT distinct country, gdp_pc_ppp, gdp_pc_growth , year
                   FROM country_latest_with_data
                  ) as B
                  ON A.country = B.country AND A.latest_year= B.year
                  ")


for_na_pop <- sqldf(" 
SELECT distinct A.country, A.population from
(SELECT country, population, year from country_latest_with_data) AS A
INNER JOIN
(SELECT  country, max(year)  as year_with_pop
from country_latest_with_data
where population is NOT  NULL
GROUP BY country
) as B
ON A.country = B.country AND A.year = year_with_pop
")

append_population <- sqldf(" select A.*, B.population FROM 
                           max_year as A LEFT JOIN for_na_pop AS B
                           on A.country = B.country")

# ==========================================================================================
# == join with the three columns c("c_underw2", "c_stunt2", "c_wast2") =====================
# ============================ On country and the year =====================================

with_counts <- sqldf(" select A.*, B.c_underw2, B.c_stunt2, B.c_wast2 
                     FROM append_population as A
                     LEFT JOIN data_stats as B
                     on A.country = B.country
                     AND A.latest_year = B.year")


# ==========================================================================================
# == Generating %s and CI, the country population not needed after all =====================
# ============================                          ====================================
# === replace NAs with NaN =================================================================
with_counts[is.na(with_counts)] <- 0

summary_underw <- with_counts%>%dplyr::group_by(country)%>% dplyr::summarize(
                   N = n(),
                   mean.ci = qwraps2::frmtci(qwraps2::mean_ci(c_underw2) *100 )
                    )
summary_stunt2 <- with_counts%>%dplyr::group_by(country)%>% dplyr::summarize(
                  N = n(),
                  mean.ci = qwraps2::frmtci(qwraps2::mean_ci( c_stunt2) *100 )
                  )
summary_wast2 <- with_counts%>%dplyr::group_by(country)%>% dplyr::summarize(
                    N = n(),
                    mean.ci = qwraps2::frmtci(qwraps2::mean_ci( c_wast2) *100)
                )
# ==========================================================================================
# ================ Final table 1                                            =================
# ============================                          =====================================

final_table1 <- sqldf(" SELECT A.country 
, B.latest_year as Year 
, A.N_Uw 
, A.Percentage_Uw
, A.N_St
, A.Percentage_St
, A.N_Wt 
, A.Percentage_Wt
, B.gdp_pc_ppp
, B.gdp_pc_growth
FROM
 (
SELECT U.country
      , ceil(  (U.N* U.[mean.ci])/100 )   as  N_Uw 
      , U.[mean.ci]                     as Percentage_Uw 
        , ceil( (S.N* S.[mean.ci] )/100 )  as N_St 
       , S.[mean.ci]                       as Percentage_St
       ,  ceil(  (W.N * W.[mean.ci] )/100  )   as N_Wt 
       , W.[mean.ci]                        as Percentage_Wt
       
  FROM summary_underw as U 
  INNER JOIN  summary_stunt2 as S
  ON U.country = S.country
  INNER JOIN  summary_wast2 as W
  ON S.country = W.country
) as A 

INNER JOIN 
max_year as B
on A.country =B.country")


# ================= Create HTML tables ======================================
# ===========================================================================

#========== Reformat and Rename final_table_1 ==========================
# =======================================================================

final_table1$gdp_pc_ppp = as.integer( final_table1$gdp_pc_ppp)
final_table1$gdp_pc_growth =  round(final_table1$gdp_pc_growth,2)

final_table1 <- final_table1 %>%
  separate(Percentage_Uw, c("Under-Weight %", "95% CI (u)"), " " , extra="merge")

final_table1 <- final_table1 %>%
  separate(Percentage_St, c("Stunted %", "95% CI (s)"), " " , extra="merge")

final_table1 <- final_table1 %>%
  separate(Percentage_Wt, c("Wasted %", "95% CI (w)"), " " , extra="merge")


html1 <- final_table1%>% addHtmlTableStyle(align = "r|r|r|r|r|r|r|r|r|r|r|r|r|r|",
                      spacer.celltype = "double_cell",
                      css.cell = "padding-left: .5em; padding-right: .2em;")%>%
htmlTable(cgroup = c("", "UnderWeight Sample", "Stunting Sample", "Wasting Sample",""),
          n.cgroup = c(2,3,3,3,2),
          caption = "Prevalance of Undernutrition")


#table 1
write.table(final_table1, "final_table1.csv", sep = ",") 


# ====================== Transpose and rename table 2 ==========================
# ==============================================================================

t_summary <- as.data.frame ( t(summary_df) )
rownames(t_summary) <- c('Dietary Diversity Score', 'Breastfeeding Initiation'
                         ,'Full Vaccination','Diarrhea in Previous 2 Weeks'
                         ,'Oral Rehydration Therapy','Indoor Pollution'
                         ,'Household Wealth','Low Maternal BMI','Safe Drinking Water'
                         ,'Improved Sanitation Facility','Antenatal Care')
t_summary$Covariate <- rownames(t_summary)
rownames(t_summary) <- c()

Mean_SD_summary <- t_summary[, c(8,1,2,3,4,5,6,7)] 
colnames(Mean_SD_summary ) <- c("Covariate", "Median", "Mean",
                                              "SE.Mean","CI.Mean.95", "Var",
                                              "SD", "Coef_Var")  
Mean_SD_summary <- Mean_SD_summary %>% mutate_if(is.numeric, round, digits=4)


# table 2
html2 <- Mean_SD_summary %>% addHtmlTableStyle(align = "r|r|r|r|r|r|r|r|r|r|r",
                  spacer.celltype = "double_cell",
                  css.cell = "padding-left: .5em; padding-right: .2em;")%>%
  htmlTable (caption = "Descriptive Statistics for Covariates")







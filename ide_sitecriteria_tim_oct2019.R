library(Rmisc)
library(plyr)
library(tidyr)
library(readr)
setwd("C:\\Users\\ohler\\Dropbox\\IDE MS_Single year extreme\\Data\\")
#read in plot npp summaries
#reduced_npp <- read.csv("anpp_clean_trt_ppt_02-26-2020.csv")
    #clean up the dataframe to useful variables
#reduced_npp <- reduced_npp[,c("site_code", "block", "plot", "subplot", "year", "trt" "mass", "mass_category","note_biomass")]

#read in a file that includes plot-treatment information
full_biomass <- read.csv("anpp_clean_trt_07-20-2020.csv")
    #clean up and reduce to the useful variables
full_biomass <- full_biomass[,c("site_code", "block", "plot", "subplot", "year", "n_treat_days", "trt","mass")]
full_biomass$n_treat_days <- as.numeric(as.character(full_biomass$n_treat_days))

#read in site info df to use PI-reported mean annual precipitation
site_info <- read.csv("Site_Elev-Disturb.csv") #changed from read.csv to read_csv (from package "readr")
site_info$MAP.reported <- site_info$precip
site_info <- site_info[,c('site_code','MAP.reported')]


#####criteria 1. Must accurately calculate ANPP (a measure of annual production)
    #notes in the biomass file tell whether or not the sites are reporting ANPP
#summary(reduced_npp$note_biomass)
#criteria1.flag <- subset(reduced_npp, note_biomass == "site DOES NOT report ANPP and cannot calculate anpp") #Change this to include other items


#####criteria 2.	ratio of [mean ANPP of control plots(all years)/MAP] should be .3 to .9 
    #merge cleaned npp dataset with full biomass survey to get plot-treatment pairings


    #only use plots that are not manipulated
control_biomass <- full_biomass %>% 
  subset( n_treat_days < 30 | trt == "Control")%>%
  ddply( c("site_code", "year", "block", "plot", "subplot"),
         function(x)data.frame(
           biomass = mean(x$mass)
         )) %>%
  ddply( c("site_code", "year", "block", "plot"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code", "year"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  merge(site_info, by = "site_code",all=TRUE)

    #calculate anpp/map ratio
control_biomass$anpp.map.ratio <- control_biomass$biomass/control_biomass$MAP.reported
    #flag sites with ratios outside of the desired range
criteria2.flag <- subset(control_biomass, anpp.map.ratio < .3 | anpp.map.ratio > .9)


#####criteria 3. Is amount of woody biomass different in treatment and control plots. Determined that if criteria 1 and 4 are met, that is sufficient to satisfy criteria 3 issues.


#####criteria 4.	Pre-treatment difference between control and drought treatments (drought treatment outside of the control 95% confidence interval)
    #reduce dataframe to exclusively pretreatment
pretreatment_biomass <- full_biomass %>% 
  subset( n_treat_days < 30& n_treat_days > -300)%>%
  subset(trt == "Control"|trt=="Drought")%>%
  ddply( c("site_code", "block", "plot", "trt"),
         function(x)data.frame(
           mass = mean(x$mass)
         )) %>%
  ddply(c("site_code", "trt"),
        function(x)data.frame(
          mean = mean(x$mass),
          sd = sd(x$mass),
          n = length(x$mass)
        ))

    #calculate upper and lower bounds for confidence intervals
pretreatment_biomass$confint.high <-pretreatment_biomass$mean + qt(0.975,df=pretreatment_biomass$n-1)*pretreatment_biomass$sd/sqrt(pretreatment_biomass$n) 
pretreatment_biomass$confint.low <- pretreatment_biomass$mean - qt(0.975,df=pretreatment_biomass$n-1)*pretreatment_biomass$sd/sqrt(pretreatment_biomass$n)

    #seperate control and treatment plots
pretreatment_biomass_control <- subset(pretreatment_biomass,trt=="Control")
pretreatment_biomass_control$mean.control <- pretreatment_biomass_control$mean
pretreatment_biomass_control <- pretreatment_biomass_control[,c('site_code','mean.control','confint.high','confint.low')]
pretreatment_biomass_drought <- subset(pretreatment_biomass,trt=="Drought")
pretreatment_biomass_drought$mean.drought <- pretreatment_biomass_drought$mean
  pretreatment_biomass_drought <- pretreatment_biomass_drought[,c('site_code','mean.drought')]
    #merge control and treatment dataframes to make comparisons
confint.df <- merge(pretreatment_biomass_control,pretreatment_biomass_drought, by="site_code")

    #flag sites where treatment plot anpp is outside control plot 95% confidence interval
criteria4.flag <- subset(confint.df, (mean.drought < confint.low) == TRUE | (mean.drought > confint.high) == TRUE)


  #merge all of the flagged data
flag.df <- criteria1.flag%>%
  merge(criteria2.flag,by="site_code",all=TRUE)%>%
  merge(criteria4.flag,by="site_code",all=TRUE)

  #generate list of flagged sites
unique(flag.df$site_code)



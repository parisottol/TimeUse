rm(list = ls())

# PACKAGES ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
packages <- c(
  "ipumsr", # Required to load the IPUMS Data extract properly
  "dplyr",
  "tidyverse",
  "data.table",
  "haven",
  "ggplot2",
  "ggpubr",
  "gt",
  "gtExtras",
  "kableExtra",
  "RColorBrewer",
  "MetBrewer",
  "Hmisc",
  "diagis"
)
pacman::p_load(packages, character.only = TRUE)
# -----------------------------------------------------------------------------


# PATHS ----------------------------------------------------------------------- 
if(Sys.info()["user"]=="lucaparisotto1") {
  #maindir <- "/Users/lucaparisotto1/Desktop/Github-projects/TimeUse"
  #datadir <- "/Users/lucaparisotto1/Desktop/Github-projects/TimeUse/Data"
  #outdir <- "/Users/lucaparisotto1/Desktop/Github-projects/TimeUse/Output"
  maindir <- "/Users/lucaparisotto1/Desktop/Dropbox/Projects - My research/USA - Time use"
  datadir <- "/Users/lucaparisotto1/Desktop/Dropbox/Projects - My research/USA - Time use/Data"
  outdir <- "//Users/lucaparisotto1/Desktop/Dropbox/Projects - My research/USA - Time use/Output"
  user    <- "Luca-mac"
} else if(Sys.info()["user"]=="new person") {
  datadir <- "where?"
  user    <- "who?"
}
# -----------------------------------------------------------------------------


# LOAD IPUMS DATA -------------------------------------------------------------
# This has to be done IPUMS style, so setwd() even if I don't like it 
setwd(maindir)
getwd()
# load ddi and main data
ddi <- read_ipums_ddi(file.path(datadir,"Data ATUS/atus_00001.xml"))
df.base <- read_ipums_micro(ddi)
# load the dataset with the constructed time use
ddi <- read_ipums_ddi(file.path(datadir,"Data ATUS/atus_00002.xml"))
df.tu <- read_ipums_micro(ddi)
# -----------------------------------------------------------------------------






# EXPLORE ---------------------------------------------------------------------
## define a function to tabulate by year
tab_freq.year <- function(d,v,ny) { 
  tab <- d %>% 
    select(YEAR,{{v}}) %>% 
    group_by(YEAR,{{v}}) %>% 
    filter({{v}}!=-8,{{v}}!=-9) %>%
    count({{v}}) %>% 
    mutate(
      total=sum(.$n)
    ) %>% 
    spread(
      .,{{v}},n
    ) %>%
    #   rename(., c("Male" = `1`,  "Female" = `2`)) %>%
    select(-total)
  print(tab , n=ny) 
}
## Frequencies by gender of the respondent
# 1 male, 2 female
tab_freq.year(df.base,SEX,27)
## Freq by marital status 
# 01 Married - spouse present
# 02 Married - spouse absent
# 03 Widowed
# 04 Divorced
# 05 Separated
# 06 Never married
tab_freq.year(df.base,MARST,27)
# -----------------------------------------------------------------------------









# CREATE A WORKING DATASET ----------------------------------------------------
## make all var names lower case 
colnames(df.base) <- tolower(colnames(df.base))
colnames(df.tu) <- tolower(colnames(df.tu))

## computer the sampling weight, i.e. merge the 06 and 20 weights (special covid weights)
df.base <- df.base %>%
  mutate(
    wt = ifelse(year==2020,wt20,wt06)
  )

## What vars do we want to keep? 
# technical admin vars
admin <- c(
  "year","caseid","hrhhid_cps8","wt"
)
# geographics
geographics <- c(
  "statefip","metarea"
)
# demographics
demographics <- c(
  "age","sex","marst","educ","educyrs","hh_size"#"hh_numownkids","hh_numkids","ageychild","famincome"
)
# employment 
employment <- c(
  "empstat","occ","occ2","ind","ind2","fullpart","uhrsworkt_cps8","earnweek_cps8","hrsworkt_cps8"
)
# partners' vars
partner <- c(
  "spage","spempstat","spusualhrs","spearnweek"
)
# activities 
activities <- df.base %>% 
  select(starts_with(c("ACT_","SCC_"))) %>%
  colnames(.) 

## select the columns we want for the df, it doesn't like it but sack it
df <- df.base %>% 
  select(
    all_of(admin),
    all_of(geographics),
    all_of(demographics),
    all_of(employment),
    #all_of(activities)
  )

## add in the calculated aggregates from df.tu
aggregates <- df.tu %>%
  filter(rectype==2) %>%
  select(c("caseid","caring_time","working_time","leisuring_time2","choring_time")) %>%
  full_join(.,df,by="caseid")
# drop aggregates to save space
df <- aggregates
rm(aggregates)
# order time use vars to the end, for no reason other than aesthetic 
df <- df %>% relocate(.,
    c("caring_time","working_time","leisuring_time2","choring_time"),
    .after = last_col()
  ) %>% 
  # and rename time vars
  rename(
    care = caring_time,
    work = working_time,
    chores = choring_time,
    leisure = leisuring_time2 
  )

## turn time into hours 
df <- df %>% 
  mutate(across(c(care,work,chores,leisure), ~ .x/60)) 

## create a college educated indicator, MUST HAVE COMPLETED COLLEGE
df$college_n <- ifelse(df$educyrs>=217,1,0) 
df$college_str <- ifelse(df$educyrs>=200,"College","No college") 
df$college <- factor(df$college_str)

## create sex indicator as factor
df$sex_n <- df$sex
df$sex_str <- ifelse(df$sex==1,"Male","Female") 
df$sex <- factor(df$sex_str)

## create the combined college and gender indicator 
df <- df %>%
  mutate(
    group.sex.gender_n = case_when(
      (sex=="Male" & college=="College") ~ 1,
      (sex=="Male" & college=="No college") ~ 2,
      (sex=="Female" & college=="College") ~ 3,
      (sex=="Female" & college=="No college") ~ 4,
      TRUE ~ 0),
    group.sex.gender_str = case_when(
      (sex=="Male" & college=="College") ~ "Male-College",
      (sex=="Male" & college=="No college") ~ "Male-No college",
      (sex=="Female" & college=="College") ~ "Female-College",
      (sex=="Female" & college=="No college") ~ "Female-No college",
      TRUE ~ "None"),
    group.sex.gender = factor(group.sex.gender_str)
  )

## keep only 21-55 year olds
df <- df %>%
  filter(age>20 & age<56)

## clean up some of the continuous variables,
# highest incomes are around 2885, there is one that is 100,000
df$earnweek_cps8 <- ifelse(df$earnweek_cps8 > 5000,NA,df$earnweek_cps8)
# check out
df %>% select(caseid,earnweek_cps8) %>% arrange(.,-earnweek_cps8)
summary(df$earnweek_cps8)
#hist(df$earnweek_cps8)

# highest hours worked are around 130, one outlier at 160 and another at 140...
df$hrsworkt_cps8 <- ifelse(df$hrsworkt_cps8 > 130,NA,df$hrsworkt_cps8)
# check out 
df %>% select(caseid,hrsworkt_cps8) %>% arrange(.,-hrsworkt_cps8)
summary(df$hrsworkt_cps8)
#hist(df$hrsworkt_cps8)

# save a dataset for easier consumption 
write_csv(df,file.path(maindir,"ATUS_analysis.csv"))
# -----------------------------------------------------------------------------











# TIME TRENDS ----------------------------------------------------------------

## Global means over time, 
df %>% 
  group_by(year) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    #list(weighted.mean = mean),
    funs(weighted.mean(., w=wt)),
    na.rm = TRUE
  ) %>%
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>%
  ggplot(aes(x=year, y=time, group=categ, color=categ)) +
    geom_line() +
    ggtitle("Average hours spent per day - global") + 
    ylim(c(0,8)) +
    theme_bw() +
    scale_color_manual(values = met.brewer("Egypt", 4)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.text = element_text(size=12),
          legend.position= "bottom",
          legend.title = element_text(size=12),
          plot.caption = element_text(hjust=0,size=10)) +
    guides(color=guide_legend(title="")) +
    ylab("") 
# save it nicely
ggsave(file.path(outdir,"aggregates_global.png"),
       dpi = 320, scale = 0.8, height = 16, width = 24, units = "cm") 

## Global means over time for MEN
df %>% 
  filter(sex=="Male") %>%
  group_by(year) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    #list(weighted.mean = mean),
    funs(weighted.mean(., w=wt)),
    na.rm = TRUE
  ) %>%
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>%
  ggplot(aes(x=year, y=time, group=categ, color=categ)) +
  geom_line() +
  ggtitle("Average hours spent per day - Men") + 
  ylim(c(0,8)) +
  theme_bw() +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.position= "bottom",
        legend.title = element_text(size=12),
        plot.caption = element_text(hjust=0,size=10)) +
  guides(color=guide_legend(title="")) +
  ylab("") 
# save it nicely
ggsave(file.path(outdir,"aggregates_men.png"),
       dpi = 320, scale = 0.8, height = 16, width = 24, units = "cm") 

## Global means over time for WOMEN
df %>% 
  filter(sex=="Female") %>%
  group_by(year) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    #list(weighted.mean = mean),
    funs(weighted.mean(., w=wt)),
    na.rm = TRUE
  ) %>%
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>%
  ggplot(aes(x=year, y=time, group=categ, color=categ)) +
  geom_line() +
  ggtitle("Average hours spent per day - Women") + 
  ylim(c(0,8)) +
  theme_bw() +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.position= "bottom",
        legend.title = element_text(size=12),
        plot.caption = element_text(hjust=0,size=10)) +
  guides(color=guide_legend(title="")) +
  ylab("") 
# save it nicely
ggsave(file.path(outdir,"aggregates_women.png"),
       dpi = 320, scale = 0.8, height = 16, width = 24, units = "cm") 


## Global means over time, for Men WITH college education
df %>% 
  filter(group.sex.gender=="Male-College") %>%
  group_by(year) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    #list(weighted.mean = mean),
    funs(weighted.mean(., w=wt)),
    na.rm = TRUE
  ) %>%
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>%
  ggplot(aes(x=year, y=time, group=categ, color=categ)) +
  geom_line() +
  ggtitle("Average hours spent per day - Men with college education") + 
  ylim(c(0,8)) +
  theme_bw() +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.position= "bottom",
        legend.title = element_text(size=12),
        plot.caption = element_text(hjust=0,size=10)) +
  guides(color=guide_legend(title="")) +
  ylab("") 
# save it nicely
ggsave(file.path(outdir,"aggregates_men-college.png"),
       dpi = 320, scale = 0.8, height = 16, width = 24, units = "cm") 

## Global means over time, for Men with LESS than college education
df %>% 
  filter(group.sex.gender=="Male-No college") %>%
  group_by(year) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    #list(weighted.mean = mean),
    funs(weighted.mean(., w=wt)),
    na.rm = TRUE
  ) %>%
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>%
  ggplot(aes(x=year, y=time, group=categ, color=categ)) +
  geom_line() +
  ggtitle("Average hours spent per day - Men with less than college education") + 
  ylim(c(0,8)) +
  theme_bw() +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.position= "bottom",
        legend.title = element_text(size=12),
        plot.caption = element_text(hjust=0,size=10)) +
  guides(color=guide_legend(title="")) +
  ylab("") 
# save it nicely
ggsave(file.path(outdir,"aggregates_men-nocollege.png"),
       dpi = 320, scale = 0.8, height = 16, width = 24, units = "cm") 


## Global means over time, for Women WITH college education
df %>% 
  filter(group.sex.gender=="Female-College") %>%
  group_by(year) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    #list(weighted.mean = mean),
    funs(weighted.mean(., w=wt)),
    na.rm = TRUE
  ) %>%
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>%
  ggplot(aes(x=year, y=time, group=categ, color=categ)) +
  geom_line() +
  ggtitle("Average hours spent per day - Women with college education") + 
  ylim(c(0,8)) +
  theme_bw() +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.position= "bottom",
        legend.title = element_text(size=12),
        plot.caption = element_text(hjust=0,size=10)) +
  guides(color=guide_legend(title="")) +
  ylab("") 
# save it nicely
ggsave(file.path(outdir,"aggregates_women-college.png"),
       dpi = 320, scale = 0.8, height = 16, width = 24, units = "cm") 

## Global means over time, for Men with LESS than college education
df %>% 
  filter(group.sex.gender=="Female-No college") %>%
  group_by(year) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    #list(weighted.mean = mean),
    funs(weighted.mean(., w=wt)),
    na.rm = TRUE
  ) %>%
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>%
  ggplot(aes(x=year, y=time, group=categ, color=categ)) +
  geom_line() +
  ggtitle("Average hours spent per day - Women with less than college education") + 
  ylim(c(0,8)) +
  theme_bw() +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.position= "bottom",
        legend.title = element_text(size=12),
        plot.caption = element_text(hjust=0,size=10)) +
  guides(color=guide_legend(title="")) +
  ylab("") 
# save it nicely
ggsave(file.path(outdir,"aggregates_women-nocollege.png"),
       dpi = 320, scale = 0.8, height = 16, width = 24, units = "cm") 

# -----------------------------------------------------------------------------













# AGGREGATE MEANS -------------------------------------------------------------

weights.calc <- function(w) {
  sum((w/sum(w))^2)
}


df %>% 
  group_by(group.sex.gender) %>% 
  summarise_at(
    vars(c(care,work,chores,leisure)),
    funs(wtd.mean(., w=wt,na.rm = TRUE),
         wtd.var(., w=wt,na.rm = TRUE))
  ) 


## plot the means of categories with the groups, over all years
df %>% 
  group_by(group.sex.gender) %>%
  summarise_at(
    vars(c(care,work,chores,leisure)),
    funs(wtd.mean(., w=wt,na.rm = TRUE),
         wtd.var(., w=wt,na.rm = TRUE))
  ) %>%
  mutate(
    across(ends_with(".var"), ~ sqrt(.x*wts))
  ) 

%>% 
  pivot_longer(
    cols = c(care,work,chores,leisure),
    names_to = "categ",
    values_to = "time"
  ) %>% 
  ggplot(aes(x=group.sex.gender, y=time, fill=categ)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Average hours spent per day - 2003-2021") + 
  theme_bw() +
  scale_fill_manual(values = met.brewer("Egypt", 4)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.position= "bottom",
        legend.title = element_text(size=12),
        plot.caption = element_text(hjust=0,size=10)) +
  guides(color=guide_legend(title="")) +
  ylab("") +
  xlab("")







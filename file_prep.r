


library(tidyverse)
library(data.table)
library(knitr)
library(kableExtra)
library(cowplot)

`%!in%` = Negate(`%in%`)

select <- dplyr::select



dyn <- read.csv("D:/cpr/up-dynasties/dyn_29.06.20.csv")



#dyn <- dyn %>% filter(status != "ADD")





# dyn <- dyn %>% rename(relative_id_1 = relative_id)
# 
# 
# 
# dyn <- dyn %>% mutate(relative_id =ifelse(relative_id_1 %in% c(8,9,10,11,12,13,17),10,
#                                            ifelse(relative_id_1 %in% c(15),11,
#                                                   ifelse(relative_id_1==14,8,
#                                                         ifelse(relative_id_1==16,9,
#                                                                ifelse(relative_id_1==18,12, relative_id_1))))))
# relative_names <- tribble(
#   ~relative_id, ~ relative_name,
#   1 , "Patriarch",
#   2 , "Father",
#   3 , "Mother",
#   4 , "Son",
#   5  ,"Daughter",
#   6 , "Brother",
#   7 , "Sister",
#   8 , "Wife" ,
#   9 , "Husband",
#   10 , "Extended",
#   11 , "Outside",
#   12, "Ext outside"
# )
# 
# dyn<- merge(dyn, relative_names, by = "relative_id", all.x =TRUE, allow.cartesian = TRUE)


# table(dyn$relative_id) 
# 
# dyn %>% filter(is.na(relative_id)) %>% select(relative_id)






dyn$rel_id_uniq <- as.numeric(paste(dyn$relative_id, dyn$rel_id_rep, sep = ""))



dyn$term_duration[dyn$election_type %in% c("NN", "NPP", "NP", "ZP", "BP")]  <- ""

dyn$term_duration <- as.numeric(dyn$term_duration)




## family_expericne



dyn <- dyn%>%
  group_by(family_id) %>%
  mutate(fam_exp_tot = sum(term_duration[year< 2017], na.rm = TRUE)) %>%
  ungroup()

year_group <- dyn %>%
  group_by(family_id, year) %>%
  summarise(year_sum=sum(term_duration[year < 2017], na.rm = TRUE)) %>%
  mutate(cum_exp = cumsum(year_sum)) %>%
  mutate(fam_exp_cum = cum_exp - year_sum) %>%
  select(family_id,year,cum_exp, fam_exp_cum, year_sum)

year_group <- year_group %>%
  select(family_id, year, fam_exp_cum)

dyn<- merge(dyn, year_group, by = c("family_id", "year"),all.x = TRUE, allow.cartesian = TRUE)

dyn$fam_exp_tot <-   as.numeric(dyn$fam_exp_tot)

dyn$fam_exp_cum <-   as.numeric(dyn$fam_exp_cum)

###

##fam expereince categoreis


dyn<- dyn %>%  mutate(fam_exp_tot_cat = cut(fam_exp_tot,c(0,1,6,11,21,Inf),include.lowest = TRUE, right =FALSE))

dyn<- dyn %>%  mutate(fam_exp_cum_cat = cut(fam_exp_cum,c(0,1,6,11,21,Inf),include.lowest = TRUE, right =FALSE))



##caste_category

####

caste_names <- tribble(
  ~caste_id, ~ caste_name,
  0 , "unspecified",
  1 , "Brahmin",
  2 , "Thakur/ Rajput",
  3 , "Baniya",
  4 , "Other upper caste",
  5  ,"Jat",
  6 , "Gujjar",
  7 , "Yadav",
  8 , "Kurmi" ,
  9 , "Lodh",
  10 , "Other OBC",
  11 , "Dalit",
  12 , "Muslim",
  13  , "Others"
)

dyn<- merge(dyn, caste_names, by = "caste_id", all.x =TRUE)

##caste distribution

dyn$caste_groups <- NA


dyn$caste_groups <- ifelse(dyn$caste_id %in% c(7,12,11),dyn$caste_name,dyn$caste_groups)

dyn$caste_groups <- ifelse(dyn$caste_id %in% c(1,2,4,3),"Upper Caste",dyn$caste_groups)


dyn$caste_groups <- ifelse(dyn$caste_id %in% c(5,6,9,8,10),"Non-Yadav OBC",dyn$caste_groups)


dyn$caste_groups <- ifelse(dyn$caste_id %in% c(0,13),"Others",dyn$caste_groups)


## dyn classification

#dyn$dyn_cum  <-ifelse(dyn$fam_size_cum>1,1,0)

dyn <- dyn %>% group_by(family_id) %>% mutate(max_year = max(year))

#dyn %>% filter(family_id== "f0009806") %>% select(max_year)

#dyn <- dyn %>% group_by(family_id) %>%  mutate(dyn_tot = max(dyn_cum))

#dyn$dyn_tot <- ifelse(dyn$fam_size_tot>1,1,0)
# 
# ##caste_group dist

### Levels

dyn <- dyn %>% mutate(election_level =case_when(election_type == "AE" ~ "AE", 
                                                election_type == "GE" ~ "GE",
                                                TRUE ~ "LB")) 

dyn_ae_lvl <- dyn %>% filter(election_level == "AE") %>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)


dyn_ge_lvl <- dyn %>% filter(election_level == "GE")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)



dyn_lb_lvl <- dyn %>% filter(election_level == "LB")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)

dyn_ae_ge_lvl <- merge(dyn_ge_lvl, dyn_ae_lvl, by = "family_id" , all =TRUE)

dyn_all_lvl <- merge(dyn_ae_ge_lvl ,dyn_lb_lvl,by = "family_id" , all =TRUE )

dyn_all_lvl$levels <- paste(dyn_all_lvl$election_level.x,dyn_all_lvl$election_level.y, dyn_all_lvl$election_level, sep = "")

dyn_all_lvl <- dyn_all_lvl %>% mutate(level =case_when(
  levels == "GEAELB" ~ "GE-AE-LB",
  levels == "GEAENA" ~ "GE-AE",
  levels == "GENALB" ~ "GE-LB",
  levels == "GENANA" ~ "GE",
  levels == "NAAELB" ~ "AE-LB",
  levels == "NAAENA" ~ "AE",
  levels == "NANALB" ~ "LB",
))


dyn_all_lvl <- dyn_all_lvl %>% select(family_id, level)



dyn <- merge(dyn, dyn_all_lvl, by = "family_id", all.x = TRUE, allow.cartesian = TRUE)

# 


#



dyn_all <- dyn

##2019

dyn_pre19 <- dyn_all %>% filter(year < 2020)

# dyn <- dyn %>% mutate(election_level =case_when(election_type == "AE" ~ "AE", 
#                         election_type == "GE" ~ "GE",
#                          TRUE ~ "LB")) 
# 
dyn_pre19_ae_lvl <- dyn_pre19 %>% filter(election_level == "AE") %>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
dyn_pre19_ge_lvl <- dyn_pre19 %>% filter(election_level == "GE")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
# 
dyn_pre19_lb_lvl <- dyn_pre19 %>% filter(election_level == "LB")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
dyn_pre19_ae_ge_lvl <- merge(dyn_pre19_ge_lvl, dyn_pre19_ae_lvl, by = "family_id" , all =TRUE)
# 
dyn_pre19_all_lvl <- merge(dyn_pre19_ae_ge_lvl ,dyn_pre19_lb_lvl,by = "family_id" , all =TRUE )
# 
dyn_pre19_all_lvl$levels <- paste(dyn_pre19_all_lvl $election_level.x,dyn_pre19_all_lvl $election_level.y, dyn_pre19_all_lvl $election_level, sep = "")
# 
dyn_pre19_all_lvl <- dyn_pre19_all_lvl %>% mutate(level_19 =case_when(
  levels == "GEAELB" ~ "GE-AE-LB",
  levels == "GEAENA" ~ "GE-AE",
  levels == "GENALB" ~ "GE-LB",
  levels == "GENANA" ~ "GE",
  levels == "NAAELB" ~ "AE-LB",
  levels == "NAAENA" ~ "AE",
  levels == "NANALB" ~ "LB",
))
# 
# 
dyn_pre19_all_lvl <- dyn_pre19_all_lvl %>% select(family_id, level_19)
dyn_pre19_all_lvl$year <- as.numeric(2019)
# 
# 

##2014

dyn_pre14 <- dyn_all %>% filter(year < 2015)

# dyn <- dyn %>% mutate(election_level =case_when(election_type == "AE" ~ "AE", 
#                         election_type == "GE" ~ "GE",
#                          TRUE ~ "LB")) 
# 
dyn_pre14_ae_lvl <- dyn_pre14 %>% filter(election_level == "AE") %>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
dyn_pre14_ge_lvl <- dyn_pre14 %>% filter(election_level == "GE")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
# 
dyn_pre14_lb_lvl <- dyn_pre14 %>% filter(election_level == "LB")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
dyn_pre14_ae_ge_lvl <- merge(dyn_pre14_ge_lvl, dyn_pre14_ae_lvl, by = "family_id" , all =TRUE)
# 
dyn_pre14_all_lvl <- merge(dyn_pre14_ae_ge_lvl ,dyn_pre14_lb_lvl,by = "family_id" , all =TRUE )
# 
dyn_pre14_all_lvl$levels <- paste(dyn_pre14_all_lvl $election_level.x,dyn_pre14_all_lvl $election_level.y, dyn_pre14_all_lvl $election_level, sep = "")
# 
dyn_pre14_all_lvl <- dyn_pre14_all_lvl %>% mutate(level_14 =case_when(
  levels == "GEAELB" ~ "GE-AE-LB",
  levels == "GEAENA" ~ "GE-AE",
  levels == "GENALB" ~ "GE-LB",
  levels == "GENANA" ~ "GE",
  levels == "NAAELB" ~ "AE-LB",
  levels == "NAAENA" ~ "AE",
  levels == "NANALB" ~ "LB",
))
# 
# 
dyn_pre14_all_lvl <- dyn_pre14_all_lvl %>% select(family_id, level_14)
dyn_pre14_all_lvl$year <- as.numeric(2014)







##2009

dyn_pre09 <- dyn_all %>% filter(year < 2010)

# dyn <- dyn %>% mutate(election_level =case_when(election_type == "AE" ~ "AE", 
#                         election_type == "GE" ~ "GE",
#                          TRUE ~ "LB")) 
# 
dyn_pre09_ae_lvl <- dyn_pre09 %>% filter(election_level == "AE") %>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
dyn_pre09_ge_lvl <- dyn_pre09 %>% filter(election_level == "GE")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
# 
dyn_pre09_lb_lvl <- dyn_pre09 %>% filter(election_level == "LB")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
dyn_pre09_ae_ge_lvl <- merge(dyn_pre09_ge_lvl, dyn_pre09_ae_lvl, by = "family_id" , all =TRUE)
# 
dyn_pre09_all_lvl <- merge(dyn_pre09_ae_ge_lvl ,dyn_pre09_lb_lvl,by = "family_id" , all =TRUE )
# 
dyn_pre09_all_lvl$levels <- paste(dyn_pre09_all_lvl $election_level.x,dyn_pre09_all_lvl $election_level.y, dyn_pre09_all_lvl $election_level, sep = "")
# 
dyn_pre09_all_lvl <- dyn_pre09_all_lvl %>% mutate(level_09 =case_when(
  levels == "GEAELB" ~ "GE-AE-LB",
  levels == "GEAENA" ~ "GE-AE",
  levels == "GENALB" ~ "GE-LB",
  levels == "GENANA" ~ "GE",
  levels == "NAAELB" ~ "AE-LB",
  levels == "NAAENA" ~ "AE",
  levels == "NANALB" ~ "LB",
))
# 
# 
dyn_pre09_all_lvl <- dyn_pre09_all_lvl %>% select(family_id, level_09)
dyn_pre09_all_lvl$year <- as.numeric(2009)
# 
# 

##2004

dyn_pre04 <- dyn_all %>% filter(year < 2005)

# dyn <- dyn %>% mutate(election_level =case_when(election_type == "AE" ~ "AE", 
#                         election_type == "GE" ~ "GE",
#                          TRUE ~ "LB")) 
# 
dyn_pre04_ae_lvl <- dyn_pre04 %>% filter(election_level == "AE") %>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
dyn_pre04_ge_lvl <- dyn_pre04 %>% filter(election_level == "GE")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
# 
dyn_pre04_lb_lvl <- dyn_pre04 %>% filter(election_level == "LB")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
dyn_pre04_ae_ge_lvl <- merge(dyn_pre04_ge_lvl, dyn_pre04_ae_lvl, by = "family_id" , all =TRUE)
# 
dyn_pre04_all_lvl <- merge(dyn_pre04_ae_ge_lvl ,dyn_pre04_lb_lvl,by = "family_id" , all =TRUE )
# 
dyn_pre04_all_lvl$levels <- paste(dyn_pre04_all_lvl $election_level.x,dyn_pre04_all_lvl $election_level.y, dyn_pre04_all_lvl $election_level, sep = "")
# 
dyn_pre04_all_lvl <- dyn_pre04_all_lvl %>% mutate(level_04 =case_when(
  levels == "GEAELB" ~ "GE-AE-LB",
  levels == "GEAENA" ~ "GE-AE",
  levels == "GENALB" ~ "GE-LB",
  levels == "GENANA" ~ "GE",
  levels == "NAAELB" ~ "AE-LB",
  levels == "NAAENA" ~ "AE",
  levels == "NANALB" ~ "LB",
))
# 
# 
dyn_pre04_all_lvl <- dyn_pre04_all_lvl %>% select(family_id, level_04)
dyn_pre04_all_lvl$year <- as.numeric(2004)




# 
# 
# dyn <- merge(dyn, dyn_all_lvl, by = "family_id", all.x = TRUE, allow.cartesian = TRUE)
# 
# dyn_fam_uniq_last_ind <- dyn %>% filter(dyn_tot >0) %>% group_by(family_id) %>% arrange(-year) %>% distinct(family_id, .keep_all = TRUE)
# 
# #dyn_fam_uniq_last_ind %>% group_by(level) %>%  summarise(count = n()) %>% arrange(-count) %>% kable()

##2012

dyn_pre12 <- dyn_all %>% filter(year < 2013)

# dyn <- dyn %>% mutate(election_level =case_when(election_type == "AE" ~ "AE", 
#                         election_type == "GE" ~ "GE",
#                          TRUE ~ "LB")) 
# 
dyn_pre12_ae_lvl <- dyn_pre12 %>% filter(election_level == "AE") %>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
dyn_pre12_ge_lvl <- dyn_pre12 %>% filter(election_level == "GE")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
# 
dyn_pre12_lb_lvl <- dyn_pre12 %>% filter(election_level == "LB")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
dyn_pre12_ae_ge_lvl <- merge(dyn_pre12_ge_lvl, dyn_pre12_ae_lvl, by = "family_id" , all =TRUE)
# 
dyn_pre12_all_lvl <- merge(dyn_pre12_ae_ge_lvl ,dyn_pre12_lb_lvl,by = "family_id" , all =TRUE )
# 
dyn_pre12_all_lvl$levels <- paste(dyn_pre12_all_lvl $election_level.x,dyn_pre12_all_lvl $election_level.y, dyn_pre12_all_lvl $election_level, sep = "")
# 
dyn_pre12_all_lvl <- dyn_pre12_all_lvl %>% mutate(level_12 =case_when(
  levels == "GEAELB" ~ "GE-AE-LB",
  levels == "GEAENA" ~ "GE-AE",
  levels == "GENALB" ~ "GE-LB",
  levels == "GENANA" ~ "GE",
  levels == "NAAELB" ~ "AE-LB",
  levels == "NAAENA" ~ "AE",
  levels == "NANALB" ~ "LB",
))
# 
# 
dyn_pre12_all_lvl <- dyn_pre12_all_lvl %>% select(family_id, level_12)
dyn_pre12_all_lvl$year <- as.numeric(2012)
# 
# 

##2017

dyn_pre17 <- dyn_all %>% filter(year < 2018)

# dyn <- dyn %>% mutate(election_level =case_when(election_type == "AE" ~ "AE", 
#                         election_type == "GE" ~ "GE",
#                          TRUE ~ "LB")) 
# 
dyn_pre17_ae_lvl <- dyn_pre17 %>% filter(election_level == "AE") %>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
dyn_pre17_ge_lvl <- dyn_pre17 %>% filter(election_level == "GE")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
# 
# 
dyn_pre17_lb_lvl <- dyn_pre17 %>% filter(election_level == "LB")%>% distinct(family_id, .keep_all = TRUE) %>% select(election_level, family_id)
# 
dyn_pre17_ae_ge_lvl <- merge(dyn_pre17_ge_lvl, dyn_pre17_ae_lvl, by = "family_id" , all =TRUE)
# 
dyn_pre17_all_lvl <- merge(dyn_pre17_ae_ge_lvl ,dyn_pre17_lb_lvl,by = "family_id" , all =TRUE )
# 
dyn_pre17_all_lvl$levels <- paste(dyn_pre17_all_lvl $election_level.x,dyn_pre17_all_lvl $election_level.y, dyn_pre17_all_lvl $election_level, sep = "")
# 
dyn_pre17_all_lvl <- dyn_pre17_all_lvl %>% mutate(level_17 =case_when(
  levels == "GEAELB" ~ "GE-AE-LB",
  levels == "GEAENA" ~ "GE-AE",
  levels == "GENALB" ~ "GE-LB",
  levels == "GENANA" ~ "GE",
  levels == "NAAELB" ~ "AE-LB",
  levels == "NAAENA" ~ "AE",
  levels == "NANALB" ~ "LB",
))
# 
# 
dyn_pre17_all_lvl <- dyn_pre17_all_lvl %>% select(family_id, level_17)
dyn_pre17_all_lvl$year <- as.numeric(2017)



dyn_pre04_09_14_19_12_17 <- rbindlist(list(data.frame(dyn_pre04_all_lvl) , data.frame(dyn_pre09_all_lvl),data.frame(dyn_pre14_all_lvl) , data.frame(dyn_pre19_all_lvl),data.frame(dyn_pre12_all_lvl) , data.frame(dyn_pre17_all_lvl)), use.names = FALSE)

names(dyn_pre04_09_14_19_12_17)[2] <- "level_cum"

dyn <- merge(dyn,dyn_pre04_09_14_19_12_17 , by =c ("year", "family_id"), all.x = TRUE)

dyn<- dyn %>% mutate(level_log_cum= ifelse(level_cum %in% c("AE", "GE"), FALSE, TRUE))

dyn$term_duration[dyn$election_type %in% c("NN", "NPP", "NP") & dyn$year  %in% c(1995,2006)] <- 6

dyn$term_duration[dyn$election_type %in% c("NN", "NPP", "NP")  & dyn$year %in% c(2001, 2012, 2017)]<- 5

##

dyn$term_duration[dyn$election_type %in% c("ZP", "BP")  & dyn$year %in% c(1995, 2000, 2005, 2010, 2015)]<- 5

dyn_ae_ge <- dyn %>% filter(election_type %in% c("AE", "GE"))

dyn_tot <- dyn_ae_ge %>% group_by(family_id) %>%  mutate(dyn_tot = max(dyn_cum)) %>% select(year, election_type,constituency_no,position, family_id,dyn_tot)

dyn <- merge(dyn, dyn_tot, by = c("year", "election_type","constituency_no","position", "family_id"), all.x =TRUE)

dyn_ae_ge <- dyn %>% filter(election_type %in% c("AE", "GE"))

write.csv(dyn, "D:/cpr/up-dynasties/dyn_other_data/dyn_all.csv")

write.csv(dyn_ae_ge, "D:/cpr/up-dynasties/dyn_other_data/dyn_ae_ge.csv")




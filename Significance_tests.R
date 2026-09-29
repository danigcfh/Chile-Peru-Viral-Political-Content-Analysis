library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)


judg <- readRDS("judg_summary_combined.rds")

df <- readRDS("all_obs.rds")%>% 
  filter(actor_type!="cuenta sospechosa/bot")%>% 
  filter(sentiment!="surprise")%>% 
  filter(sentiment!="na")
df_ciud <- df%>% filter(actor_type=="ciudadano")
df_pol <- df%>% filter(actor_type=="politico")
df_inf <- df%>% filter(actor_type=="influencer")
df_med <- df%>% filter(actor_type=="medio")


ch_df <- readRDS("ch_df.rds") 
pr_df <- readRDS("pr_df.rds") 

judg_ch <- readRDS("judg_combined.rds") %>% 
  filter(country== "Chile") %>%
  filter(actor_type != "cuenta sospechosa/bot")
judg_pr <- readRDS("judg_combined.rds") %>% 
  filter(country== "Perú")%>%
  filter(actor_type != "cuenta sospechosa/bot")

##############
#general descriptive stats
##############

mean(ch_df$Stakes[ch_df$actor_type == "medio"])
mean(ch_df$Intensidad[ch_df$actor_type == "medio"])

mean(pr_df$Intensidad[pr_df$actor_type == "medio"], na.rm=TRUE)
mean(pr_df$Stakes[pr_df$actor_type == "medio"], na.rm=TRUE)

mean(ch_df$Intensidad[ch_df$actor_type == "influencer"])
mean(ch_df$Stakes[ch_df$actor_type == "influencer"])

chile <- pr_df$Stakes[pr_df$actor_type == "medio"]
peru <- ch_df$Stakes[ch_df$actor_type == "medio"]
t.test(chile, peru )

table(ch_df$Denuncia[ch_df$actor_type != "politico"])
table(pr_df$Denuncia[pr_df$actor_type != "politico"])

median(ch_df$views_per_capita)
ineq(ch_df$views_per_capita)

median(pr_df$views_per_capita)
ineq(pr_df$views_per_capita)

median(ch_df$comments_per_capita)/median(ch_df$reactions_per_capita)
median(pr_df$comments_per_capita)/median(pr_df$reactions_per_capita)

median(ch_df$comments_per_capita)
median(pr_df$comments_per_capita)

##############
#Identify differences in judgement of candidates
##############


appreciation_cols <- c("judg_izq",  "judg_der")

app_judg_ch <- judg_ch  %>% filter(column %in% appreciation_cols) 
app_judg_pr <- judg_pr  %>% filter(column %in% appreciation_cols) 

candidate_data_ch <- as.data.frame(table(app_judg_ch$valence, app_judg_ch$actor_type))
candidate_data_pr <- as.data.frame(table(app_judg_pr$valence, app_judg_pr$actor_type))

names(candidate_data_ch) <- c("valence", "actor_type", "count")
names(candidate_data_pr) <- c("valence", "actor_type", "count")

candidate_data_pr$country <- "Peru"
candidate_data_ch$country <- "Chile"
candidate_data <- bind_rows(candidate_data_pr, candidate_data_ch)

#confirm if there's significant difference in overall judgement

cont_table_candidate <- candidate_data %>% 
  group_by(country, valence) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  xtabs(count ~ country + valence, data = .)

gtest_candidate <- GTest(cont_table_candidate, correct = "williams")
print(gtest_candidate)

#There is no difference in overall candidate evaluation

#confirm if there's significant difference in posts by ciudadanos

ciudadanos_candidatos <- candidate_data %>%
  filter(actor_type == "ciudadano")

cont_table_ciud_candidatos <- xtabs(count ~ country + valence, data = ciudadanos_candidatos)
gtest_ciud_candidatos <- GTest(cont_table_ciud_candidatos, correct = "williams")
print(gtest_ciud_candidatos)
#There is no difference in  candidate evaluation by citizens

#confirm if there's significant difference in posts by politicos

politico <- candidate_data %>%
  filter(actor_type == "politico")

cont_table_pol_candidatos <- xtabs(count ~ country + valence, data = politico)
gtest_politico_candidatos <- GTest(cont_table_pol_candidatos, correct = "williams")
print(gtest_politico_candidatos)
#There is  difference in  candidate evaluation by politicians

##############
#Identify differences in overall post valence 
##############

event_col <- c("judg_ev")
valence_col <- c("pos_actor")

#identify overall valence

val_judg_ch <- judg_ch  %>% filter(column %in% valence_col) 
val_judg_pr <- judg_pr  %>% filter(column %in% valence_col) 

valence_data_pr <- as.data.frame(table(val_judg_pr$valence, val_judg_pr$actor_type))
valence_data_ch <- as.data.frame(table(val_judg_ch$valence, val_judg_ch$actor_type))

names(valence_data_pr) <- c("valence", "actor_type", "count")
names(valence_data_ch) <- c("valence", "actor_type", "count")

valence_data_pr$country <- "Peru"
valence_data_ch$country <- "Chile"
valence_data <- bind_rows(valence_data_pr, valence_data_ch)

#confirm if there's significant difference in overall valence of posts

cont_table_valence <- valence_data %>% 
  group_by(country, valence) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  xtabs(count ~ country + valence, data = .)

gtest_valence <- GTest(cont_table_valence, correct = "williams")
print(gtest_valence)

#There is difference in overall valence

#confirm if there's significant difference in valence in posts by ciudadanos

ciudadanos <- valence_data %>%
  filter(actor_type == "ciudadano")

cont_table_ciud <- xtabs(count ~ country + valence, data = ciudadanos)
gtest_ciud <- GTest(cont_table_ciud, correct = "williams")
print(gtest_ciud)

#There is  difference in overall valence by citizens

#confirm if there's significant difference in valence in posts by politicos

politico <- valence_data %>%
  filter(actor_type == "politico")

cont_table_pol <- xtabs(count ~ country + valence, data = politico)
gtest_politico <- GTest(cont_table_pol, correct = "williams")
print(gtest_politico)

#There is  difference in overall valence by citizens


##############
#test for differences in sentiment distribution 
##############


#test overall sentiment distribution 
GTest(table(df$country, df$sentiment), correct = "williams")

#no difference in overall sentiment distribution 

#test sentiment distribution per actor type
GTest(table(df_ciud$sentiment, df_ciud$country))
#no difference in sentiment distribution for citizens

GTest(table(df_pol$sentiment, df_pol$country))
#no difference in sentiment distribution for politicians

GTest(table(df_inf$sentiment, df_inf$country))
#no difference in sentiment distribution for influencers

GTest(table(df_med$sentiment, df_med$country))
#no difference in sentiment distribution for medios

##############
#test for differences in intensity  
##############

#test overall intensity difference
t.test(df$Intensidad[df$country == "Chile"], df$Intensidad[df$country == "Perú"])
#no overall intensity difference

#test intensity difference per actor type
t.test(df_ciud$Intensidad[df_ciud$country == "Chile"], df_ciud$Intensidad[df_ciud$country == "Perú"])
#no  intensity difference for citizens


t.test(df_pol$Intensidad[df_pol$country == "Chile"], df_pol$Intensidad[df_pol$country == "Perú"])
#  intensity difference for politicians ***


t.test(df_inf$Intensidad[df_inf$country == "Chile"], df_inf$Intensidad[df_inf$country == "Perú"])
#no  intensity difference for influencers


t.test(df_med$Intensidad[df_med$country == "Chile"], df_med$Intensidad[df_med$country == "Perú"])

#no  intensity difference for medios




##############
#test for differences in stakes  
##############

#test overall stake difference
t.test(df$Stakes[df$country == "Chile"], df$Stakes[df$country == "Perú"])
#no overall stake difference

#test stake difference per actor type
t.test(df_ciud$Stakes[df_ciud$country == "Chile"], df_ciud$Stakes[df_ciud$country == "Perú"])
#no  stake difference for citizens


t.test(df_pol$Stakes[df_pol$country == "Chile"], df_pol$Stakes[df_pol$country == "Perú"])
#  no stake difference for politicians 


t.test(df_inf$Stakes[df_inf$country == "Chile"], df_inf$Stakes[df_inf$country == "Perú"])
#no  stake difference for influencers


t.test(df_med$Stakes[df_med$country == "Chile"], df_med$Stakes[df_med$country == "Perú"])

#  intensity difference for medios *



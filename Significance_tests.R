library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)


#Load data

judg <- readRDS("judg_summary_combined.rds")

df <- readRDS("all_obs.rds")%>% 
  filter(actor_type!="cuenta sospechosa/bot")%>% 
  filter(sentiment!="surprise")%>% 
  filter(sentiment!="na")
df_ciud <- df%>% filter(actor_type=="ciudadano")
df_pol <- df%>% filter(actor_type=="politico")
df_inf <- df%>% filter(actor_type=="influencer")
df_med <- df%>% filter(actor_type=="medio")


ch_df <- df %>% 
  filter(country== "Chile")
pr_df <- df %>% 
  filter(country== "Perú")

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


#citizens

ciudadanos_candidatos <- candidate_data %>%
  filter(actor_type == "ciudadano")

cont_table_ciud_candidatos <- xtabs(count ~ country + valence, data = ciudadanos_candidatos)

#politicos

politico <- candidate_data %>%
  filter(actor_type == "politico")

cont_table_pol_candidatos <- xtabs(count ~ country + valence, data = politico)

#influencer

influencer <- candidate_data %>%
  filter(actor_type == "influencer")

cont_table_inf_candidatos <- xtabs(count ~ country + valence, data = influencer)


#medios
medio <- candidate_data %>%
  filter(actor_type == "medio")

cont_table_med_candidatos <- xtabs(count ~ country + valence, data = medio)


#Using gtests because categorical variables
candidatos_tests <- list(
  overall =  GTest(cont_table_candidate, correct = "williams"),
  ciudadano = GTest(cont_table_ciud_candidatos, correct = "williams"),
  politico =GTest(cont_table_pol_candidatos, correct = "williams"),
  influencer = GTest(cont_table_inf_candidatos, correct = "williams"),
  medio = GTest(cont_table_med_candidatos, correct = "williams")
)

candidatos_p <- sapply(
  candidatos_tests,
  function(x) x$p.value
)

#adjusting to limit false discovery rate
candidato_results <- data.frame(
  comparison = names(candidatos_p),
  p_raw = as.numeric(candidatos_p),
  p.adjust = p.adjust(
    candidatos_p,
    method = "BH"
  )
)
candidato_results


#almost a difference for politicians, but the adjust kicks it out



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

#overall valence of posts

cont_table_valence <- valence_data %>% 
  group_by(country, valence) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  xtabs(count ~ country + valence, data = .)

#citizens 

ciudadanos <- valence_data %>%
  filter(actor_type == "ciudadano")

cont_table_ciud <- xtabs(count ~ country + valence, data = ciudadanos)

#politicians

politico <- valence_data %>%
  filter(actor_type == "politico")

cont_table_pol <- xtabs(count ~ country + valence, data = politico)

#medios

medio <- valence_data %>%
  filter(actor_type == "medio")

cont_table_med <- xtabs(count ~ country + valence, data = medio)

#influencers

influencer <- valence_data %>%
  filter(actor_type == "influencer")

cont_table_inf <- xtabs(count ~ country + valence, data = influencer)

#Using gtests because categorical variables

valence_tests <- list(
  overall = GTest(cont_table_valence, correct = "williams"),
  ciudadano = GTest(cont_table_ciud, correct = "williams"),
  politico =GTest(cont_table_pol, correct = "williams"),
  influencer = GTest(cont_table_inf, correct = "williams"),
  medio = GTest(cont_table_med, correct = "williams")
)

valence_p <- sapply(
  valence_tests,
  function(x) x$p.value
)

valence_results <- data.frame(
  comparison = names(valence_p),
  p_raw = as.numeric(valence_p),
  p.adjust = p.adjust(
    valence_p,
    method = "BH"
  )
)
valence_results

#significant for overall, citizen and politician, influencer is borderline


##############
#test for differences in sentiment distribution 
##############

#Using gtests because categorical variables

sentiment_tests <- list(
  overall = GTest(table(df$country, df$sentiment), correct = "williams"),
  ciudadano = GTest(table(df_ciud$sentiment, df_ciud$country), correct = "williams"),
  politico =GTest(table(df_pol$sentiment, df_pol$country), correct = "williams"),
  influencer = GTest(table(df_inf$sentiment, df_inf$country), correct = "williams"),
  medio = GTest(table(df_med$sentiment, df_med$country), correct = "williams")
)

sentiment_p <- sapply(
  sentiment_tests,
  function(x) x$p.value
)

sentiment_results <- data.frame(
  comparison = names(sentiment_p),
  p_raw = as.numeric(sentiment_p),
  p.adjust = p.adjust(
    sentiment_p,
    method = "BH"
  )
)
sentiment_results

#no difference in sentiment distribution for anyone, 
#at least nothing statistically significant, too few obs per cell

##############
#test for differences in intensity  
##############
#Using wilcox because ordinal variables

intensity_tests <- list(
  overall = wilcox.test(
    Intensidad ~ country,
    data = df,
    exact = FALSE
  ),
  ciudadano = wilcox.test(
    Intensidad ~ country,
    data = df_ciud,
    exact = FALSE
  ),
  politico =wilcox.test(
    Intensidad ~ country,
    data = df_pol,
    exact = FALSE
  ),
  influencer = wilcox.test(
    Intensidad ~ country,
    data = df_inf,
    exact = FALSE
  ),
  medio = wilcox.test(
    Intensidad ~ country,
    data = df_med,
    exact = FALSE
  )
)

intensity_p <- sapply(
  intensity_tests,
  function(x) x$p.value
)

intensity_results <- data.frame(
  comparison = names(intensity_p),
  p_raw = as.numeric(intensity_p),
  p.adjust = p.adjust(
    intensity_p,
    method = "BH"
  )
)

intensity_results

# only intensity difference for politico




##############
#test for differences in stakes  
##############


#Using wilcox because ordinal variables
stakes_tests <- list(
  overall = wilcox.test(
    Stakes ~ country,
    data = df,
    exact = FALSE
  ),
  ciudadano = wilcox.test(
    Stakes ~ country,
    data = df_ciud,
    exact = FALSE
  ),
  politico =wilcox.test(
    Stakes ~ country,
    data = df_pol,
    exact = FALSE
  ),
  influencer = wilcox.test(
    Stakes ~ country,
    data = df_inf,
    exact = FALSE
  ),
  medio = wilcox.test(
    Stakes ~ country,
    data = df_med,
    exact = FALSE
  )
)

Stakes_p <- sapply(
  stakes_tests,
  function(x) x$p.value
)

Stakes_results <- data.frame(
  comparison = names(Stakes_p),
  p_raw = as.numeric(Stakes_p),
  p_adjust = p.adjust(
    Stakes_p,
    method = "BH"
  )
)

Stakes_results

#  No difference for stakes



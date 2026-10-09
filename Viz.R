library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)

df <- readRDS("all_obs.rds")
ch_df <- df %>% 
  filter(country== "Chile")
pr_df <- df %>% 
  filter(country== "Perú")

judg <- readRDS("judg_summary_combined.rds")


judg_ch <- readRDS("judg_combined.rds") %>% 
  filter(country== "Chile") %>% filter(actor_type!= "cuenta sospechosa/bot")
judg_pr <- readRDS("judg_combined.rds") %>% 
  filter(country== "Perú") %>% filter(actor_type!= "cuenta sospechosa/bot")




################
#sentiment per country per actor type
################

ch_sent <- as.data.frame(table(ch_df$sentiment, ch_df$actor_type))  %>% mutate(country = "Chile")
pr_sent <- as.data.frame(table(pr_df$sentiment, pr_df$actor_type))  %>% mutate(country = "Perú")

sent <- rbind(ch_sent,pr_sent)%>%
  filter(Freq > 0) %>% filter( Var1 != "na") %>%
  filter( Var2 != "cuenta sospechosa/bot") %>%
  group_by(country, Var2) %>%
  mutate(
    total = sum(Freq),
    pct = Freq / total * 100
  ) %>%
  ungroup()


ggplot(sent, aes(x = Var2, y = pct, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack")+
  scale_x_discrete(labels = c(
    "ciudadano" = "Citoyen",
    "influencer" = "Influenceur",
    "medio" = "Média",
    "politico" = "Politicien"))+
  geom_text(aes(label = paste0(round(pct, 0), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 2.5, color = "white") +
  facet_wrap(~ country, scales = "fixed", ncol = 1) +
  theme(
    panel.spacing = unit(2, "cm")
  )+
  scale_fill_manual(
    values = c(
      "anger" = "#d7191c",
      "anticipation" = "#fdae61",
      "disgust" = "#7c3a3a",
      "fear" = "#5e3c99",
      "joy" = "#2b83ba",
      "na" = "#bdbdbd",
      "neutro" = "#f0f0f0",
      "trust" = "#1a9641"
    ),
    labels = c(
      "anger"        = "Colère",
      "anticipation" = "Anticipation",
      "disgust"      = "Dégoût",
      "fear"         = "Peur",
      "joy"          = "Joie",
      "neutro"       = "Neutre",
      "trust"        = "Confiance"
    )
  ) +
  labs(
    title = "Distribution des sentiments par type d'acteur",
    subtitle = "Comparaison par pays",
    x = "Type d'acteur",
    y = "Pourcentage (%)",
    fill = "Sentiment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    panel.spacing.y = unit(2, "lines")
  )

################
#Valence per country per actor type
################
appreciation_cols <- c("judg_izq",  "judg_der")
event_col <- c("judg_ev")
valence_col <- c("pos_actor")

#identify judgement of candidates

app_judg_ch <- judg_ch  %>% filter(column %in% appreciation_cols) 
app_judg_pr <- judg_pr  %>% filter(column %in% appreciation_cols) 


#identify overal post valence

val_judg_ch <- judg_ch  %>% filter(column %in% valence_col) 
val_judg_pr <- judg_pr  %>% filter(column %in% valence_col) 

valence_data_pr <- as.data.frame(table(val_judg_pr$valence, val_judg_pr$actor_type))
valence_data_ch <- as.data.frame(table(val_judg_ch$valence, val_judg_ch$actor_type))

names(valence_data_pr) <- c("valence", "actor_type", "count")
names(valence_data_ch) <- c("valence", "actor_type", "count")

valence_data_pr$country <- "Peru"
valence_data_ch$country <- "Chile"
valence_data <- bind_rows(valence_data_pr, valence_data_ch)

valence_plot <- valence_data %>%
  mutate(
    count_plot = ifelse(valence == "positive", count, -count)
  )%>% filter(actor_type!= "cuenta sospechosa/bot")



# Plot both countries
ggplot(valence_plot, aes(x = actor_type, y = count_plot, fill = valence)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(labels = c(
    "ciudadano" = "Citoyen",
    "influencer" = "Influenceur",
    "medio" = "Média",
    "politico" = "Politicien"))+
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_wrap(~ country, scales = "fixed") +
  theme(
    panel.spacing = unit(2, "cm")
  )+  scale_fill_manual(
    values = c("positive" = "#2b83ba", "negative" = "#d7191c"),
    labels = c("positive" = "Positive", "negative" = "Negative")
  ) +
  labs(
    title = "Valence des jugements par type d'acteur",
    subtitle = "Comparaison par pays",
    x = "Type d'acteur",
    y = "N",
    fill = "Valence"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.spacing.x = unit(3,"lines")
  )

################
#candidate appreciation
################

# Prepare the data: aggregate positive and negative per variable and column
judg_actor_pos <- judg %>% filter(column== "pos_actor")%>%
  select(country, column, 
         starts_with("derecha"), starts_with("izquierda") )%>%
  pivot_longer(
    cols = -c(country, column),
    names_to = "metric",
    values_to = "count"
  ) %>%
  mutate(
    variable = gsub("_v_pos|_v_neg", "", metric),
    valence = case_when(
      grepl("_v_pos", metric) ~ "positive",
      grepl("_v_neg", metric) ~ "negative",
      TRUE ~ "total"
    )
  ) %>%
  filter(valence != "total") %>%
  mutate(
    count_plot = ifelse(valence == "positive", count, -count),
    variable_display = case_when(
      variable == "derecha" ~ "Right",
      variable == "izquierda" ~ "Left",
      TRUE ~ variable
    )  ) 


# First, find the maximum absolute value across both countries
max_value <- max(abs(judg_actor_pos$count_plot), na.rm = TRUE)

# Round up to a nice number
y_limit <- ceiling(max_value / 10) * 10

# Create a combined plot with both countries side by side
ggplot(judg_actor_pos, aes(x = variable_display, y = count_plot, fill = valence)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_wrap(~ country, scales = "fixed", ncol = 1) +
  theme(
    panel.spacing = unit(2, "cm")
  )+
  scale_y_continuous(limits = c(-y_limit, y_limit), breaks = seq(-y_limit, y_limit, by = 5)) +
  scale_fill_manual(
    values = c("positive" = "#2b83ba", "negative" = "#d7191c"),
    labels = c("positive" = "Positive", "negative" = "Negative")
  ) +
  labs(
    title = "Appreciation des Candidats",
    subtitle = "Comparaison par pays",
    x = "Position du candidat",
    y = "N",
    fill = "Valence"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )


####################
### candidate appreciation by variable and country

##############################  
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)


# Prepare the data: aggregate positive and negative per variable and column
judg_stacked <- judg %>% filter(column!= "judg_ev")%>% filter(column!= "pos_actor")%>%
  select(country, column, 
         starts_with("capacity"), starts_with("normality"), 
         starts_with("tenacity"), starts_with("integrity"),
         starts_with("civility"), starts_with("honesty") )%>%
  pivot_longer(
    cols = -c(country, column),
    names_to = "metric",
    values_to = "count"
  ) %>%
  mutate(
    variable = gsub("_v_pos|_v_neg", "", metric),
    valence = case_when(
      grepl("_v_pos", metric) ~ "positive",
      grepl("_v_neg", metric) ~ "negative",
      TRUE ~ "total"
    )
  ) %>%
  filter(valence != "total") %>%
  mutate(
    count_plot = ifelse(valence == "positive", count, -count),
    variable_display = case_when(
      variable == "capacity" ~ "Capacity",
      variable == "normality" ~ "Normality",
      variable == "tenacity" ~ "Tenacity",
      variable == "integrity" ~ "Integrity",
      variable == "civility" ~ "Civility",
      variable == "honesty" ~ "Honesty",
      TRUE ~ variable
    ),
    column_display = case_when(
      column == "judg_izq" ~ "Left",
      column == "judg_der" ~ "Right",
      TRUE ~ column
    )
  ) %>%
  # Filter variables with at least 5 total observations
  group_by(variable, country) %>%
  mutate(total_count = sum(abs(count_plot), na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(
    variable_display = ifelse(
      total_count >= 10,
      variable_display,
      "Other"
    )
  ) %>%
  # Aggregate "Other" category
  group_by(country, column, column_display, variable_display, valence) %>%
  summarise(
    count_plot = sum(count_plot, na.rm = TRUE),
    .groups = "drop"
  )



judg_filtered <- judg_stacked %>%
  group_by(variable_display) %>%
  filter(sum(abs(count_plot), na.rm = TRUE) > 10) %>%
  ungroup()%>% filter(variable_display != "Other")

# First, find the maximum absolute value across both countries
max_value <- max(abs(judg_filtered$count_plot), na.rm = TRUE)

# Round up to a nice number
y_limit <- ceiling(max_value / 10) * 10

# Variable labels
variable_labels <- c(
  "Capacity"   = "Capacité",
  "Normality"  = "Normalité",
  "Tenacity"   = "Ténacité",
  "Integrity"  = "Intégrité",
  "Civility"   = "Civilité",
  "Honesty"    = "Honnêteté"
)

# Country labels
country_labels <- c(
  "Chile" = "Chili",
  "Peru"  = "Pérou",
  "Perú"  = "Pérou"
)

# Orientation labels
column_labels <- c(
  "Left"  = "Gauche",
  "Right" = "Droite"
)

# Valence labels
valence_labels <- c(
  "positive" = "Positif",
  "negative" = "Négatif"
)

ggplot(judg_filtered, aes(x = column_display, y = count_plot, fill = valence)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_grid(
    country ~ variable_display,
    labeller = labeller(
      country          = country_labels,
      variable_display = variable_labels
    ),
    scales = "fixed",
  ) +
  scale_y_continuous(
    limits = c(-y_limit, y_limit),
    breaks = seq(-y_limit, y_limit, by = 10)
  ) +
  scale_x_discrete(labels = column_labels) +
  scale_fill_manual(
    values = c("positive" = "#2b83ba", "negative" = "#d7191c"),
    labels = valence_labels
  ) +
  labs(
    title = "Évaluation des candidats par dimension appréciative",
    subtitle = "Comparaison Chili / Pérou (Gauche vs Droite)",
    x = "Orientation",
    y = "Nombre de jugements",
    fill = "Valence"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.spacing.y = unit(2, "lines"))

####################
### event appreciation by variable and country

##############################  

# Prepare the data: aggregate positive and negative per variable and column
judg_event <- judg %>% filter(column== "judg_ev")%>% 
  select(country, column, 
         starts_with("power"), starts_with("impact"), 
         starts_with("conflictivity"), starts_with("integrity"))%>%
  pivot_longer(
    cols = -c(country, column),
    names_to = "metric",
    values_to = "count"
  ) %>%
  mutate(
    variable = gsub("_v_pos|_v_neg", "", metric),
    valence = case_when(
      grepl("_v_pos", metric) ~ "positive",
      grepl("_v_neg", metric) ~ "negative",
      TRUE ~ "total"
    )
  ) %>%
  filter(valence != "total") %>%
  mutate(
    count_plot = ifelse(valence == "positive", count, -count),
    variable_display = case_when(
      variable == "power" ~ "Power",
      variable == "impact" ~ "Impact",
      variable == "conflictivity" ~ "Conflictivity",
      variable == "integrity" ~ "Integrity",
      TRUE ~ variable
    )
  ) %>%
  # Filter variables with at least 5 total observations
  group_by(variable, country) %>%
  mutate(total_count = sum(abs(count_plot), na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(
    variable_display = ifelse(
      total_count >= 10,
      variable_display,
      "Other"
    )
  ) %>%
  # Aggregate "Other" category
  group_by(country, column, variable_display, valence) %>%
  summarise(
    count_plot = sum(count_plot, na.rm = TRUE),
    .groups = "drop"
  )


judg_event <- judg_event %>%
  group_by(variable_display) %>%
  filter(sum(abs(count_plot), na.rm = TRUE) > 10) %>%
  ungroup()%>% filter(variable_display != "Other")

# First, find the maximum absolute value across both countries
max_value <- max(abs(judg_event$count_plot), na.rm = TRUE)

# Round up to a nice number
y_limit <- ceiling(max_value / 10) * 10

# Create a combined plot with both countries side by side
ggplot(judg_event, aes(x = variable_display, y = count_plot, fill = valence)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_wrap(~ country, scales = "fixed", ncol = 1) +
  theme(
    panel.spacing = unit(2, "cm")
  )+  scale_y_continuous(limits = c(-y_limit, y_limit), breaks = seq(-y_limit, y_limit, by = 5)) +
  scale_fill_manual(
    values = c("positive" = "#2b83ba", "negative" = "#d7191c"),
    labels = c("positive" = "Positive", "negative" = "Negative")
  ) +
  labs(
    title = "Event Appreciation by Variable",
    subtitle = "Comparison between Chile and Peru",
    x = "Judgement Type",
    y = "Count",
    fill = "Valence"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  ) 




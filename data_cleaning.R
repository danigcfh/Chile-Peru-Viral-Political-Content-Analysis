library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(ineq)
library(DescTools)

ch_df <- read_excel("Chile_viral.xlsx") %>% 
  mutate( 
    content_type = as.factor(tolower(content_type)),
    actor_type = as.factor(tolower(`Tipo de actor`)),
    basis = as.factor(tolower(`Base`)),
    sentiment = as.factor(tolower(`Sentimientos`)),
    Eval = as.factor(tolower(`Eval`)),
    Candidato_pol = as.factor(tolower(`Candidato_pol`)),
    Candidato_att = as.factor(tolower(`Candidato_att`)),
    Pol_pub = as.factor(tolower(`Pol_pub`)),
    Endorsement = as.factor(tolower(`Endorsement`)),
    Personal = as.factor(tolower(`Personal`)),
    Denuncia = as.factor(tolower(`Denuncia`)),
    E_D = as.factor(tolower(`E_D`)),
    judg_izq = as.character(`Judgement izquierdas`),
    judg_der = as.character(`Judgement Derechas`),
    judg_ev = as.character(`Juicio evento`),
    pos_actor = as.character(tolower(`Posicionamiento de actor`)), 
    reactions_per_capita = statistics.reaction_count / 18.4,
    comments_per_capita = statistics.comment_count / 18.4,
    shares_per_capita = statistics.share_count / 18.4,
    views_per_capita = statistics.views/ 18.4,
    country = "Chile"
  )


pr_df <- read_excel("Peru_viral.xlsx")  %>% 
  mutate( 
    content_type = as.factor(tolower(content_type)),
    actor_type = as.factor(tolower(`Tipo de actor`)),
    basis = as.factor(tolower(`Base`)),
    sentiment = as.factor(tolower(`Sentimientos`)),
    Eval = as.factor(tolower(`Eval`)),
    Candidato_pol = as.factor(tolower(`Candidato_pol`)),
    Candidato_att = as.factor(tolower(`Candidato_att`)),
    Pol_pub = as.factor(tolower(`Pol_pub`)),
    Endorsement = as.factor(tolower(`Endorsement`)),
    Denuncia = as.factor(tolower(`Denuncia`)),
    E_D = as.factor(tolower(`E_D`)),
    Personal = as.factor(tolower(`Personal`)),
    Candidato_pol = as.factor(tolower(`Candidato_pol`)),
    judg_izq = as.character(`Judgement izquierdas`),
    judg_der = as.character(`Judgement Derechas`),
    judg_ev = as.character(`Juicio evento`),
    pos_actor = as.character(tolower(`Posicionamiento de actor`)), 
    reactions_per_capita = statistics.reaction_count / 34.2,
    comments_per_capita = statistics.comment_count / 34.2,
    shares_per_capita = statistics.share_count / 34.2, 
    views_per_capita = statistics.views/ 34.2,
    country = "Perú"
  )


df <- rbind (pr_df, ch_df)



saveRDS(df, "all_obs.rds")

prop.table(table(pr_df$actor_type))
prop.table(table(ch_df$actor_type))

decompose_column <- function(data, col_name, id_col = NULL) {
  
  # Get the column as character vector
  col_data <- as.character(data[[col_name]])
  
  if(!is.null(id_col)) {
    id_data <- as.character(data[[id_col]])
  } else {
    id_data <- seq_along(col_data)  # Fallback to row numbers
  }
  
  col_data[col_data == "NA" | col_data == ""] <- NA
  
  split_entries <- str_split(col_data, "[,;]\\s*")
  
  result <- data.frame()
  
  for(i in seq_along(split_entries)) {
    entries <- split_entries[[i]]
    
    if(length(entries) == 0 || all(is.na(entries)) || entries[1] == "NA") next
    
    for(entry in entries) {
      entry <- str_trim(entry)
      if(entry == "" || is.na(entry) || entry == "NA") next
      
      variable <- str_trim(str_extract(entry, "^[^\\+\\-]+"))
      raw_valence <- str_trim(str_extract(entry, "[\\+\\-]+$"))
      if(is.na(raw_valence)) {
        raw_valence <- str_extract(entry, "[\\+\\-]+")
      }
      
      if(!is.na(raw_valence)) {
        valence <- case_when(
          raw_valence %in% c("++", "+") ~ "positive",
          raw_valence %in% c("--", "-") ~ "negative",
          TRUE ~ raw_valence
        )
      } else {
        intensity <- NA
      }
      
      variable <- str_trim(variable)
      
      if(!is.na(variable) && variable != "") {
        result <- rbind(result, data.frame(
          id = id_data[i],  # Use the actual ID
          row_id = i,       # Keep row number for reference
          column = col_name,
          variable = variable,
          valence = valence,
          raw_valence = raw_valence,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(result)
}# Call the function with the column name as a string
ch_decomposed <- decompose_column(ch_df, col_name = "judg_izq", id_col="id")

columns <- c("judg_izq", "judg_der", "judg_ev","pos_actor")

results_ch <- list()
results_pr <- list()

for(col in columns) {
  cat("Processing:", col, "\n")
  results_ch[[col]] <- decompose_column(ch_df, col, id_col="id")
  results_pr[[col]] <- decompose_column(pr_df, col, id_col="id")
  
}

judg_ch <- bind_rows(results_ch) %>% 
  mutate( 
    column =  as.factor(tolower(column)),
    variable = as.factor(tolower(variable)),
    valence = as.factor(tolower(valence)),
    raw_valence = as.factor(tolower(raw_valence))
    ) %>% left_join(ch_df, by = "id")

judg_ch_summary <- judg_ch %>% group_by(column) %>% summarise(
    capacity = sum(variable=="capacity"),
    capacity_v_pos = sum(valence[variable=="capacity"]=="positive"),
    capacity_v_neg = sum(valence[variable=="capacity"]=="negative"),

    normality = sum(variable=="normality"),
    normality_v_pos = sum(valence[variable=="normality"]=="positive"),
    normality_v_neg = sum(valence[variable=="normality"]=="negative"),
    
    tenacity = sum(variable=="tenacity"),
    tenacity_v_pos = sum(valence[variable=="tenacity"]=="positive"),
    tenacity_v_neg = sum(valence[variable=="tenacity"]=="negative"),

    integrity = sum(variable=="integrity"),
    integrity_v_pos = sum(valence[variable=="integrity"]=="positive"),
    integrity_v_neg = sum(valence[variable=="integrity"]=="negative"),

    civility = sum(variable=="civility"),
    civility_v_pos = sum(valence[variable=="civility"]=="positive"),
    civility_v_neg = sum(valence[variable=="civility"]=="negative"),
    
    honesty = sum(variable=="honesty"),
    honesty_v_pos = sum(valence[variable=="honesty"]=="positive"),
    honesty_v_neg = sum(valence[variable=="honesty"]=="negative"),
    
    power = sum(variable=="power"),
    power_v_pos = sum(valence[variable=="power"]=="positive"),
    power_v_neg = sum(valence[variable=="power"]=="negative"),
    
    impact = sum(variable=="impact"),
    impact_v_pos = sum(valence[variable=="impact"]=="positive"),
    impact_v_neg = sum(valence[variable=="impact"]=="negative"),
    
    conflictivity = sum(variable=="conflictivity"|variable=="conflicitivity"|variable=="coflictivity"),
    conflictivity_v_pos = sum(valence[variable=="conflictivity"]=="positive"),
    conflictivity_v_neg = sum(valence[variable=="conflictivity"]=="negative"),
    

    derecha = sum(variable=="derecha"),
    derecha_v_pos = sum(valence[variable=="derecha"]=="positive"),
    derecha_v_neg = sum(valence[variable=="derecha"]=="negative"),
    
    izquierda = sum(variable=="izquierda"),
    izquierda_v_pos = sum(valence[variable=="izquierda"]=="positive"),
    izquierda_v_neg = sum(valence[variable=="izquierda"]=="negative"),
    
    neutro = sum(variable=="neutro"),
    country = "Chile"
    
  )

    
judg_pr <- bind_rows(results_pr)%>% 
  mutate( 
    column =  as.factor(tolower(column)),
    variable = as.factor(tolower(variable)),
    valence = as.factor(tolower(valence)),
    raw_valence = as.factor(tolower(raw_valence))
  ) %>% left_join(pr_df, by = "id")


judg_pr_summary <- judg_pr %>% group_by(column) %>% summarise(
    capacity = sum(variable=="capacity"),
    capacity_v_pos = sum(valence[variable=="capacity"]=="positive"),
    capacity_v_neg = sum(valence[variable=="capacity"]=="negative"),
    
    normality = sum(variable=="normality"),
    normality_v_pos = sum(valence[variable=="normality"]=="positive"),
    normality_v_neg = sum(valence[variable=="normality"]=="negative"),
    
    tenacity = sum(variable=="tenacity"),
    tenacity_v_pos = sum(valence[variable=="tenacity"]=="positive"),
    tenacity_v_neg = sum(valence[variable=="tenacity"]=="negative"),
    
    integrity = sum(variable=="integrity"),
    integrity_v_pos = sum(valence[variable=="integrity"]=="positive"),
    integrity_v_neg = sum(valence[variable=="integrity"]=="negative"),
    
    civility = sum(variable=="civility"),
    civility_v_pos = sum(valence[variable=="civility"]=="positive"),
    civility_v_neg = sum(valence[variable=="civility"]=="negative"),
    
    honesty = sum(variable=="honesty"),
    honesty_v_pos = sum(valence[variable=="honesty"]=="positive"),
    honesty_v_neg = sum(valence[variable=="honesty"]=="negative"),
    
    power = sum(variable=="power"),
    power_v_pos = sum(valence[variable=="power"]=="positive"),
    power_v_neg = sum(valence[variable=="power"]=="negative"),
    
    impact = sum(variable=="impact"),
    impact_v_pos = sum(valence[variable=="impact"]=="positive"),
    impact_v_neg = sum(valence[variable=="impact"]=="negative"),
    
    conflictivity = sum(variable=="conflictivity"|variable=="conflicitivity"|variable=="coflictivity"),
    conflictivity_v_pos = sum(valence[variable=="conflictivity"]=="positive"),
    conflictivity_v_neg = sum(valence[variable=="conflictivity"]=="negative"),
    
    derecha = sum(variable=="derecha"),
    derecha_v_pos = sum(valence[variable=="derecha"]=="positive"),
    derecha_v_neg = sum(valence[variable=="derecha"]=="negative"),
    
    izquierda = sum(variable=="izquierda"),
    izquierda_v_pos = sum(valence[variable=="izquierda"]=="positive"),
    izquierda_v_neg = sum(valence[variable=="izquierda"]=="negative"),
    
    neutro = sum(variable=="neutro"),
    country = "Perú"
      )


judg_combined <- bind_rows(judg_ch, judg_pr)
judg_summary_combined <- bind_rows(judg_ch_summary, judg_pr_summary)

saveRDS(judg_combined, "judg_combined.rds")
saveRDS(judg_summary_combined, "judg_summary_combined.rds")


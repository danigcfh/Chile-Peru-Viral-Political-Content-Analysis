library(dplyr)
library(stringr)

Peru_2026 <- read_csv("Peru_2026.csv")
Chile_2025 <- read_csv("Chile_2025.csv")

min_word_length <- 10

Peru_2026 <- Peru_2026 %>%
  filter(sapply(strsplit(text, "\\s+"), length) >= min_word_length)

Chile_2025 <- Chile_2025 %>%
  filter(sapply(strsplit(text, "\\s+"), length) >= min_word_length)

#exclude non peru related posts

keywords_lower <- tolower(c("Colombia", "#Colombia", "#AbelardoPresidente", 
                            "#ElTigreEnPrimera", "#ColombiaDecide",
                            "#abelardodelaespriella", "#CepedaPresidente",
                            "#PP", "#PSOE", "#VOX", "#ColombiaVota", "COLOMBIA",
                            "Espriella","cepeda", "España", "#PolíticaColombiana"))

pattern <- paste(keywords_lower, collapse = "|")

keyword <- Peru_2026 %>%
  filter(str_detect(tolower(text), pattern))

ids <- keyword$id
Peru_filtered <- Peru_2026 %>% subset(!id %in% ids)

#same thing for Chile

keywords_lower <- tolower(c("Honduras", "#EleccionesHonduras2025", "Aldana", 
                            "Asfura", "Nasralla",
                            "#Honduras", "#ClaudiaSheinbaum",
                            "Sheinbaum", "Guajardo", "#CorreoArgentino" ))

pattern <- paste(keywords_lower, collapse = "|")

keyword <- Chile_2025 %>%
  filter(str_detect(tolower(text), pattern))

ids <- keyword$id
Chile_filtered <- Chile_2025 %>% subset(!id %in% ids)


#  keep top  posts by engagement in each country

Peru_viral <- Peru_filtered %>%
  arrange(desc(statistics.reaction_count)) %>%
  group_by(post_owner.id) %>%  
  slice_head(n = 5) %>%       
  ungroup() %>%
  arrange(desc(statistics.reaction_count)) %>%
  slice_head(n = 120)

Chile_viral <- Chile_filtered %>%
  arrange(desc(statistics.reaction_count)) %>%
  group_by(post_owner.id) %>%  
  slice_head(n = 5) %>%       
  ungroup() %>%
  arrange(desc(statistics.reaction_count)) %>%
  slice_head(n = 120)



write.csv(Peru_viral, "Peru_viral.csv")
write.csv(Chile_viral, "Chile_viral.csv")






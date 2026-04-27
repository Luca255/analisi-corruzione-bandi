library(tidyverse)


red_flag_stazioni = read.csv("dati/red_flag_stazioni_2011_2025.csv", header=T)
stazioni = read.csv2("dati/stazioni-appaltanti_csv.csv", header=T)


# Caricamento del ranking fatto sui soli comuni
ranking = read.csv("dati/final_ranking.csv", header=T)
ranking$rank = 1:nrow(ranking)

# Aggiunta del rank alle stazioni con red flag
red_flag_stazioni_rank = red_flag_stazioni %>% 
  left_join(ranking %>% select(CF_stazione, rank), 
            by="CF_stazione") %>% 
  filter(complete.cases(.))


# Preprocessing dei dati
red_flag_stazioni_rank = red_flag_stazioni_rank %>% 
  filter(rank <= 1000) %>% 
  left_join(stazioni %>% 
              select(codice_fiscale, provincia_nome, codice_fiscale),  
            by=c("CF_stazione" = "codice_fiscale")) %>% 
  filter(complete.cases(.)) %>% 
  mutate(provincia_nome = str_trim(toupper(provincia_nome))) %>% 
  mutate(provincia_nome = case_when(
    provincia_nome == "REGGIO CALABRIA" ~ "REGGIO DI CALABRIA",
    provincia_nome == "FORLÌ-CESENA" ~ "FORLI' - CESENA",
    provincia_nome == "MONZA E DELLA BRIANZA" ~ "MONZA AND BRIANZA",
    provincia_nome == "MANTOVA" ~ "MANTUA",
    provincia_nome == "SIRACUSA" ~ "SYRACUSE",
    provincia_nome == "FIRENZE" ~ "FLORENCE",
    provincia_nome == "PADOVA" ~ "PADUA",
    provincia_nome == "CARBONIA IGLESIAS" ~ "CARBONIA-IGLESIAS",
    provincia_nome == "MASSA-CARRARA" ~ "MASSA CARRARA",
    provincia_nome == "REGGIO EMILIA" ~ "REGGIO NELL'EMILIA",
    provincia_nome == "VERBANO" ~ "VERBANO-CUSIO-OSSOLA",
    TRUE ~ provincia_nome
  ))




red_flag_stazioni_rank %>% 
  group_by(provincia_nome) %>% 
  summarise(median_rank = median(rank)) %>% 
  arrange(median_rank) %>% 
  mutate(median_rank_pos = row_number()) %>% 
  print(n=30)


library(sf)
library(geodata)

italy = gadm(country = "ITA", level = 2, path = tempdir())

italy_sf = st_as_sf(italy) %>% 
  mutate(NAME_2 = str_trim(toupper(NAME_2)))

mappa = italy_sf %>%
  left_join(red_flag_stazioni_rank %>% 
              group_by(provincia_nome) %>% 
              summarise(median_rank = median(rank)) %>% 
              arrange(median_rank),
            by = c("NAME_2" = "provincia_nome"))

# Province senza valori
setdiff(italy_sf$NAME_2, red_flag_stazioni_rank$provincia_nome)

ggplot(mappa) +
  geom_sf(aes(fill = median_rank), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    na.value = "grey90",
    guide = guide_colorbar(reverse = TRUE),
    direction = -1
  ) +
  theme_minimal() +
  labs(fill = "Ranking mediano")

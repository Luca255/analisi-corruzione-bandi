
# codice per preprocessing e unire csv ------------------------------------


# SA        codice_fiscale                              <-> cf_amministrazione_appaltante     bando
# bando     cig | cig_accordo_quadro | CIG_COLLEGAMENTO <-> cig                               aggiudicazioni

library(tidyverse)

# Nelle seguenti sezioni, ogni dataset usato viene scaricato, e preprocessato
# vengono selezionate solo colonne utilizzate nell'analisi o potenzialmente utili per gli scopi di questa analisi
# Nei casi con 2 colonne di dataset diversi che rappresentano lo stesso elemento, 
# la colonna preferita è sempre stata confermata avere le stesse informazioni dell'altra, più altre che mancavano a quest'ultima


# load Bando cig ----------------------------------------------------------
library(data.table)

# prima di tutto, scaricare tutti i bandi cig voluti con l'apposito codice python
base_path <- "Bando_cig_files/" # cartella in cui sono stati scaricate tutte le cartelle dei bandi cig per anno


# list all zip files recursively
files <- list.files(
  base_path,
  pattern = "\\.zip$",
  full.names = TRUE,
  recursive = TRUE
)

# colonne da leggere, per evitare di doverle leggere tutte, meglio a livello di costo computazionale
Bando_cols_keep <- c(
  "cig",
  "cig_accordo_quadro",
  "numero_gara",
  "oggetto_gara",
  "importo_complessivo_gara",
  "settore",
  "data_pubblicazione",
  "data_scadenza_offerta",
  "cod_tipo_scelta_contraente",
  "tipo_scelta_contraente",
  "cod_modalita_realizzazione",
  "cf_amministrazione_appaltante",
  "denominazione_amministrazione_appaltante",
  "descrizione_cpv",
  "CIG_COLLEGAMENTO",
  "COD_ESITO",
  "ESITO",
  "DATA_COMUNICAZIONE_ESITO"
)

# function to read each zip
read_zip_csv <- function(zipfile){
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  unzip(zipfile, exdir = temp_dir)
  
  csv_file <- list.files(temp_dir, pattern="\\.csv$", full.names=TRUE)
  
  dt <- fread(
    csv_file, 
    select = Bando_cols_keep,
    colClasses = c(
      cf_amministrazione_appaltante = "character",
      codice_ausa = "character"
    )
  )
  
  unlink(temp_dir, recursive = TRUE)  # clean temp files
  
  print(paste("done:", basename(zipfile)))
  
  return(dt)
}

# read and combine everything
final_cig_df <- rbindlist(
  lapply(files, read_zip_csv), 
  fill = TRUE) %>% 
  as.data.frame() %>% 
  mutate(
    data_pubblicazione = as.Date(data_pubblicazione),
    data_scadenza_offerta = as.Date(data_scadenza_offerta),
    DATA_COMUNICAZIONE_ESITO = as.Date(DATA_COMUNICAZIONE_ESITO)
  ) %>% 
  rename(
    codice_fiscale = cf_amministrazione_appaltante,             
    # stato_vita_cig = stato, # tolto perché la variabile è stata tolta
    denominazione_scritto_simile = denominazione_amministrazione_appaltante
  )

Bando %>% glimpse()

## salva csv
# write.csv(final_cig_df, "cig_2011_2025.csv", row.names = FALSE) # check se c'è modo per vedere barra avanzamaento con write.csv
# library(data.table)
# final_cig_df <- fread("cig_2011_2025.zip", colClasses = c(codice_fiscale = "character", codice_ausa = "character"))
final_cig_df %>% glimpse()

Bando = final_cig_df %>% 
  mutate(
    data_pubblicazione = as.Date(data_pubblicazione),
    data_scadenza_offerta = as.Date(data_scadenza_offerta),
    DATA_COMUNICAZIONE_ESITO = as.Date(DATA_COMUNICAZIONE_ESITO)
  ) %>% 
  select(
    cig,
    cig_accordo_quadro,
    numero_gara,
    oggetto_gara,
    importo_complessivo_gara,
    # n_lotti_componenti,
    # oggetto_lotto,
    # importo_lotto,
    # oggetto_principale_contratto,
    # stato,
    settore,
    # luogo_istat,
    # provincia,
    data_pubblicazione,
    data_scadenza_offerta,
    cod_tipo_scelta_contraente,
    tipo_scelta_contraente,
    cod_modalita_realizzazione,
    # modalita_realizzazione,
    # codice_ausa,                # il codice ausa nel dataset della stazione appaltante ha più codici di questa colonna, avendo corrispondenti tutti gli altri 
    codice_fiscale,
    denominazione_scritto_simile,
    # sezione_regionale,
    # id_centro_costo,
    # denominazione_centro_costo,
    # anno_pubblicazione,
    # mese_pubblicazione,
    # cod_cpv,
    descrizione_cpv,
    # flag_prevalente,
    # COD_MOTIVO_CANCELLAZIONE,
    # MOTIVO_CANCELLAZIONE,
    # DATA_CANCELLAZIONE,
    # DATA_ULTIMO_PERFEZIONAMENTO,
    # COD_MODALITA_INDIZIONE_SPECIALI,
    # MODALITA_INDIZIONE_SPECIALI,
    # COD_MODALITA_INDIZIONE_SERVIZI,
    # MODALITA_INDIZIONE_SERVIZI,
    # DURATA_PREVISTA,
    # COD_STRUMENTO_SVOLGIMENTO,
    # STRUMENTO_SVOLGIMENTO,
    # FLAG_URGENZA,
    # COD_MOTIVO_URGENZA,
    # MOTIVO_URGENZA,
    # FLAG_DELEGA,
    # FUNZIONI_DELEGATE,
    # CF_SA_DELEGANTE,
    # DENOMINAZIONE_SA_DELEGANTE,
    # CF_SA_DELEGATA,
    # DENOMINAZIONE_SA_DELEGATA,
    # IMPORTO_SICUREZZA,
    # TIPO_APPALTO_RISERVATO,
    # CUI_PROGRAMMA,
    # FLAG_PREV_RIPETIZIONI,
    # COD_IPOTESI_COLLEGAMENTO,
    # IPOTESI_COLLEGAMENTO,
    CIG_COLLEGAMENTO,
    COD_ESITO,
    ESITO,
    DATA_COMUNICAZIONE_ESITO
    # FLAG_PNRR_PNC
  )

Bando$DATA_COMUNICAZIONE_ESITO %>% summary()
Bando %>% nrow() # 9966106



# Stazione appaltante -----------------------------------------------------

SA = read.csv("ANAC_datasets/stazioni-appaltanti_csv.csv", header = T, sep = ";") %>% 
  mutate(
    data_inizio = as.Date(data_inizio),
    data_fine = as.Date(data_fine),
    # codice_ausa = str_remove(codice_ausa, "^0+")
  ) %>% 
  select(
    # selected columns:
    codice_fiscale, 
    partita_iva, 
    denominazione, 
    codice_ausa,
    natura_giuridica_codice, 
    # natura_giuridica_descrizione,
    # soggetto_estero, 
    provincia_codice, 
    # provincia_nome,
    # citta_codice, 
    # citta_nome,
    # indirizzo_odonimo,
    # cap,
    # flag_inHouse, 
    # flag_partecipata, 
    stato, 
    data_inizio, 
    data_fine 
    
    # not selected columns:
    # , , , indirizzo_odonimo, 
  ) %>% 
  rename(
    stato_banca_dati = stato
  ) %>% 
  distinct(codice_fiscale, .keep_all = TRUE) # per rimuovere solo 1 riga duplicata se non per la data che cambia, dato non utile per l'analisi

SA %>% nrow() # 46873
SA %>% names


# Aggiudicazioni ----------------------------------------------------------

# file download data: 27/02/2026
AGG = read.csv("ANAC_datasets/aggiudicazioni_csv.csv", header = T, sep = ";")  %>% 
  mutate(
    data_aggiudicazione_definitiva = as.Date(data_aggiudicazione_definitiva),
    data_comunicazione_esito  = as.Date(data_comunicazione_esito),
    flag_subappalto = as.logical(flag_subappalto),
    DATA_INCARICO_PROG  = as.Date(DATA_INCARICO_PROG),
    DATA_CONS_PROG      = as.Date(DATA_CONS_PROG)
  ) %>%
  select(
    cig,
    data_aggiudicazione_definitiva,
    # esito,                      # non necessita coalesce perché corrisponde perfettamente ai casi aggiudicati in Bando
    criterio_aggiudicazione,
    # data_comunicazione_esito,   # analogo ad esito
    numero_offerte_ammesse,
    numero_offerte_escluse,
    importo_aggiudicazione,
    ribasso_aggiudicazione,
    num_imprese_offerenti,
    flag_subappalto,
    id_aggiudicazione,
    # cod_esito,                  # analogo ad esito
    num_imprese_richiedenti,
    # asta_elettronica,
    num_imprese_invitate,
    massimo_ribasso,
    minimo_ribasso,
    # FLAG_SCOMPUTO,
    # COD_PRESTAZIONI_COMPRESE,
    # PRESTAZIONI_COMPRESE,
    CIG_PROG_ESTERNA,
    # DATA_INCARICO_PROG,
    # DATA_CONS_PROG,
    # COD_MODO_RIAGGIUDICAZIONE,
    # MODO_RIAGGIUDICAZIONE,
    FLAG_PROC_ACCELERATA
    # N_MANIF_INTERESSE
  )



# Aggiudicatari -----------------------------------------------------------

aggiudicatari = read.csv("ANAC_datasets/aggiudicatari_csv.csv", header = T, sep = ";") %>%
  select(
    cig,
    ruolo,
    codice_fiscale,
    denominazione,
    tipo_soggetto,
    id_aggiudicazione
  ) %>% 
  rename(
    CF_aggiudicatario = codice_fiscale,
    denominazione_aggiudicatario = denominazione
  )
  



# Unire i dataset ---------------------------------------------------------

# Bando: # 9966106 righe


# attuale unità delle righe: una sola gara, con più tipologie di lavori associate (ex. stessi campi ma cambia la descrizione)

Bando_SA_AGG <- Bando %>%
  left_join(
    SA,
    by = "codice_fiscale" #c("cf_amministrazione_appaltante" = "codice_fiscale"),
  ) %>% 
  rename(
    CF_stazione = codice_fiscale
  ) %>% 
  left_join(
    AGG,
    by = "cig",
    relationship = "many-to-many"
  )
Bando_SA_AGG %>% nrow() # 9976944
# Bando: # 9966106


dati <- Bando_SA_AGG %>%
  left_join(
    aggiudicatari,
    by = c("id_aggiudicazione", "cig")#,
    # relationship = "many-to-many"
  ) %>%
  as_tibble()

dati %>% nrow() # 10383739
# Bando: # 9966106


# write.csv(df_full, "dataset_cig_completo.csv", row.names = FALSE)


# Seleziono solo le stazioni appaltanti per cui si hanno almeno 2 bandi
stazioni_un_appalto = dati %>% 
  group_by(CF_stazione) %>% 
  summarise(n_appalti = n()) %>% 
  filter(n_appalti == 1) %>% 
  pull(CF_stazione)

stazioni_un_appalto %>% length()
# 3722 stazioni con 1 solo bando

dati = dati %>% 
  filter(! CF_stazione %in% stazioni_un_appalto)

dati %>% nrow()
# ora: 10380017
# prima: 10383739

# Aggregazione affidamenti vari -------------------------------------------

### rendere questo più corto computazionalmente
dati = dati %>%
  mutate(tipo_scelta_contraente = case_when(
    
    # Affidamenti diretti (tutte le varianti)
    grepl("AFFIDAMENTO DIRETTO", tipo_scelta_contraente, ignore.case = TRUE) ~ 
      "Affidamento diretto",
    
    # Procedure negoziate
    grepl("PROCEDURA NEGOZIATA", tipo_scelta_contraente, ignore.case = TRUE) ~ 
      "Procedura negoziata",
    
    # Procedura aperta
    grepl("PROCEDURA APERTA", tipo_scelta_contraente, ignore.case = TRUE) ~ 
      "Procedura aperta",
    
    # Competitiva / dialogo
    grepl("DIALOGO COMPETITIVO|COMPETITIVA", tipo_scelta_contraente, ignore.case = TRUE) ~ 
      "Procedura competitiva",
    
    # Partenariato
    grepl("PARTENARIATO", tipo_scelta_contraente, ignore.case = TRUE) ~ 
      "Partenariato",
    
    # Altri casi
    # TRUE ~ "Altro"
  ))





# Varianti e fine contratto -----------------------------------------------

varianti = read.csv2("ANAC_datasets/varianti_csv.csv", header=T) %>% 
  as_tibble()

fine_contratto = read.csv2("ANAC_datasets/fine-contratto_csv.csv", 
                           header=T) %>% as_tibble()

dati_num_varianti = varianti %>% 
  group_by(cig, id_aggiudicazione) %>% 
  summarise(num_varianti = n(), .groups = "drop")

# finito dovrebbe essere la flag che indica se il contratto è finito oppure no,
# in questo caso ovviamente è sempre uguale a 1 perché per definizione questo
# dataset contiene le informazioni sui contratti finiti.
dati_fine_contratto = fine_contratto %>% 
  group_by(cig, id_aggiudicazione) %>% 
  summarise(finito = 1, .groups = "drop")

dati = dati %>% 
  left_join(dati_fine_contratto, by = c("cig", "id_aggiudicazione")) %>% 
  left_join(dati_num_varianti, by = c("cig", "id_aggiudicazione"))



# Quadro economico --------------------------------------------------------

quadro = read.csv2("ANAC_datasets/quadro-economico_csv.csv", header=T) %>% 
  as_tibble() %>% 
  mutate(across(c(importo_sicurezza, importo_forniture, importo_lavori,
                  importo_progettazione, somme_a_disposizione,
                  importo_servizi, ulteriori_oneri_non_soggetti_ribasso),
                ~ as.numeric(.))) %>% 
  mutate(across(c(importo_sicurezza, importo_forniture, importo_lavori,
                  importo_progettazione, somme_a_disposizione,
                  importo_servizi, ulteriori_oneri_non_soggetti_ribasso), 
                ~ ifelse(is.na(.), 0, .)))

quadro_ok = quadro %>% 
  filter(descrizione_evento == "CONSUNTIVO") %>% 
  group_by(cig, id_aggiudicazione) %>% 
  summarise(
    importo_sicurezza = max(importo_sicurezza),
    importo_forniture = max(importo_forniture),
    importo_lavori = max(importo_lavori),
    importo_progettazione = max(importo_progettazione),
    somme_a_disposizione = max(somme_a_disposizione),
    importo_servizi = max(importo_servizi),
    ulteriori_oneri_non_soggetti_ribasso = max(ulteriori_oneri_non_soggetti_ribasso),
    .groups = "drop"
  ) %>% 
  mutate(importo_finale = importo_lavori + importo_servizi + importo_forniture +
           importo_progettazione)

# Controllo che non ci siano altri duplicati
# quadro_ok %>%
#   group_by(cig, id_aggiudicazione) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))

dati = dati %>% 
  left_join(quadro_ok, by = c("cig", "id_aggiudicazione"))





# Unione partecipanti -----------------------------------------------------

partecipanti = read.csv2("ANAC_datasets/partecipanti_csv.csv", header=T)
# View(partecipanti)

setDT(partecipanti)
counts = partecipanti[, .N, by=cig] %>% 
  as_tibble() %>% 
  rename(num_partecipanti = N)

dati = dati %>% 
  left_join(counts, by = "cig") %>% 
  relocate(num_partecipanti, .after = "num_imprese_offerenti")



# aggiudicatari = read.csv("ANAC_datasets/aggiudicatari_csv.csv", header = T, sep = ";") %>%
#   select(
#     cig,
#     ruolo,
#     codice_fiscale,
#     denominazione,
#     tipo_soggetto,
#     id_aggiudicazione
#   ) %>% 
#   rename(
#     CF_aggiudicatario = codice_fiscale,
#     denominazione_aggiudicatario = denominazione
#   )

setDT(aggiudicatari)
counts_aggiudicatari = aggiudicatari[, .N, by=cig] %>% 
  as_tibble() %>% 
  rename(num_aggiudicatari = N)

dati = dati %>% 
  left_join(counts_aggiudicatari, by = "cig") %>% 
  relocate(num_aggiudicatari, .after = "numero_offerte_ammesse")

dati %>% nrow() # 10380017 # uguale a prima, come dovrebbe essere

# coalesce denominazioni --------------------------------------------------


# le colonne denominazione e denominazione scritto simile corrispondono rispettivamente alla colonna denominazione nei file di Stazione Appaltante e Bando
# Le 2 però non sono identiche, in varie denominazioni sono presenti piccoli o grandi cambiamenti che possono variare da un trattino in più o in meno, fino a varie parole aggiunte
# In altri casi, il corrispettivo nell'altra colonna è proprio mancante
# con Il coalesce risolviamo il problema dei mancanti, e consideriamo come colonna denominazione principale quella del file della Stazione Appaltante poiché sembra più esaustiva e completa nella maggiorparte dei casi in cui le due differiscono

dati = dati %>% mutate(
  denominazione = coalesce(denominazione, denominazione_scritto_simile)
)


dati %>% names()

# alcune stazioni hanno la denominazione scritta in modo diverso usando lo stesso codice fiscale
# (ex. a volte scritto con o senza l'accento ' ) 
# (poterbbero essere stati tra i casi in cui il campo denominazione era NA, ed è stato quindi rimpiazzato dal campo denominazione_scritto_simile)
# cambio quindi la denominazione in modo ad averne 1 unica per la stessa azienda
dati[dati$CF_stazione == "88888888885",]$denominazione = "ASSOCIAZIONI, UNIONI O CONSORZI PRIVI DI PERSONALITA' GIURIDICA"
dati[dati$CF_stazione == "01335970693",]$denominazione = "UNIVERSITA' DEGLI STUDI G. D'ANNUNZIO CHIETI - PESCARA"

# questa stazione invece aveva in un paio di casi il codice scritto con 5 zeri in più e la denominazione leggeremente diversa come i casi sopra
dati[dati$CF_stazione == "0000093068580666",]$CF_stazione = "93068580666"
dati[dati$CF_stazione == "93068580666",]$denominazione = "UFFICIO SPECIALE RICOSTRUZIONE CITTA' DE L'AQUILA"


# Probabilmente errori simili potrebbero esssercene ancora presenti nle dataset
# ma per ovvie ragioni non vale abbastanza la pena da trovarle e sistemarle tutte

# Sono anche presenti 243 casi con denominazione e codice Fiscale vuoti: ""
# Nell'analisi li considereremo con una azienda singola, senza toglierli

write.csv(dati, "dati_2011_2025.csv", row.names = FALSE)
# ~ 15-20 min con CPU

dati %>% nrow() # 10380017 # uguale a prima, come dovrebbe essere
dati %>% ncol() # 57 

Bando %>% nrow() # 9966106



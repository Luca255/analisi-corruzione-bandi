
# rm(list = ls())
library(tidyverse)
library(data.table)

# caricare il dataset creato nel file precedente
dati <- fread("dati/dati_2011_2025.zip", # più veloce di read.csv vista la mole di dati (~10 milioni di righe, ~60 colonne)
              colClasses = c( # fread non riconosce che sono stringhe, altrimenti le tratta come numeri
                CF_stazione = "character", 
                codice_ausa = "character")
              ) %>% 
  as_tibble() %>% 
  mutate( # passa dal formato iDate -> Date 
    data_pubblicazione = as.Date(data_pubblicazione),
    data_scadenza_offerta = as.Date(data_scadenza_offerta),
    data_inizio = as.Date(data_inizio),
    data_fine = as.Date(data_fine),
    data_aggiudicazione_definitiva = as.Date(data_aggiudicazione_definitiva),
    DATA_COMUNICAZIONE_ESITO = as.Date(DATA_COMUNICAZIONE_ESITO),
  )


# Creazione delle red flag -------------------------------------------------

# Nota: le variabili indicate con una lettera minuscola sono quelle inventate.
# Invece quelle indicate con un numero sono le red flag tratte dalla 
# documentazione ufficiale ANAC


# 13. Proporzione di contratti aggiudicati alla stessa azienda
red_flag_13 = dati %>% 
  group_by(CF_stazione) %>% 
  count(CF_aggiudicatario) %>% 
  mutate(f = n / sum(n)) %>% 
  summarise(
    K = n(),
    O = 1 / (K/(K-1) * (1 - sum(f^2)))
  ) %>% 
  select(-K)


red_flag_stazioni = dati %>% 
  mutate(num_offerte_ammesse_ok = ifelse(numero_offerte_ammesse != 0,
                                         numero_offerte_ammesse,
                                         num_aggiudicatari),
         num_offerte_totali_ok = ifelse(num_imprese_offerenti != 0,
                                         num_imprese_offerenti,
                                         num_partecipanti)) %>% 
  mutate(num_offerte_totali_ok = ifelse(num_offerte_ammesse_ok > num_offerte_totali_ok,
                                     num_offerte_ammesse_ok,
                                     num_offerte_totali_ok)) %>% 
  group_by(CF_stazione, denominazione) %>% 
  summarize(
    # a. Numero totale di appalti
    num_appalti = n(),
    
    # b. Numero di appalti con affidamento diretto
    num_appalti_diretti = sum(tipo_scelta_contraente == "Affidamento diretto"),
    
    # c. Valore totale delle gare
    val_tot = sum(importo_complessivo_gara),
    
    # d. Proporzione di appalti con criterio di aggiudicazione mancante
    prop_na_criterio_aggiudicazione = mean(is.na(criterio_aggiudicazione)),
    
    # e. Proporzione di appalti con numero di offerte mancante
    prop_na_offerte_ammesse = mean(is.na(num_offerte_ammesse_ok)),
    
    # f. Proporzione di appalti con importo complessivo mancante
    prop_na_importo_gara = mean(is.na(importo_complessivo_gara)),
    
    # g. Proporzione di bandi con data di aggiudicazione definitiva precedente alla 
    # data di scadenza dell'offerta
    prop_date_contrarie = mean(data_aggiudicazione_definitiva < 
                                 data_scadenza_offerta, na.rm=T),
    
    # 1. Offerta economicamente più vantaggiosa
    offerta_econo_vant = sum(criterio_aggiudicazione == 
                               "OFFERTA ECONOMICAMENTE PIÙ VANTAGGIOSA: CRITERIO DEL MINOR PREZZO", 
                             na.rm = T) / sum(!is.na(criterio_aggiudicazione)),
    
    # 2. Numero delle procedure non aperte
    num_proc_non_aperte = sum(tipo_scelta_contraente %in%
                                c("Affidamento diretto", "Procedura negoziata")) / 
      sum(!is.na(tipo_scelta_contraente)),
    
    # 3. Valore delle procedure non aperte
    val_proc_non_aperte = sum(importo_complessivo_gara[
      tipo_scelta_contraente %in% c("Affidamento diretto", "Procedura negoziata")],
      na.rm = TRUE) / sum(importo_complessivo_gara, na.rm = T),
    
    # 4. Contratti aggiudicati e modificati per effetto di almeno una variante
    contr_almeno_una_var = sum(num_varianti[finito==1] > 0, na.rm=T) / 
                              sum(finito, na.rm=T),
    
    # 5. Scostamento dei costi di esecuzione
    scostamento_costi = mean(importo_finale/importo_aggiudicazione, na.rm=T),
    
    # 7. Inadempimento delle comunicazioni di aggiudicazione
    inademp_comun_aggiudicazione = sum(ESITO == "") / num_appalti,
    
    # 9. Offerta singola
    offerta_singola = sum(num_offerte_ammesse_ok == 1 & num_offerte_totali_ok == 1, 
                          na.rm = T) /
      sum(num_offerte_ammesse_ok != 0 & num_offerte_totali_ok != 0,
          na.rm = T),
    
    # 10. Proporzione di offerte escluse
    prop_offerte_escluse = mean(
      1 - num_offerte_ammesse_ok / num_offerte_totali_ok,
      na.rm = TRUE
    ),
    
    # 11. Esclusione di tutte le offerte tranne una
    escl_tutte_tranne_una = sum(num_offerte_ammesse_ok == 1 & num_offerte_totali_ok > 1, 
                                na.rm = T) /
      sum(num_offerte_ammesse_ok == 1, na.rm = T),
    
    # 12. Proporzione di offerte escluse in procedure con tutte le offerte escluse tranne una
    prop_offerte_escluse_12 = mean(
      1 - num_offerte_ammesse_ok[num_offerte_ammesse_ok == 1 &
                                   num_offerte_totali_ok > 1] /
        num_offerte_totali_ok[num_offerte_ammesse_ok == 1 &
                                num_offerte_totali_ok > 1],
      na.rm = TRUE
    ),
    
    # 14. Estensione del periodo di pubblicazione del bando (tra pubblicazione del bando e data di
    # scadenza sottomissione proposte):
    mean_diff_date1 = as.numeric(mean(data_scadenza_offerta - data_pubblicazione, na.rm = T)),
    
    # 15. Estensione del periodo di valutazione dell’offerta (tra data offerta e data aggiudicazione)
    # per procedura di gara
    mean_diff_date2 = as.numeric(mean(data_aggiudicazione_definitiva - data_scadenza_offerta, na.rm = T)),
    
  ) %>% 
  
  # Attacco la flag 13
  left_join(red_flag_13, by = "CF_stazione") %>% 
  rename(prop_contr_stessa_azienda = O) %>% 
  relocate(prop_contr_stessa_azienda, .before = mean_diff_date1) %>%
  
  
  # Aggiungo le dummy per indicare se la flag i-esima è calcolabile
  mutate(
    # 1b. Offerta economicamente più vantaggiosa: calcolabile?
    calcolabile1 = ifelse(is.na(offerta_econo_vant), 0, 1),
    
    # 2b. Numero delle procedure non aperte
    calcolabile2 = ifelse(is.na(num_proc_non_aperte), 0, 1),
    
    # 3b. Valore delle procedure non aperte
    calcolabile3 = ifelse(is.na(val_proc_non_aperte), 0, 1),
    
    # 4b. Contratti aggiudicati e modificati per effetto di almeno una variante
    calcolabile4 = ifelse(is.na(contr_almeno_una_var), 0, 1),
    
    # 5b. Scostamento dei costi di esecuzione
    calcolabile5 = ifelse(is.na(scostamento_costi) | scostamento_costi == Inf, 0, 1),
    
    # 7b. Inadempimento delle comunicazioni di aggiudicazione
    calcolabile7 = ifelse(is.na(inademp_comun_aggiudicazione), 0, 1),
    
    # 9b. Offerta singola
    calcolabile9 = ifelse(is.na(offerta_singola), 0, 1),
    
    # 10b. Proporzione di offerte escluse
    calcolabile10 = ifelse(is.na(prop_offerte_escluse) | 
                             prop_offerte_escluse == -Inf, 0, 1),
    
    # 11b. Esclusione di tutte le offerte tranne una
    calcolabile11 = ifelse(is.na(escl_tutte_tranne_una), 0, 1),
    
    # 12b. Proporzione di offerte escluse in procedure con tutte le offerte 
    # escluse tranne una
    calcolabile12 = ifelse(is.na(prop_offerte_escluse_12) |
                             prop_offerte_escluse_12 == -Inf, 0, 1),
    
    # 13b. Proporzione di contratti aggiudicati alla stessa azienda
    calcolabile13 = ifelse(is.na(prop_contr_stessa_azienda), 0, 1),
    
    # 14b. Estensione del periodo di pubblicazione del bando (tra pubblicazione del bando e data di
    # scadenza sottomissione proposte):
    calcolabile14 = ifelse(is.na(mean_diff_date1), 0, 1),
    
    # 15b. Estensione del periodo di valutazione dell’offerta (tra data offerta e data aggiudicazione)
    # per procedura di gara
    calcolabile15 = ifelse(is.na(mean_diff_date2), 0, 1),
    
  ) %>% 
  # Numero totale di red flag calcolabili
  mutate(calcolabile_tot = sum(across(calcolabile1:calcolabile15))) %>% 
  ungroup()


# Percentuale di dati mancanti/infiniti per variabile
red_flag_stazioni %>% 
  select(-starts_with("calcolabile")) %>% 
  select(-c(CF_stazione, denominazione)) %>% 
  apply(2, function(x) 
    round(sum(is.na(x) | is.infinite(x)) / length(x) * 100, 2)
  ) %>% 
  enframe(name = "variabile", value = "percentuale") %>% 
  print(n=30)


hist(red_flag_stazioni$num_proc_non_aperte, xlim=c(0,1), nclass=50)


# Sistemazione dati mancanti/problematici
red_flag_stazioni = red_flag_stazioni %>% 
  # Esclusione variabili
  select(-c(prop_offerte_escluse_12, num_proc_non_aperte),
         -c(calcolabile1:calcolabile15)) %>%  
  # Risoluzione dei dati mancanti/problematici
  mutate(
    num_appalti_diretti = coalesce(num_appalti_diretti, 0),
    val_tot = coalesce(val_tot, 0),
    prop_date_contrarie = coalesce(prop_date_contrarie, 0),
    
    offerta_econo_vant = coalesce(offerta_econo_vant, 0),
    val_proc_non_aperte = coalesce(val_proc_non_aperte, 0),
    contr_almeno_una_var = coalesce(contr_almeno_una_var, 0),
    scostamento_costi = ifelse(is.na(scostamento_costi) | scostamento_costi == Inf |
                                 scostamento_costi < 0,
                               1, scostamento_costi),
    offerta_singola = coalesce(offerta_singola, 0),
    prop_offerte_escluse = ifelse(is.na(prop_offerte_escluse) |
                                    prop_offerte_escluse == -Inf,
                                  0, prop_offerte_escluse),
    escl_tutte_tranne_una = coalesce(escl_tutte_tranne_una, 0),
    prop_contr_stessa_azienda = coalesce(prop_contr_stessa_azienda, median(prop_contr_stessa_azienda, na.rm=T)),
    mean_diff_date1 = coalesce(mean_diff_date1, median(mean_diff_date1, na.rm=T)),
    mean_diff_date2 = coalesce(mean_diff_date2, median(mean_diff_date2, na.rm=T))

  )

glimpse(red_flag_stazioni)

# Numero di casi in cui le date sono contrarie
sum(red_flag_stazioni$mean_diff_date1 < 0)
sum(red_flag_stazioni$mean_diff_date2 < 0)




# Rinomino i casi con CF_stazione e denominazione mancanti per evitare crei problemi l'essere un NaN
red_flag_stazioni[1,]$CF_stazione = "CF_stazione mancante"
red_flag_stazioni[1,]$denominazione = "denominazione mancante"


write.csv(red_flag_stazioni, "dati/red_flag_stazioni_2011_2025.csv", row.names = FALSE)

# red_flag_stazioni = read.csv("red_flag_stazioni_2011_2025.csv")



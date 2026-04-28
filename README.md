# analisi-corruzione-bandi
Questo lavoro, svolto da [@Luca255](https://github.com/Luca255) e [@DakoDC](https://github.com/DakoDC) per il [Laboratorio di statistica con le aziende](https://www.stat.unipd.it/laboratorio-di-statistica-con-le-aziende-tre-i-premi-di-studio-palio) 2025/26 dell'Università di Padova, si propone di provare a costruire un ranking delle stazioni appaltanti italiane basato sul loro possibile rischio di corruzione, sfruttando i dati provenienti dall'ANAC (Autorità Nazionale Anticorruzione) e tramite metodi statistici e di machine learning per l'anomaly detection non supervisionata.

Di seguito sono riportati i procedimenti e le informazioni necessarie per riproducibilità e avviamento del codice. Inoltre, nel file Report.pdf, è presente una descrizione dell'intera analisi svolta.

## Riproducibilità  
Per ottenere gli stessi risultati eseguire i codici con Python 3.13.12 e assicurarsi che le versioni dei pacchetti corrispondano a quelle in requirements.txt.

## Procedimento  
Vengono qui brevemente descritte le fasi da svolgere per riprodurre per l'analisi:  
- Avviare il codice di download_bando_cig.py, il quale scarica in modo automatico nella cartella selezionata tutti i Dataset relativi al Bando CIG dal sito dell'ANAC nel periodo di anni voluto.  
- Scaricare a mano i restanti singoli csv dal sito dell'ANAC necessari per l'analisi: Aggiudicatari, Aggiudicazioni, Bando CIG, Fine Contratto, Partecipanti, Quadro Economico, Stazione Appaltante e Varianti; salvandoli in una cartella ANAC_datasets.  
- Avviare il codice ANAC_datasets_merge.R il quale a partire dai file scaricati, svolge del preprocessing e unione di questi csv in un unico Dataset dati_2011_2025.zip, disponibile a questo [link MEGA](https://mega.nz/file/tB0TgaYL#b2XK1FDEYqSRQkezH7hprfmCKY7IFBByiTo_d33dFQo).  
- Tramite questo file dati, avviare il codice calcolo_red_flag.R che creerà a sua volta il Dataset red_flag_stazioni_2011_2025.csv, contenente gli indicatori per ogni stazione appaltante (unico file necessario per l'analisi successiva).  
- Adesso si può svolgere l'analisi tramite il codice in analisi_appalti_ANAC.ipynb, tramite il quale si svolge l'analisi tramite l'autoencoder e l'isolation forest. Facciamo notare che essendo l'analisi divisa per tipologie di stazioni appaltanti [comuni, scuole, altro], nel codice sarà necessario cambiare la variabile corrispondente per ottenere il ranking finale della tipologia desiderata, il quale è usato anche per creare la mappa delle province italiane tramite il codice in mappa.R .  

## Altro  
Nella repository è anche presente il file modificato relativo all'uso di un metodo SHAP per l'autoencoder, che però non si è rivelato sufficientemente informativo, ma lasciamo il codice per completezza.

## Disclaimer   
Per questioni di privacy il file del ranking e altre informazioni potenzialmente sensibili nei codici sono state tolte o non sono presenti nella repository.

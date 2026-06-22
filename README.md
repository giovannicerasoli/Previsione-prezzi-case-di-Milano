# Milan Housing Price Prediction

Questo progetto ha l’obiettivo di stimare il prezzo di vendita degli immobili situati nel comune di Milano attraverso tecniche di analisi statistica e machine learning.

## Descrizione del dataset

Il dataset contiene informazioni relative a **12.800 immobili in vendita** nel comune di Milano.

I dati sono suddivisi in:

* **Training set**: 8.000 osservazioni, comprensive della variabile target `selling_price`;
* **Test set**: 4.800 osservazioni, utilizzate per generare le previsioni finali.

Il dataset originale era composto da 16 variabili. La colonna `other_features`, contenente più caratteristiche dell’immobile, è stata trasformata in un insieme di variabili dummy.

Dopo le attività di pulizia, imputazione e feature engineering, il dataset finale è stato ridotto e riorganizzato in **27 variabili**, selezionate per mantenere le informazioni più rilevanti e ridurre la ridondanza.

## Data Cleaning e imputazione dei valori mancanti

Il dataset presentava valori mancanti e alcune osservazioni anomale. Per gestirli sono state applicate strategie di imputazione differenziate in base alla natura delle variabili.

* La variabile `square_meters` presentava alcuni valori anomali, inferiori a 12 mq. Tali osservazioni sono state reimputate attraverso una media condizionata a due livelli, basata sul numero di stanze e sul numero di bagni.

* I valori mancanti nelle variabili `bathrooms_number`, `conditions` e `total_floors_in_building` sono stati imputati mediante regressione logistica ordinale (`polr`).

* Le variabili binarie o categoriali, tra cui `lift`, `heating_centralized` ed `energy_efficiency_class`, sono state imputate tramite modelli di regressione logistica binaria oppure ordinale, a seconda della tipologia della variabile.

* I valori mancanti di `condominium_fees`, così come i valori anomali superiori a 11.000, sono stati sostituiti mediante medie condizionate rispetto ad altre caratteristiche strutturali dell’immobile.

* Anche i valori mancanti di `year_of_construction` sono stati imputati attraverso medie condizionate basate su variabili strutturali e territoriali.

* L’unico valore mancante nella variabile `zone` è stato assegnato a una categoria territoriale con caratteristiche immobiliari simili.

## Feature Engineering

Per aumentare la capacità predittiva dei modelli sono state create nuove variabili e aggregate alcune caratteristiche fortemente correlate.

* **`distance`**: misura la distanza tra la zona dell’immobile e le coordinate del Duomo di Milano.

* **`zone_cluster_hc`**: raggruppa le zone della città in 20 cluster ottenuti attraverso un algoritmo di clustering gerarchico.

* **`security_level`**: variabile ordinale costruita aggregando caratteristiche legate alla sicurezza dell’immobile, come sistema di allarme, videocitofono e porta blindata.

* **`luxury_level`**: variabile ordinale che sintetizza la presenza di caratteristiche rare o di pregio, come piscina, campo da tennis o idromassaggio.

Inoltre, diverse variabili simili sono state aggregate per ridurre la dimensionalità del dataset. Ad esempio, le diverse tipologie di giardino sono state riassunte nella variabile `garden`, mentre le caratteristiche relative a balconi e terrazzi sono state sintetizzate nella variabile `balcony/terrace`.

## Scelta del modello

L’analisi esplorativa ha evidenziato che l’applicazione di una trasformazione logaritmica alla variabile target `selling_price` migliora sensibilmente la distribuzione dei dati, rendendo la relazione con le principali variabili esplicative più lineare e riducendo i problemi di eteroschedasticità.

In particolare, è emersa una forte relazione lineare tra:

```r
log(selling_price)
```

e:

```r
log(square_meters)
```

Tra le variabili risultate maggiormente significative nella spiegazione del prezzo figurano inoltre:

* `bathrooms_number`;
* `zone_cluster_hc`;
* `square_meters`;
* le principali caratteristiche strutturali e qualitative dell’immobile.

Sono stati confrontati diversi modelli, tra cui:

* Ridge Regression;
* Lasso Regression;
* Generalized Additive Model (GAM);
* regressione lineare multipla (`lm`).

Il modello di regressione lineare multipla ha ottenuto un Mean Absolute Error sul validation set pari a:

```r
MAE = 76937.59
```

Il risultato è molto vicino a quello ottenuto dal modello GAM:

```r
MAE = 76222.77
```

e superiore alle prestazioni registrate dai modelli Ridge Regression testati durante l’analisi.

## Conclusioni

Per le previsioni finali è stato selezionato un modello di regressione lineare multipla (`lm`).

Nonostante il GAM abbia ottenuto un errore leggermente inferiore, la differenza in termini di MAE risulta marginale. La regressione lineare offre invece vantaggi rilevanti in termini di interpretabilità, semplicità di implementazione e capacità di leggere economicamente l’effetto delle variabili sul prezzo degli immobili.

La scelta del modello lineare è stata inoltre supportata dalla verifica delle principali ipotesi del modello, risultate complessivamente adeguate dopo la trasformazione logaritmica della variabile target.

Il progetto mostra quindi come, attraverso un’attenta fase di data cleaning, imputazione e feature engineering, un modello lineare relativamente semplice possa raggiungere buone performance nella previsione dei prezzi immobiliari nel mercato milanese.


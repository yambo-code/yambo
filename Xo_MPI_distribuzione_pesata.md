# Distribuzione MPI pesata delle bande in Xo

## Sintesi

`PARALLEL_index` contiene già un algoritmo generico per distribuire indici in modo non contiguo e pesato. Questo meccanismo, tuttavia, non è attualmente attivato per le bande di conduzione e valenza di Xo e non calcola automaticamente il costo derivante dai raggruppamenti `[X-CG]`.

## Ruolo di `CONSECUTIVE`

In `src/parallel/PARALLEL_index.F`, l'argomento opzionale `CONSECUTIVE` seleziona due modalità:

- `CONSECUTIVE=.TRUE.` assegna a ogni rank un intervallo contiguo ed equinumeroso di indici;
- `CONSECUTIVE=.FALSE.` usa una distribuzione non contigua che cerca di bilanciare la somma dei pesi assegnati agli indici.

Se `px%weight_1D` non è già allocato, il ramo non contiguo assegna automaticamente peso unitario a ogni indice:

```fortran
px%weight_1D=1
```

In questo caso si ottiene una distribuzione non contigua ed equinumerosa, ma non informata dal costo effettivo delle bande.

Se invece il chiamante prepara `px%weight_1D`, `PARALLEL_index` ordina gli elementi in base al peso e li assegna progressivamente al rank con il carico accumulato minore. È quindi già disponibile un algoritmo greedy di bilanciamento pesato.

## Situazione attuale in Xo

In `src/parallel/PARALLEL_global_indexes.F`, le chiamate relative alle bande di conduzione e valenza di Xo specificano esplicitamente:

```fortran
CONSECUTIVE=.TRUE.
```

Di conseguenza:

- `X_and_IO_CPU` controlla quanti rank appartengono ai livelli `q`, `g`, `k`, `c` e `v`;
- `X_and_IO_ROLEs` assegna il significato dei livelli della gerarchia MPI;
- nessuna variabile del file di input permette attualmente di scegliere la modalità consecutiva o non consecutiva;
- `PAR_IND_CON_BANDS_X%weight_1D` non viene popolato con una stima del costo delle bande.

Per modificare questo comportamento è quindi necessario intervenire sul sorgente e ricompilare.

## Rapporto con i raggruppamenti `[X-CG]`

L'infrastruttura esistente permette di utilizzare pesi già noti, ma non determina tali pesi dai raggruppamenti delle energie di transizione.

Quando viene eseguito `PARALLEL_global_indexes`, le bande non sono ancora state assegnate e i gruppi energetici non sono ancora stati costruiti. `coarse_grid_N` e `bare_grid_N` vengono calcolati successivamente e localmente, dopo l'applicazione delle maschere MPI. Il bilanciatore non può quindi usare direttamente i gruppi effettivi senza una fase preliminare o una riorganizzazione del flusso di calcolo.

## `FREQUENCIES_coarse_grid` e la soglia `tresh`

In `X_irredux`, ogni rank costruisce dapprima il proprio vettore locale `X_poles`, contenente le energie delle sole transizioni selezionate dalle sue maschere MPI. Questo vettore viene poi passato a `FREQUENCIES_coarse_grid`:

```fortran
call FREQUENCIES_coarse_grid('X',X_poles,n_poles,X%cg_percentual, &
&                            X_Ein_poles,l_X_terminator)
```

In `src/common/FREQUENCIES_coarse_grid.F`, la soglia iniziale è:

```fortran
real(SP), parameter :: default_treshold=1.E-5
```

La routine ordina le energie locali e confronta ogni energia con quella di riferimento del gruppo corrente. Un nuovo gruppo viene aperto quando la separazione supera `tresh`:

```fortran
df=bg_sorted(i_bg)-bg_sorted(i_bg_ref)
if (abs(df)>tresh) then
  i_cg=i_cg+1
  i_bg_ref=i_bg
endif
```

Quando è attivo il terminatore viene controllata anche la differenza fra le energie degli stati iniziali tramite `tresh_ini`.

La soglia di partenza è uguale su tutti i rank, ma il raggruppamento è locale perché gli array di energie in ingresso sono diversi. Inoltre, in funzione di `cg_percentual`, `tresh` può essere aggiornata usando la distribuzione locale delle differenze `bg_diffs`. Rank diversi possono quindi avere differenti:

- insiemi di energie di transizione;
- distribuzioni delle separazioni energetiche;
- valori effettivi di `tresh` durante il procedimento adattivo;
- numeri e dimensioni dei gruppi finali;
- valori di `coarse_grid_N` e `bare_grid_N`.

Questa località spiega perché lo stesso criterio numerico non produca lo stesso numero di gruppi sui diversi rank.

## Possibili livelli di intervento

### 1. Distribuzione non contigua con pesi unitari

La prova più semplice consiste nel sostituire, per le bande `c` di Xo:

```fortran
CONSECUTIVE=.TRUE.
```

con:

```fortran
CONSECUTIVE=.FALSE.
```

Questo mescolerebbe bande basse e alte fra i rank, pur mantenendo approssimativamente lo stesso numero di bande per rank. Nel caso analizzato potrebbe attenuare l'andamento monotono del costo, ma non garantirebbe il bilanciamento perché non utilizza `coarse_grid_N`.

Prima di adottare questa modalità occorre verificare che l'intero percorso Xo gestisca correttamente maschere di bande non contigue; il percorso corrente è configurato esplicitamente per blocchi contigui.

### 2. Distribuzione con pesi stimati per banda

Prima della chiamata a `PARALLEL_index` si potrebbe allocare e riempire:

```fortran
PAR_IND_CON_BANDS_X(X_type)%weight_1D
```

con una stima del costo di ogni banda. Il ramo `CONSECUTIVE=.FALSE.` userebbe quindi automaticamente questi pesi.

Possibili stime includono:

- numero previsto di energie di transizione distinte per banda;
- numero di gruppi ottenuto mediante una fase preliminare;
- costo misurato in un run precedente;
- combinazione del numero di transizioni grezze e del numero di gruppi.

### 3. Raggruppamento globale delle transizioni

È tecnicamente possibile costruire una coarse grid globale. Una possibile sequenza sarebbe:

1. costruire le transizioni locali su ciascun rank;
2. raccogliere globalmente le energie `X_poles` e i relativi descrittori;
3. ordinare l'insieme globale delle energie;
4. applicare una sola volta `FREQUENCIES_coarse_grid`, o una sua variante distribuita;
5. assegnare i gruppi globali risultanti ai rank.

In questo modo `tresh`, l'ordinamento e i confini dei gruppi sarebbero definiti rispetto alla distribuzione globale delle energie, anziché separatamente per ogni blocco locale di bande.

Il cambiamento non è però limitato alla sola chiamata di raggruppamento. Un gruppo globale può contenere transizioni che, nella distribuzione originale, appartengono a rank diversi. Occorre quindi scegliere come calcolare il suo residuo.

Le principali strategie possibili sono:

- **Ridistribuzione completa dei gruppi:** ogni gruppo viene assegnato a un rank, trasferendo anche i descrittori e rendendo disponibili le funzioni d'onda necessarie. Offre unità di lavoro globali esplicite, ma modifica la località dei dati e può aumentare memoria e comunicazioni.
- **Residui parziali:** le transizioni restano sui rank originari; ogni rank calcola la propria parte del gruppo globale e i contributi vengono combinati tramite riduzioni MPI. Evita di spostare le funzioni d'onda, ma può richiedere sincronizzazioni o riduzioni molto più frequenti.
- **Raggruppamento globale usato solo per stimare i pesi:** una fase preliminare determina un costo per banda o per insieme di bande; tali pesi alimentano poi `PARALLEL_index` e il calcolo principale continua con gruppi locali. È meno invasiva, ma il bilanciamento rimane approssimato.

La raccolta completa di tutte le transizioni su ogni rank tramite `MPI_Allgatherv` sarebbe concettualmente semplice, ma avrebbe costi di memoria e comunicazione proporzionali al numero globale di transizioni. Per problemi grandi sarebbe preferibile raccogliere i dati su un solo rank, usare un ordinamento distribuito oppure costruire direttamente statistiche compatte per stimare i pesi.

Un raggruppamento globale renderebbe disponibile una definizione uniforme dei gruppi e consentirebbe di distribuire direttamente le unità di lavoro effettive. Non garantirebbe tuttavia da solo un miglioramento: il vantaggio deve essere confrontato con il costo delle comunicazioni, l'accesso non locale agli oscillatori e alle funzioni d'onda, la dimensione delle GEMM risultanti e la strategia della riduzione finale di Xo.

## Conclusione

Yambo possiede già il contenitore `weight_1D` e l'algoritmo greedy necessario per una distribuzione pesata e non contigua. Per Xo manca però il collegamento applicativo: la modalità è forzata a contigua, i pesi non vengono calcolati e non esiste un'opzione di input per attivarla.

La coarse grid viene attualmente costruita localmente a partire da insiemi diversi di energie; anche l'eventuale adattamento di `tresh` dipende quindi dai dati locali. Una distribuzione realmente guidata da `[X-CG]` richiederebbe una stima preliminare del costo oppure un raggruppamento globale. La prima soluzione conserva maggiormente l'organizzazione attuale, mentre la seconda definisce unità di lavoro globali più fedeli ma richiede una revisione sostanziale della proprietà dei dati e delle comunicazioni MPI.

# Proposta: prepass per bande e partizione contigua pesata

## Obiettivo e separazione dall'analisi

Questa sezione descrive una possibile modifica futura del codice. Non fa parte dell'analisi del comportamento attuale e non implica che tale algoritmo sia già disponibile in Yambo.

L'obiettivo è conservare blocchi contigui di bande di conduzione, assegnando però a ciascun rank un numero potenzialmente diverso di bande. I confini dei blocchi verrebbero scelti in modo da rendere approssimativamente uniforme un costo stimato mediante un prepass.

La proposta evita di distribuire direttamente i gruppi globali e lascia invariati il calcolo principale di Xo, la costruzione dei residui, le GEMM e la riduzione finale.

## Punto d'inserimento nel flusso

In `X_dielectric_matrix.F`, la struttura parallela viene costruita mediante:

```fortran
call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space_and_IO",X=X)
```

Solo successivamente le funzioni d'onda vengono distribuite usando le maschere delle bande:

```fortran
call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz, &
&                           B_index=PAR_IND_CON_BANDS_X(X%whoami), &
&                           Bp_index=PAR_IND_VAL_BANDS_X(X%whoami), &
&                           CLEAN_UP=.TRUE.)
```

`PARALLEL_global_indexes` riceve già le energie e le occupazioni elettroniche in `E`, le griglie `Xk` e `q` e i parametri della risposta in `X`. Esiste quindi un punto del flusso nel quale è possibile stimare il costo delle bande prima di distribuire le funzioni d'onda e prima di fissare definitivamente la maschera `c`.

L'effettiva disponibilità, in questo punto, di tutte le mappature necessarie per ogni q-point dovrà comunque essere verificata durante l'implementazione.

## Prepass per la stima del peso

Per ogni banda di conduzione `ic`, una routine preliminare potrebbe:

1. generare le energie di transizione associate alla banda;
2. applicare gli stessi criteri di accettazione usati da `X_eh_setup`;
3. ordinare le energie accettate;
4. applicare una versione leggera del criterio di raggruppamento di `FREQUENCIES_coarse_grid`;
5. produrre un peso scalare per la banda.

Una prima definizione potrebbe essere:

```text
weight(ic) = numero stimato di gruppi della banda ic
```

Una stima più generale potrebbe includere anche il costo delle transizioni grezze:

```text
weight(ic) = alpha * numero_transizioni(ic)
           + beta  * numero_gruppi_stimati(ic)
```

Nel run analizzato il numero di transizioni grezze è identico fra i rank, mentre il numero di gruppi varia sensibilmente. Il termine associato ai gruppi sarebbe quindi il candidato principale per un primo prototipo.

## Conteggio dei gruppi senza effetti collaterali

Non è opportuno chiamare direttamente `FREQUENCIES_coarse_grid` una volta per ogni banda, perché la routine prepara strutture globali quali `coarse_grid_N` e `bare_grid_N`, effettua allocazioni ed emette i messaggi `[X-CG]`.

È preferibile introdurre una funzione priva di effetti collaterali, per esempio:

```fortran
integer function FREQUENCIES_count_groups(energies,n,tresh)
```

La funzione dovrebbe ordinare o ricevere ordinate le energie e restituire soltanto il numero di gruppi secondo lo stesso criterio numerico usato dalla coarse grid. Per evitare divergenze future, la logica elementare di conteggio dovrebbe essere condivisa con `FREQUENCIES_coarse_grid`, invece di essere duplicata in modo indipendente.

## Costruzione dei blocchi contigui

Una volta calcolato il vettore ordinato:

```text
weight(c_min:c_max)
```

si determina il peso obiettivo:

```text
target = sum(weight) / numero_rank_c
```

I confini vengono scelti lungo l'ordine naturale delle bande affinché il peso cumulativo di ogni intervallo sia vicino a `target`:

```text
rank 0: c_min      ... b0
rank 1: b0+1       ... b1
rank 2: b1+1       ... b2
...
rank P: b(P-1)+1   ... c_max
```

Il numero di bande può quindi differire fra i rank, mentre ciascun rank continua a possedere un unico intervallo contiguo.

La routine di partizione deve inoltre:

- garantire almeno una banda per rank quando richiesto;
- compilare coerentemente `element_1D`, `first_of_1D`, `last_of_1D` e `n_of_elements`;
- produrre gli stessi confini su tutti i rank del comunicatore `c`;
- gestire pesi nulli, bande escluse e casi con meno bande che rank.

Per un prototipo è sufficiente scegliere i confini più vicini alle frazioni cumulative del peso totale. Una versione successiva potrebbe risolvere il problema di partizione lineare che minimizza il massimo peso assegnato a un rank.

## Trattamento dei q-point

Il numero di gruppi associato a una banda dipende dal q-point. Sono possibili diverse strategie:

- usare soltanto `q=1`, minimizzando il costo del prepass;
- usare un piccolo insieme rappresentativo di q-point;
- sommare o mediare la stima su tutti i q-point;
- leggere pesi o statistiche ottenuti da un run precedente.

Nel run analizzato l'andamento del numero di gruppi rispetto ai blocchi di bande è sistematico su tutti i q-point. Questo suggerisce che un campione limitato potrebbe essere sufficiente per un primo esperimento, ma la scelta deve essere verificata su sistemi differenti.

Cambiare i confini a ogni q-point offrirebbe una maggiore adattività, ma complicherebbe la distribuzione e il riuso delle funzioni d'onda. La proposta iniziale prevede quindi una sola partizione, costruita da pesi aggregati o campionati e mantenuta per l'intero calcolo Xo.

## Limite della stima per banda

Il numero di gruppi non è una quantità perfettamente additiva:

```text
Ngruppi(unione di più bande)
  != somma esatta di Ngruppi delle singole bande
```

Transizioni provenienti da bande differenti possono infatti confluire nello stesso gruppo energetico. Il peso del prepass rappresenta quindi una stima del costo del blocco e non una previsione esatta del successivo `coarse_grid_N` locale.

Per migliorare la stima senza costruire una coarse grid globale completa si potrebbero usare istogrammi energetici compatti oppure valutare direttamente alcuni intervalli candidati attorno ai confini preliminari.

## Modifiche previste

Una prima implementazione potrebbe rimanere circoscritta a:

1. una routine per stimare il peso delle bande;
2. una funzione di conteggio dei gruppi senza effetti collaterali;
3. una routine per la partizione contigua pesata;
4. una modifica in `PARALLEL_global_indexes.F` per installare le nuove maschere `c`;
5. una variabile di input che permetta di scegliere esplicitamente il nuovo comportamento e conservi come default la distribuzione contigua equinumerosa.

Non sarebbe necessario modificare:

- `X_irredux_residuals`;
- il batching delle GEMM;
- il loop principale sui gruppi;
- la riduzione finale di Xo;
- la proprietà delle transizioni durante il calcolo principale.

## Valutazione della proposta

Il prepass seguito da una partizione contigua pesata è meno invasivo del raggruppamento globale con redistribuzione diretta dei gruppi. Conserva la località delle bande e delle funzioni d'onda e interviene principalmente nella fase di inizializzazione della distribuzione MPI.

Il metodo non garantisce lo stesso numero finale di gruppi su ogni rank, ma può essere efficace anche con una stima imperfetta, purché i pesi riproducano l'andamento relativo del costo lungo le bande. Prima di considerarlo definitivo sarà necessario misurare separatamente il costo del prepass, la qualità del bilanciamento di `coarse_grid_N`, i tempi `Xo (procedure)` e l'attesa in `Xo (REDUX)`.

## Interfaccia e comportamento implementati

La prima implementazione è attivata dal flag di input:

```text
X_WeightedBands
```

Il flag appartiene alle opzioni parallele, è disabilitato per default e viene
letto soltanto da `X_dielectric_matrix`; `OPTICS_driver` resta quindi invariato.
Con il flag assente, la chiamata storica a `PARALLEL_global_indexes` continua a
produrre gli stessi blocchi contigui equinumerosi e non viene eseguito alcun
prepass.

Con il flag presente, più di un rank `c` e almeno un q-point pendente assegnato
al gruppo `q`, `X_weighted_bands_setup` viene eseguita dopo la verifica dei
database e la costruzione ordinaria degli indici, ma prima di
`PARALLEL_WF_distribute`. La routine:

- conserva le maschere `q`, `k` e `v` e ignora soltanto la maschera `c`
  provvisoria;
- assegna ciclicamente le bande del prepass ai rank `c`;
- per ogni banda e ogni q-point pendente riproduce occupazioni, finestra
  energetica e terminatore di `X_eh_setup`;
- moltiplica il contributo di `q=1` per il numero effettivo di direzioni
  ottiche;
- conta i gruppi con `FREQUENCIES_group_engine`, lo stesso nucleo privo di
  effetti collaterali ora usato dal wrapper `FREQUENCIES_coarse_grid`;
- somma pesi e transizioni accettate con riduzioni MPI intere a 64 bit;
- sostituisce esclusivamente `PAR_IND_CON_BANDS_X(X%whoami)` con la partizione
  contigua minimax.

`PARALLEL_index_weighted_contiguous` trova il minimo carico massimo mediante
ricerca binaria e ricostruisce deterministicamente gli intervalli. In caso di
peso totale nullo riproduce la distribuzione equinumerosa storica. Quando le
bande sono meno dei rank, assegna una banda ai primi rank utili e segnala i rank
eccedenti. I campi `element_1D`, `first_of_1D`, `last_of_1D` e
`n_of_elements` contengono rispettivamente maschera locale, limiti globali e
numero reale di bande.

Il report `[X-WB]` mostra intervallo, numero di bande, peso previsto e numero di
transizioni accettate del rank. Il costo è registrato separatamente come
`Xo weighted bands prepass` nei timer.

## Correzione dopo T01: peso marginale fra bande adiacenti

T01 ha mostrato che la somma dei gruppi ottenuti da ciascuna banda isolata non
è una stima utile del costo di un blocco: le transizioni di bande vicine si
sovrappongono energeticamente e confluiscono negli stessi gruppi. I pesi
isolati risultavano quasi uniformi e riproducevano quindi quasi esattamente la
partizione equinumerosa, mentre i gruppi del calcolo effettivo diminuivano
sistematicamente procedendo verso le bande alte.

Il prepass usa ora un'approssimazione marginale locale. Per ogni banda `ic`
calcola, allo stesso q-point, sia i gruppi della coppia `(ic-1,ic)` sia quelli
della sola banda `ic-1`, e assegna:

```text
weight(ic) = max(Ngruppi(ic-1 U ic) - Ngruppi(ic-1), 0)
```

Per la prima banda resta `weight(ic_low)=Ngruppi(ic_low)`. I contributi sono
poi sommati sui q-point come in precedenza. Questa stima include la perdita di
lavoro dovuta alla sovrapposizione locale senza rendere necessario costruire o
raccogliere l'intero insieme globale delle transizioni. Rimane
un'approssimazione: non rappresenta sovrapposizioni fra bande non adiacenti né
la piena dipendenza del raggruppamento adattivo dall'intero intervallo.

## Diagnostica delle partizioni specifiche per q-point

Il prepass conserva una sola partizione statica per il calcolo produttivo, ma
calcola anche la partizione contigua minimax stimata separatamente per ciascun
q-point. Le righe `[X-WB-Q]` riportano, per ogni q e c-rank, intervallo e carico
marginale previsto. Un riepilogo confronta inoltre il massimo carico marginale
della partizione specifica con quello prodotto, sullo stesso q, dalla
partizione statica e ne indica la riduzione percentuale teorica.

Questa diagnostica non modifica le maschere produttive e non ridistribuisce le
funzioni d'onda. Serve a misurare quanto cambierebbero i confini specifici per
q e a stimare se il possibile beneficio giustifichi in futuro il costo e la
complessità di una redistribuzione delle funzioni d'onda durante il q-loop.

## Diagnostica interval-aware successiva a T02

Poiché T02 ha dimostrato che anche i marginali fra bande adiacenti non
rappresentano il costo di blocchi di centinaia di bande, il prepass misura ora
anche il costo non additivo di intervalli completi. Questa parte resta
strettamente diagnostica e non sostituisce la maschera produttiva ottenuta dai
pesi marginali.

La sequenza è:

1. contare con `FREQUENCIES_group_engine` i gruppi dell'intero intervallo
   statico assegnato a ciascun c-rank, per ogni q-point;
2. assegnare temporaneamente a ciascuna banda la densità media osservata nel
   proprio intervallo, cioè gruppi esatti aggregati divisi per numero di bande;
3. costruire da questa approssimazione piecewise-constant una partizione
   proposta con lo stesso partizionatore contiguo minimax;
4. ricalcolare i gruppi degli interi intervalli proposti, senza usare somme di
   pesi per-banda;
5. riportare per ogni q e rank i costi esatti statici e proposti, i massimi per
   q e i totali aggregati.

Le righe sono marcate `[X-WB-I]`. Il costo dei due conteggi completi è
registrato separatamente dal resto del prepass nel timer
`Xo weighted interval diagnostic`. La partizione proposta non viene passata a
`PARALLEL_WF_distribute`: serve soltanto a verificare se il forte beneficio
indicato dall'oracle offline sopravvive al ricalcolo non additivo dei gruppi.

## Raffinamento diagnostico interval-aware

Dopo la prima validazione sugli intervalli completi, il diagnostico esegue una
sola iterazione aggiuntiva. La densità piecewise-constant viene ricostruita dai
costi esatti della prima proposta, anziché da quelli della partizione marginale;
il partizionatore contiguo minimax genera quindi confini raffinati e
`FREQUENCIES_group_engine` ne misura nuovamente i costi completi.

Le righe `[X-WB-R]` confrontano proposta e raffinamento per q-point, riportano
confini e costi aggregati e indicano se il massimo aggregato esatto è realmente
diminuito. Il raffinamento è considerato accettato soltanto in quest'ultimo
caso. Questa seconda iterazione resta diagnostica: né la prima proposta né i
confini raffinati modificano `PAR_IND_CON_BANDS_X` o vengono passati a
`PARALLEL_WF_distribute`.

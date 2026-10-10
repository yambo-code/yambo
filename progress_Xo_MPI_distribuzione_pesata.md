# Progressi: distribuzione MPI contigua pesata delle bande di Xo

Ultimo aggiornamento: 2026-10-11.

## Stato Git iniziale

- Base: `5.4` a `a4a1dfaa9800917b36bd5aee613d27ff99c60c73`.
- Branch: `tech-xo-weighted-band-distribution`.
- Worktree iniziale: tre documenti di analisi non tracciati:
  `Xo_MPI_analisi_livello_c.md`, `Xo_MPI_distribuzione_pesata.md` e
  `Xo_MPI_investigation_c_level.md`; nessuna modifica a file tracciati.

## Attività completate

- Verificata la base Git e creato il branch di lavoro.
- Acquisiti nel piano i requisiti d'interfaccia, implementazione, verifica e
  scambio dei test Leonardo-Booster.
- Individuati i primi punti d'integrazione: `X_dielectric_matrix`,
  `X_eh_setup`, `FREQUENCIES_coarse_grid` e `PARALLEL_global_indexes`.
- Implementata l'opzione parallela `X_WeightedBands`, default `.false.`.
- Estratto `FREQUENCIES_group_engine` e collegato al wrapper X-CG esistente.
- Implementati prepass su q-point pendenti, riduzione a 64 bit e riepilogo
  `[X-WB]`.
- Implementata la partizione contigua minimax con fallback a peso nullo e rank
  eccedenti.
- Mantenuto invariato `OPTICS_driver` e condizionato l'intero nuovo percorso al
  flag, a più di un rank `c` e alla presenza di q-point locali pendenti.

## Build e verifiche locali

- Configurazione rilevata: MPI+OpenMP+ScaLAPACK+SLEPc+HDF5 parallel I/O.
- `module load profile/gcc-14.3.0 && make yambo`: completato; link e avvio di
  `bin/yambo -h` riusciti.
- La prima invocazione senza profilo è fallita con `mpifort: not found`; non era
  un errore sorgente. Una successiva invocazione concorrente di `make core` ha
  lasciato un oggetto dipoli incompleto; una build pulita e serializzata del
  solo `yambo` ha risolto il problema.
- Test sintetico esaustivo della partizione per 1--8 bande, 1--5 rank e tutti i
  vettori di pesi con valori 0--3: copertura, contiguità, assenza di
  sovrapposizioni e massimo carico uguale all'ottimo esaustivo verificati.
- Verificati inoltre casi uniformi, peso dominante, pesi nulli e più rank che
  bande.
- `git diff --check`: superato.
- Caso locale Al_bulk, 4 rank MPI tutti sul livello `c`, percorso lifetimes su
  8 q-point: completati run legacy e pesato in
  `/home/nicola/tmp/codex/yambo5-xo-weighted-local-87bc4e536`.
- Il primo run pesato ha rivelato che lifetimes richiamava
  `X_dielectric_matrix` separatamente per q e ricostruiva la partizione. La
  correzione ora esegue il prepass una sola volta sulla sequenza completa dei
  q-point e riusa la maschera nelle chiamate successive.
- Rerun `weighted-static`: una sola partizione, intervalli `2:5`, `6:10`,
  `11:15`, `16:20`; copertura completa, contigua e senza sovrapposizioni.
- Confronto transizioni prepass/X-CG esatto su ogni rank: 3068 per il primo
  intervallo e 4040 per ciascuno degli altri; nei log per-rank i totali X-CG
  sono rispettivamente la somma 378+379+385+387+382+385+383+389 e 505*8.
- Il confronto dei soli `o-legacy.qp` e `o-weighted_static.qp` mostra identiche
  energie e differenze di arrotondamento nell'ultima cifra stampata per alcuni
  valori, coerenti col diverso ordine delle somme MPI.
- Tempi del piccolo caso (non rappresentativo): legacy `Xo (procedure)` max
  0.0521 s; pesato statico 0.0620 s, prepass 0.0004 s. Il criterio prestazionale
  resta demandato al benchmark anatase.

## Commit pubblicati e richieste di test

- Commit documentale pubblicato: `48bef44a4`.
- Commit iniziale d'implementazione pubblicato: `87bc4e536`.
- Correzione statica e verifiche locali pubblicate: `cf76e8fdf`.
- Commit esatto richiesto per T01:
  `cf76e8fdf366f678e4c39f33ece00d5f253c0676`.
- T01 era stato inizialmente predisposto come due run q=1 sullo stesso binario,
  ma tali input non sono eseguibili per anatase perché le successive fasi
  HF/GW richiedono i dati di tutti i q-point. I risultati effettivamente forniti
  usano pertanto l'input anatase completo.

## Directory dei risultati

- Radice prevista:
  `/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata`.
- Sessione dei risultati:
  `T01_anatase_full_a4a1dfaa9_cf76e8fdf`, con manifest, istruzioni, input
  completo e report/output/log di entrambe le varianti.

## Analisi e problemi

- Il percorso attuale costruisce la maschera `c` in
  `PARALLEL_global_indexes` e distribuisce le funzioni d'onda più tardi in
  `X_dielectric_matrix`, lasciando il punto necessario per sostituire la sola
  maschera prima della distribuzione.
- La logica adattiva del coarse grid è attualmente accoppiata ad allocazioni,
  strutture globali e messaggi; va estratto un nucleo condiviso.

## Analisi T01

- I risultati copiati in `T01_anatase_full_a4a1dfaa9_cf76e8fdf` sono relativi all'input
  completo `yambo-anatase-full.in`, con tutti i 30 q-point. Questa scelta è
  necessaria perché limitare Xo al solo q=1 rende indisponibili i dati richiesti
  dalle successive fasi HF/GW della self-energy.
- Il nuovo percorso si attiva una sola volta e completa il run. Il prepass
  costa 9.53 s per rank rispetto a circa 01h35m della fase Xo.
- La partizione ottenuta è quasi equinumerosa: `249, 248, 247, 247, 246,
  246, 247, 246` bande. Anche i pesi previsti sono quasi uniformi, fra
  15,837,486 e 15,882,285 gruppi per rank.
- Il carico reale non è uniforme. La somma dei gruppi `[X-CG]` sui 30 q-point
  scende monotonicamente da 7,819,825 sul rank 0 a 5,443,476 sul rank 7. Il
  timer `Xo (procedure)` scende nello stesso ordine da 5816.88 s a 3173.22 s;
  l'attesa `Xo (REDUX)` cresce da 0.64 s a 2646.79 s.
- La causa è la non additività dei gruppi: il prepass somma
  `Ngruppi(banda)` calcolati su bande isolate, ma il calcolo raggruppa l'unione
  delle transizioni dell'intero intervallo. Le sovrapposizioni energetiche fra
  bande eliminano una quota crescente dei gruppi e il segnale utile viene
  perso.
- La provenienza confermata dall'utente è: legacy dal branch `5.4`, commit
  `a4a1dfaa9800917b36bd5aee613d27ff99c60c73`; weighted dal branch di lavoro
  con la funzionalità T01. L'indicazione di branch/revisione stampata nei report
  non è attendibile e non va usata per ricostruire il binario impiegato.
- Poiché legacy e weighted sono intenzionalmente due build diverse, il
  confronto misura la modifica rispetto alla base prevista. La verifica fisica
  resta affidata a `o-OUTPUT.qp`, tenendo presenti le normali differenze dovute
  all'ordine delle riduzioni MPI.

## Correzione successiva a T01

- Il peso di una banda è ora il contributo marginale locale
  `max(Ngruppi(ic-1 U ic)-Ngruppi(ic-1),0)`, sommato sugli stessi q-point.
  Per la prima banda si usa il suo numero di gruppi.
- La coppia adiacente introduce nel peso la sovrapposizione energetica che T01
  ha mostrato essere dominante, lasciando invariati partizionatore contiguo,
  filtri fisici e percorso produttivo Xo.
- La stima resta intenzionalmente locale e approssimata: T01 successivo dovrà
  verificare che i nuovi confini anticipino il carico verso le bande basse e
  riducano la dispersione di `[X-CG]` e `Xo (procedure)`.
- Eseguita una build pulita di `yambo` dopo
  `module load profile/gcc-14.3.0`: compilazione e link completati.
- Rilanciato il caso MPI locale Al_bulk a 4 rank con job
  `weighted_marginal_gcc143`. Il prepass è eseguito una volta, costa 0.0011 s
  e produce gli intervalli `2:6`, `7:11`, `12:15`, `16:20`, con pesi marginali
  rispettivamente 625, 684, 538 e 683. La copertura resta completa, contigua e
  senza sovrapposizioni.
- Il confronto con `o-weighted_static.qp` conserva energie identiche e mostra
  soltanto differenze di arrotondamento nelle ultime cifre stampate delle altre
  colonne, coerenti con il diverso ordine delle riduzioni MPI.
- Per le prossime build e per tutti i test locali usare sempre
  `module load profile/gcc-14.3.0`; non usare più il file profilo
  `/home/nicola/src/profile_gcc_openmpi.txt`.
- Avviata la valutazione non invasiva delle partizioni specifiche per q-point:
  il prepass riduce separatamente i pesi marginali di ciascun q, calcola la
  relativa partizione minimax e la riporta nelle righe `[X-WB-Q]`. La somma dei
  pesi per q continua a determinare l'unica partizione statica produttiva;
  maschere e distribuzione delle funzioni d'onda non cambiano durante il loop.
- Per ogni q viene inoltre riportato il confronto `ideal max`/`static max` e la
  riduzione percentuale teorica del massimo carico marginale. Questa misura
  permetterà di decidere dai dati anatase se studiare una redistribuzione delle
  funzioni d'onda per q o poche partizioni condivise da gruppi di q-point.
- Build incrementale e test MPI locale `weighted_qbenefit_gcc143` completati
  con `profile/gcc-14.3.0`. Sul caso Al_bulk, sei q-point su otto hanno beneficio
  teorico nullo rispetto alla partizione statica; q=6 e q=8 mostrano soltanto
  2.13% e 2.33%. Il prepass passa da circa 0.0011 s a 0.0020 s e l'output fisico
  conserva le sole differenze di arrotondamento già osservate. Il caso piccolo
  non giustifica una redistribuzione per q, ma la decisione resta demandata alla
  diagnostica sul benchmark anatase.

## Prossima attività

- Correzione marginale e diagnostica per-q pubblicate nel commit
  `d48b5dd5f2c940a9a89e9352d45a4a2c7c153b69`.
- T02 predisposto in
  `/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata/`
  `T02_anatase_full_a4a1dfaa9_d48b5dd5f`, con input anatase completi e
  directory separate per legacy e weighted.
- Eseguire T02 e analizzare partizione statica, diagnostica `[X-WB-Q]`, carichi
  reali `[X-CG]`, timer e `o-OUTPUT.qp` prima di ulteriori modifiche alla
  distribuzione.

## Analisi T02: stima marginale e diagnostica per q

Provenienza usata per l'analisi, indipendentemente dalle stringhe stampate nei
report:

- T01 legacy: `a4a1dfaa9800917b36bd5aee613d27ff99c60c73`;
- T01 weighted: `cf76e8fdf366f678e4c39f33ece00d5f253c0676`;
- T02 weighted: `d48b5dd5f2c940a9a89e9352d45a4a2c7c153b69`.

Gli input weighted T01 e T02 sono byte-identici (SHA-256
`bde9fc279770e9480091c2dc8816c1cce8ecfd8ef449d3b5dafe054a53c46695`).
L'input legacy differisce per l'assenza dell'opzione weighted, come previsto.

### Partizione statica e significato del messaggio `247/1976`

La partizione T02 riportata da `[X-WB]` è:

| c-rank | bande | numero | gruppi marginali previsti | transizioni accettate |
|---:|:---:|---:|---:|---:|
| 0 | 25:273 | 249 | 13,560,352 | 38,724,480 |
| 1 | 274:521 | 248 | 13,545,929 | 38,568,960 |
| 2 | 522:769 | 248 | 13,560,914 | 38,568,960 |
| 3 | 770:1017 | 248 | 13,570,588 | 38,568,960 |
| 4 | 1018:1262 | 245 | 13,530,561 | 38,102,400 |
| 5 | 1263:1507 | 245 | 13,544,664 | 38,102,400 |
| 6 | 1508:1754 | 247 | 13,535,283 | 38,413,440 |
| 7 | 1755:2000 | 246 | 13,563,924 | 38,257,920 |

La copertura 25:2000 è completa, contigua e senza sovrapposizioni. I pesi
previsti hanno range 40,027 e CV 0.100%, quindi il partizionatore risolve
correttamente il problema definito dalla stima. Rispetto a T01 weighted i
confini cambiano però molto poco: i numeri di bande passano da
`249,248,247,247,246,246,247,246` a
`249,248,248,248,245,245,247,246`.

La riga di log
`[PARALLEL Response_G_space_and_IO for CON bands on 8 CPU] ... 247/1976`
non descrive la maschera weighted finale. Viene emessa dentro
`PARALLEL_global_indexes`, immediatamente dopo la distribuzione storica
equinumerosa provvisoria. `X_weighted_bands_setup` viene chiamata dopo e
sostituisce `PAR_IND_CON_BANDS_X`; ancora dopo,
`PARALLEL_WF_distribute` riceve questa maschera aggiornata. Le righe `[X-WB]`
e i cambiamenti coerenti dei conteggi `[X-CG]` sui rank i cui confini sono
mutati confermano che la nuova maschera viene effettivamente usata. Il
messaggio `247/1976` è quindi fuorviante per questa modalità, non evidenza di
una mancata distribuzione weighted.

### Diagnostica marginale specifica per q

Tutti i 30 insiemi `[X-WB-Q]` coprono 25:2000 con otto intervalli contigui.
I confini specifici oscillano di poche bande attorno a quelli statici: il
primo limite superiore varia fra 270 e 274 e gli altri limiti mostrano
spostamenti dello stesso ordine. Il confronto completo dei massimi è:

| q | ideal max | static max | beneficio teorico |
|---:|---:|---:|---:|
| 1 | 146,220 | 147,700 | 1.00% |
| 2 | 565,363 | 565,546 | 0.03% |
| 3 | 541,255 | 541,299 | 0.01% |
| 4 | 327,331 | 328,034 | 0.21% |
| 5 | 531,056 | 531,145 | 0.02% |
| 6 | 651,335 | 654,050 | 0.42% |
| 7 | 674,449 | 676,128 | 0.25% |
| 8 | 791,666 | 793,634 | 0.25% |
| 9 | 348,552 | 350,258 | 0.49% |
| 10 | 521,922 | 521,922 | 0.00% |
| 11 | 705,405 | 708,837 | 0.48% |
| 12 | 343,965 | 345,629 | 0.48% |
| 13 | 211,568 | 213,484 | 0.90% |
| 14 | 229,169 | 230,334 | 0.51% |
| 15 | 500,418 | 501,290 | 0.17% |
| 16 | 541,185 | 541,185 | 0.00% |
| 17 | 491,649 | 492,058 | 0.08% |
| 18 | 358,893 | 359,329 | 0.12% |
| 19 | 530,950 | 531,314 | 0.07% |
| 20 | 791,031 | 793,372 | 0.30% |
| 21 | 468,114 | 468,114 | 0.00% |
| 22 | 340,782 | 342,262 | 0.43% |
| 23 | 521,809 | 521,809 | 0.00% |
| 24 | 315,090 | 316,961 | 0.59% |
| 25 | 483,147 | 483,147 | 0.00% |
| 26 | 229,009 | 230,378 | 0.59% |
| 27 | 565,351 | 565,586 | 0.04% |
| 28 | 359,029 | 359,772 | 0.21% |
| 29 | 345,497 | 347,219 | 0.50% |
| 30 | 143,788 | 145,054 | 0.87% |

Il beneficio medio non pesato è 0.301%, la mediana 0.230%, il massimo
1.00%; cinque q-point hanno beneficio nullo. Sommando i massimi sui q, la
riduzione prevista è 31,852 gruppi marginali su 13,606,850, cioè 0.234%.

### Gruppi reali `[X-CG]`

La tabella riporta tutti i q-point e i rank T02; l'ultima colonna è il
rapporto fra massimo e minimo del q.

| q | r0 | r1 | r2 | r3 | r4 | r5 | r6 | r7 | max/min |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 119743 | 111111 | 107137 | 104860 | 102382 | 100940 | 99633 | 98363 | 1.217 |
| 2 | 306512 | 260063 | 241187 | 231715 | 224221 | 216919 | 210091 | 206293 | 1.486 |
| 3 | 298787 | 254230 | 236467 | 226937 | 219470 | 212741 | 206426 | 202592 | 1.475 |
| 4 | 218769 | 193219 | 182292 | 176535 | 171453 | 167510 | 163347 | 160813 | 1.360 |
| 5 | 296971 | 253081 | 235226 | 225937 | 218130 | 211912 | 205202 | 201730 | 1.472 |
| 6 | 332585 | 279396 | 258892 | 248145 | 239491 | 232021 | 224330 | 220236 | 1.510 |
| 7 | 338769 | 283208 | 262010 | 250879 | 241842 | 234016 | 225794 | 221931 | 1.526 |
| 8 | 367482 | 303940 | 279660 | 267571 | 257487 | 248952 | 239607 | 234921 | 1.564 |
| 9 | 229311 | 200936 | 189357 | 183447 | 177799 | 173702 | 169316 | 166587 | 1.377 |
| 10 | 294143 | 250913 | 233533 | 224603 | 216364 | 210384 | 203873 | 200301 | 1.469 |
| 11 | 347193 | 289333 | 266758 | 255928 | 246330 | 238884 | 229770 | 225803 | 1.538 |
| 12 | 227179 | 199682 | 188101 | 182360 | 176386 | 172377 | 168107 | 165537 | 1.372 |
| 13 | 161121 | 145832 | 139587 | 136051 | 132428 | 130249 | 127682 | 126413 | 1.275 |
| 14 | 170031 | 153552 | 146354 | 142123 | 138574 | 135723 | 133119 | 131449 | 1.294 |
| 15 | 285668 | 244772 | 228730 | 219442 | 212655 | 206292 | 200638 | 197203 | 1.449 |
| 16 | 298327 | 254199 | 236839 | 226942 | 219278 | 212899 | 206304 | 202776 | 1.471 |
| 17 | 282903 | 242262 | 226415 | 217874 | 210658 | 204584 | 198658 | 195080 | 1.450 |
| 18 | 232356 | 203752 | 191981 | 185544 | 180317 | 175926 | 171304 | 168637 | 1.378 |
| 19 | 297077 | 252842 | 235215 | 226297 | 218357 | 212155 | 204941 | 201609 | 1.474 |
| 20 | 367563 | 303588 | 279573 | 267302 | 257504 | 248864 | 239477 | 235117 | 1.563 |
| 21 | 276048 | 237343 | 221811 | 213684 | 206790 | 201205 | 195169 | 191822 | 1.439 |
| 22 | 226404 | 198965 | 187560 | 181924 | 176171 | 172089 | 167800 | 165322 | 1.369 |
| 23 | 294244 | 250626 | 233259 | 224414 | 216454 | 210542 | 204004 | 200175 | 1.470 |
| 24 | 214337 | 189344 | 179001 | 173473 | 167638 | 164507 | 160047 | 158043 | 1.356 |
| 25 | 280098 | 240562 | 224631 | 216193 | 209465 | 203739 | 197339 | 194078 | 1.443 |
| 26 | 169965 | 153432 | 146557 | 142236 | 138534 | 136060 | 133279 | 131420 | 1.293 |
| 27 | 306600 | 259820 | 241469 | 231671 | 223964 | 216983 | 210115 | 206531 | 1.485 |
| 28 | 232497 | 204099 | 192035 | 185889 | 180230 | 176066 | 171298 | 168813 | 1.377 |
| 29 | 228614 | 200584 | 189131 | 183234 | 177304 | 173695 | 168996 | 166159 | 1.376 |
| 30 | 118528 | 110242 | 106337 | 104056 | 101542 | 100320 | 99032 | 97722 | 1.213 |

Le somme sui 30 q per rank sono
`7,819,825, 6,724,928, 6,287,105, 6,057,266, 5,859,218, 5,702,256,
5,534,698, 5,443,476`. Lo sbilanciamento resta monotono e sostanzialmente
identico a T01 weighted. Il CV passa anzi da 11.832% a 11.869%; il range resta
2,376,349 perché gli estremi, rank 0 e rank 7, non cambiano. La stima
marginale bilancia se stessa ma non il numero reale di gruppi di un intervallo:
la non additività a più bande rimane dominante.

### Timer e costo complessivo

| run | `Xo (procedure)` min/max | range | CV | `Xo (REDUX)` min/max | prepass max | cammino critico Xo + prepass |
|:---|:---|---:|---:|:---|---:|---:|
| T01 legacy | 3197.292 / 5736.258 s | 2538.967 s | 20.186% | 0.644 / 2549.431 s | 0 | 5736.258 s |
| T01 weighted | 3173.223 / 5816.876 s | 2643.653 s | 20.783% | 0.638 / 2646.794 s | 9.532 s | 5826.407 s |
| T02 weighted | 3174.054 / 5797.772 s | 2623.718 s | 20.737% | 0.634 / 2627.305 s | 29.336 s | 5827.108 s |

La correzione riduce il massimo `Xo (procedure)` di 19.10 s rispetto a T01
weighted, ma il prepass aumenta di 19.80 s per la diagnostica per-q: il totale
stimato del cammino critico è 0.70 s più lento di T01 weighted e 90.85 s
(1.58%) più lento del legacy. A risoluzione di un minuto, entrambi i run
weighted riportano 02h34m complessivi e il legacy 02h33m. Le variazioni di
pochi decimi di punto nei timer di procedura non costituiscono un miglioramento
misurabile del bilanciamento.

### Equivalenza fisica e anomalie

`o-OUTPUT.qp` contiene gli stessi 420 stati `(k,banda)` e valori `Eo`
identici al legacy T01. Sulle colonne dipendenti dal calcolo:

- T02 contro legacy: differenza massima `E-Eo` 1.47e-4 eV, RMS 3.48e-5 eV;
  differenza massima `Sc|Eo` 2.8e-5 eV, RMS 1.17e-5 eV;
- T02 contro T01 weighted: differenza massima `E-Eo` 2.8e-5 eV e massima
  `Sc|Eo` 5e-6 eV.

Le differenze T02/legacy sono della stessa ampiezza già presente fra T01
weighted e legacy (massimo `E-Eo` 1.60e-4 eV), mentre T01/T02 weighted sono
molto più vicini. Non emerge quindi una regressione fisica attribuibile alla
correzione marginale; le differenze sono compatibili con il diverso ordine
delle somme parallele e con la variabilità numerica fra i due run/build.

Il run termina regolarmente. Non compaiono errori, abort, NaN o warning nuovi.
L'unico warning, ripetuto nei log dei rank e una volta nel report, è quello
preesistente secondo cui `[x,Vnl]` rallenta il calcolo dei dipoli. Le stringhe
di branch/revisione nei report sono state intenzionalmente ignorate. L'unica
anomalia diagnostica rilevante è il messaggio provvisorio `247/1976`, corretto
nel punto in cui viene emesso ma facile da interpretare erroneamente come
distribuzione produttiva finale.

### Decisione dopo T02

1. La stima marginale **non migliora realmente il bilanciamento** rispetto a
   T01: confini e carichi reali cambiano troppo poco, il CV `[X-CG]` non
   diminuisce e la dispersione dei timer resta sostanzialmente invariata.
2. Una partizione specifica per q **non giustifica la redistribuzione delle
   funzioni d'onda**: il guadagno teorico massimo è 1.00%, quello medio 0.301%
   e quello aggregato 0.234%. Inoltre tali percentuali ottimizzano una stima
   che non correla adeguatamente col carico reale.
3. Non vi è evidenza neppure per introdurre poche classi di q-point: i confini
   specifici sono già molto vicini alla partizione statica e il vantaggio
   residuo è trascurabile. Fra le tre alternative considerate conviene
   **mantenere una sola partizione statica**. Questa conclusione non rende però
   efficace l'attuale euristica marginale: prima di qualsiasi T03 o modifica
   ulteriore va discusso se fermarsi, oppure cercare in futuro una stima del
   costo di intervallo realmente non additiva. Non è stata implementata
   alcuna ulteriore modifica e T03 non è stato predisposto.

## Analisi oracle offline successiva a T02

Su richiesta, è stata eseguita un'analisi esclusivamente offline per capire
se il fallimento della stima marginale escluda anche un vantaggio ottenibile
da una diversa partizione statica. Non sono stati modificati sorgenti o
risultati e non è stato predisposto T03.

### Capacità predittiva dei gruppi reali

La somma dei gruppi `[X-CG]` sui 30 q-point è un ottimo predittore del timer
per rank:

| run | correlazione Pearson | correlazione di rango | R² regressione lineare | errore RMS del fit |
|:---|---:|---:|---:|---:|
| T01 weighted | 0.99734 | 1.000 | 0.99469 | 59.3 s |
| T02 weighted | 0.99767 | 1.000 | 0.99535 | 55.3 s |

In T02 il fit sui rank è

```text
Xo (procedure) [s] = -2907.05 + 0.00110374 * somma_gruppi
```

Anche ripetendo il confronto separatamente per ciascun q, fra i suoi otto
carichi `[X-CG]` e il timer totale dei rank, le correlazioni sono sempre molto
alte: minimo 0.9902, media 0.9968, massimo 0.9990. Questo non assegna un tempo
separato a ogni q, ma dimostra che il gradiente dei gruppi fra rank è stabile
su tutti i q e spiega quasi interamente il gradiente del tempo complessivo.
Bilanciare i gruppi reali è quindi un obiettivo prestazionale fondato; il
fallimento T02 riguarda la loro stima, non la scelta della metrica osservata.

### Oracle statico approssimato

Come primo limite superiore si è assegnata a ogni banda la densità media
osservata nell'intervallo T02 che la contiene, quindi si è riequilibrata la
somma di questi costi piecewise-constant. Questo modello conserva il carico
totale osservato, 49,428,772 gruppi, con obiettivo 6,178,596.5 per rank.

| c-rank | intervallo T02 | intervallo oracle approssimato | bande oracle | carico previsto |
|---:|:---:|:---:|---:|---:|
| 0 | 25:273 | 25:221 | 197 | 6,186,769 |
| 1 | 274:521 | 222:440 | 219 | 6,161,536 |
| 2 | 522:769 | 441:678 | 238 | 6,176,591 |
| 3 | 770:1017 | 679:928 | 250 | 6,190,451 |
| 4 | 1018:1262 | 929:1184 | 256 | 6,167,611 |
| 5 | 1263:1507 | 1185:1448 | 264 | 6,194,443 |
| 6 | 1508:1754 | 1449:1721 | 273 | 6,168,441 |
| 7 | 1755:2000 | 1722:2000 | 279 | 6,182,930 |

Il massimo del modello scende da 7,819,825 a 6,194,443 gruppi, -20.79%.
Applicando il fit T02, il cammino critico `Xo (procedure)` passerebbe da
5797.8 a circa 3930.0 s, un limite teorico di circa 1868 s o 32.2%. La
percentuale sul tempo è maggiore di quella sui gruppi a causa dell'intercetta
del fit e va considerata soltanto nel dominio di carico osservato, nel quale il
nuovo massimo comunque ricade.

I confini oracle richiedono molte meno bande sui rank bassi e molte di più su
quelli alti. Questo è esattamente il segnale che le due stime per-banda provate
finora non hanno riprodotto: sia T01 sia T02 hanno prodotto blocchi quasi
equinumerosi.

### Informazione empirica dagli spostamenti T01--T02

Le poche differenze fra le due partizioni permettono due misure locali pulite:

- aggiungere la banda 769 all'intervallo 522:768 aumenta la somma reale dei
  gruppi di 19,008, con incrementi positivi per tutti i q (415--760,
  media 633.6);
- rimuovere la banda 1262 dall'intervallo 1262:1507 riduce la somma di 18,829,
  con decrementi per tutti i q (371--789 in valore assoluto, media 627.6).

Questi marginali reali valgono rispettivamente circa il 76.5% e l'82.1% della
densità media per banda dei relativi intervalli. Confermano due aspetti:

1. lo spostamento dei confini produce effetti reali, coerenti e misurabili su
   tutti i q;
2. anche il costo marginale reale di una banda dipende dall'intervallo che la
   contiene e non coincide con un peso additivo indipendente.

Le variazioni dei timer T01--T02 non possono invece essere attribuite banda
per banda: quattro rank non cambiano maschera ma mostrano comunque variazioni
fra -19.1 e +4.5 s, che quantificano il rumore fra run. I cambiamenti osservati
sui rank modificati sono dello stesso ordine o poco superiori a tale rumore.

### Interpretazione e passo successivo raccomandato

L'oracle non è una previsione quantitativa della partizione finale. Assume
additività all'interno degli otto intervalli misurati, mentre proprio T01/T02
dimostrano che dividere, estendere o unire un intervallo cambia le
sovrapposizioni energetiche e quindi il suo costo. I risultati esistenti non
contengono il costo reale dei nuovi intervalli oracle; tale costo non può
essere ricostruito esattamente dai soli otto valori per q.

Il margine teorico del 20.8% sui gruppi e del 32.2% sul timer è però molto più
grande del rumore e del costo del prepass T02. Esiste quindi evidenza
sufficiente per non abbandonare l'idea di una partizione statica pesata. Il
problema va attribuito principalmente all'algoritmo di stima dei pesi:
`PARALLEL_index_weighted_contiguous` bilancia correttamente i numeri ricevuti,
ma né gruppi isolati né marginali di coppie rappresentano il costo di blocchi
di 200--300 bande.

Il prossimo esperimento, da discutere prima di implementarlo, dovrebbe
misurare direttamente una funzione `cost(a,b,q)` sugli intervalli candidati.
Una strategia contenuta sarebbe:

1. partire dai confini oracle approssimati;
2. nel prepass valutare i gruppi dell'intero intervallo per pochi confini
   candidati attorno a ciascun taglio;
3. spostare iterativamente i tagli dal rank più carico a quello adiacente,
   usando il costo ricalcolato dei due intervalli e non pesi per-banda;
4. produrre sempre una sola partizione statica, senza classi di q o
   redistribuzioni durante il loop;
5. prima di un run completo, aggiungere una modalità diagnostica che calcoli e
   stampi i costi degli otto intervalli proposti senza usarli, così da validare
   la previsione separatamente dall'esecuzione produttiva.

Questa evidenza giustifica un prepass *interval-aware*, non un altro tentativo
di perfezionare un peso additivo locale per banda.

## Implementazione diagnostica interval-aware

Implementata, senza cambiare la partizione produttiva, la verifica proposta
dall'oracle offline:

- il prepass conta i gruppi dell'intero intervallo statico di ciascun c-rank;
- costruisce una proposta usando la densità media di gruppi per banda misurata
  negli intervalli statici;
- conta nuovamente i gruppi degli interi intervalli proposti;
- stampa in `[X-WB-I]` costi statici e proposti per ogni q/rank, massimi per q,
  confini e totali aggregati;
- misura separatamente i due conteggi nel timer
  `Xo weighted interval diagnostic`;
- lascia `PAR_IND_CON_BANDS_X` sulla partizione marginale precedente, quindi
  non ridistribuisce le funzioni d'onda secondo la proposta.

L'helper interval-aware riproduce gli stessi filtri fisici del prepass e usa
`FREQUENCIES_group_engine`. Ogni c-rank valuta un intervallo completo; una
riduzione intera a 64 bit rende disponibili tutti i costi. Il caso con rank
eccedenti resta collettivo e non introduce ritorni anticipati che potrebbero
causare deadlock.

### Build e test MPI locale

- Build incrementale con `module load profile/gcc-14.3.0 && make yambo`:
  completata.
- Caso Al_bulk lifetimes, 4 rank tutti sul livello `c`, 8 q-point, job
  `weighted_interval_diag2_gcc143`: completato.
- I costi statici `[X-WB-I]` riproducono esattamente le righe `[X-CG]` del
  calcolo produttivo per ciascun q e rank. Le somme sono rispettivamente
  `625, 715, 575, 698`.
- La partizione produttiva resta `2:6, 7:11, 12:15, 16:20`. La proposta
  diagnostica è `2:5, 6:10, 11:15, 16:20`, con costi esatti aggregati
  `507, 696, 704, 698`: il massimo passa da 715 a 704, soltanto -1.54%, come
  atteso dal debole potenziale del piccolo caso.
- Per q, la variazione del massimo è compresa fra -2.17% e +3.06%; il segno
  misto conferma la necessità di validare gli intervalli completi anziché
  fidarsi della sola proposta piecewise-constant.
- Il timer `Xo weighted interval diagnostic` è 0.0009 s; l'intero prepass
  arriva a 0.0026 s nel caso piccolo.
- `o-weighted_interval_diag2_gcc143.qp` conserva gli stessi stati ed energie;
  le differenze rispetto al precedente weighted sono esclusivamente nelle
  ultime cifre stampate, coerenti con la normale variabilità delle riduzioni.
- Eseguito anche `legacy_interval_diag_guard_gcc143` con il flag disabilitato:
  nessuna riga `[X-WB]` o `[X-WB-I]`, completamento regolare e sole differenze
  di arrotondamento rispetto al precedente output legacy.
- `git diff --check`: superato.

Il diagnostico è stato pubblicato nel commit
`5ebe585da72b80b6d37409a4735b9dfd0b549569`. T03 è predisposto in
`/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata/`
`T03_anatase_interval_diag_d48b5dd5f_5ebe585da` come singolo run weighted
diagnostico sull'input anatase completo. Il manifest richiede esplicitamente
il checkout del commit `5ebe585da`, raccoglie report, output QP e tutti i log
per-rank e usa T02 weighted e T01 legacy già disponibili come riferimenti.

## Analisi T03: diagnostica interval-aware su anatase

### Provenienza, completezza e terminazione

La provenienza attendibile dichiarata dall'utente è il binario costruito dal
commit applicativo `5ebe585da72b80b6d37409a4735b9dfd0b549569`; le stringhe
di branch e revisione stampate da Yambo sono state ignorate. Il successivo
commit `ae3b52247a73652a90dadac42d1738e2d8cf5bf1` modifica soltanto questo
diario. Il branch corrente è `tech-xo-weighted-band-distribution`, allineato a
`origin`, e prima di questa analisi il solo file non tracciato era
`prossimo_prompt_post_T03.md`.

La directory T03 contiene l'input effettivo `yambo-anatase-full.in`, un report,
`o-OUTPUT.qp` e tutti gli otto log `CPU_1`--`CPU_8`. L'input ha SHA-256
`bde9fc279770e9480091c2dc8816c1cce8ecfd8ef449d3b5dafe054a53c46695`,
identico byte per byte agli input weighted T01 e T02. Ogni log contiene una
sola sezione `Game Over`; il report registra inizio 2026-10-09 23:03, fine
2026-10-10 01:38 e tempo totale 02h35m. Non vi sono file mancanti né segnali di
terminazione incompleta.

### Partizione realmente usata

La partizione produttiva `[X-WB]` coincide esattamente con T02:

| c-rank | bande produttive | numero |
|---:|:---:|---:|
| 0 | 25:273 | 249 |
| 1 | 274:521 | 248 |
| 2 | 522:769 | 248 |
| 3 | 770:1017 | 248 |
| 4 | 1018:1262 | 245 |
| 5 | 1263:1507 | 245 |
| 6 | 1508:1754 | 247 |
| 7 | 1755:2000 | 246 |

Anche pesi marginali previsti e transizioni accettate coincidono con T02. Le
240 coppie `(q,c-rank)` di `static exact groups` coincidono senza eccezioni con
le successive righe produttive `[X-CG]` dello stesso q e rank: 240/240 uguali,
zero discrepanze, differenza massima e somma delle differenze entrambe zero.
Questo prova sia l'esattezza del diagnostico statico sia che la proposta
`[X-WB-I]` non è stata passata a `PARALLEL_WF_distribute`.

### Dati completi per q-point

In ciascuna cella sono elencati i rank 0--7. Le ultime colonne riportano i due
massimi e la riduzione misurata stampata dal diagnostico.

| q | static exact groups r0--r7 | proposed exact groups r0--r7 | max statico | max proposto | riduzione |
|---:|:---|:---|---:|---:|---:|
| 1 | 119743,111111,107137,104860,102382,100940,99633,98363 | 96885,100681,104374,106184,106882,107712,108450,109190 | 119743 | 109190 | 8.81% |
| 2 | 306512,260063,241187,231715,224221,216919,210091,206293 | 256230,242346,238773,236019,233271,229937,226419,223948 | 306512 | 256230 | 16.40% |
| 3 | 298787,254230,236467,226937,219470,212741,206426,202592 | 249664,236620,233930,231093,228411,225652,222344,220076 | 298787 | 249664 | 16.44% |
| 4 | 218769,193219,182292,176535,171453,167510,163347,160813 | 179984,177922,179321,179346,178400,177981,176812,176038 | 218769 | 179984 | 17.73% |
| 5 | 296971,253081,235226,225937,218130,211912,205202,201730 | 247898,235541,232516,229933,227142,224903,221191,219092 | 296971 | 247898 | 16.52% |
| 6 | 332585,279396,258892,248145,239491,232021,224330,220236 | 279378,261064,256618,252493,248969,245800,241037,238323 | 332585 | 279378 | 16.00% |
| 7 | 338769,283208,262010,250879,241842,234016,225794,221931 | 284991,264864,259651,255494,251708,248220,242858,240217 | 338769 | 284991 | 15.87% |
| 8 | 367482,303940,279660,267571,257487,248952,239607,234921 | 310619,285188,277833,272901,267939,263572,257304,253681 | 367482 | 310619 | 15.47% |
| 9 | 229311,200936,189357,183447,177799,173702,169316,166587 | 189164,185428,186389,186478,185031,184548,183261,182106 | 229311 | 189164 | 17.51% |
| 10 | 294143,250913,233533,224603,216364,210384,203873,200301 | 245343,233198,231053,228674,225479,223464,219540,217675 | 294143 | 245343 | 16.59% |
| 11 | 347193,289333,266758,255928,246330,238884,229770,225803 | 292028,270778,264909,260808,256399,253002,247127,244134 | 347193 | 292028 | 15.89% |
| 12 | 227179,199682,188101,182360,176386,172377,168107,165537 | 187281,184038,185269,185245,183978,183201,181853,181037 | 227179 | 187281 | 17.56% |
| 13 | 161121,145832,139587,136051,132428,130249,127682,126413 | 131369,133286,136411,137983,138142,138882,138796,139269 | 161121 | 139269 | 13.56% |
| 14 | 170031,153552,146354,142123,138574,135723,133119,131449 | 138770,140371,143403,144095,144467,144468,144759,144667 | 170031 | 144759 | 14.86% |
| 15 | 285668,244772,228730,219442,212655,206292,200638,197203 | 238210,227386,225739,223113,221199,218694,215988,214250 | 285668 | 238210 | 16.61% |
| 16 | 298327,254199,236839,226942,219278,212899,206304,202776 | 249391,236694,233978,231164,228599,225617,222181,220112 | 298327 | 249391 | 16.40% |
| 17 | 282903,242262,226415,217874,210658,204584,198658,195080 | 235461,225351,223862,221361,219534,216843,214149,212087 | 282903 | 235461 | 16.77% |
| 18 | 232356,203752,191981,185544,180317,175926,171304,168637 | 191963,188137,188888,188826,187789,187031,185229,184427 | 232356 | 191963 | 17.38% |
| 19 | 297077,252842,235215,226297,218357,212155,204941,201609 | 247956,235374,232239,229920,227131,225161,221171,218912 | 297077 | 247956 | 16.53% |
| 20 | 367563,303588,279573,267302,257504,248864,239477,235117 | 310530,285001,277783,272628,268064,263660,257226,253749 | 367563 | 310530 | 15.52% |
| 21 | 276048,237343,221811,213684,206790,201205,195169,191822 | 229694,220506,219015,217424,215222,213439,210569,208734 | 276048 | 229694 | 16.79% |
| 22 | 226404,198965,187560,181924,176171,172089,167800,165322 | 186834,183575,184729,184831,183527,182780,181413,180888 | 226404 | 186834 | 17.48% |
| 23 | 294244,250626,233259,224414,216454,210542,204004,200175 | 245552,232978,230857,228609,225547,223313,219776,217526 | 294244 | 245552 | 16.55% |
| 24 | 214337,189344,179001,173473,167638,164507,160047,158043 | 176334,174163,175911,176180,174842,174797,173471,173321 | 214337 | 176334 | 17.73% |
| 25 | 280098,240562,224631,216193,209465,203739,197339,194078 | 233208,223404,221890,220211,217765,216213,212673,211212 | 280098 | 233208 | 16.74% |
| 26 | 169965,153432,146557,142236,138534,136060,133279,131420 | 138762,140289,143277,144394,144381,144806,144796,144824 | 169965 | 144824 | 14.79% |
| 27 | 306600,259820,241469,231671,223964,216983,210115,206531 | 256467,242362,238872,236054,233108,230097,226274,224117 | 306600 | 256467 | 16.35% |
| 28 | 232497,204099,192035,185889,180230,176066,171298,168813 | 192051,188270,189039,188796,187870,186880,185257,184435 | 232497 | 192051 | 17.40% |
| 29 | 228614,200584,189131,183234,177304,173695,168996,166159 | 188510,185048,186074,186355,184933,184621,182610,181726 | 228614 | 188510 | 17.54% |
| 30 | 118528,110242,106337,104056,101542,100320,99032,97722 | 95953,99790,103523,105501,105779,106959,108009,108412 | 118528 | 108412 | 8.53% |

Il beneficio per q è sempre positivo: 30 positivi, zero nulli e zero negativi;
media 15.944%, mediana 16.525%, minimo 8.53% e massimo 17.73%.

### Confini proposti e gruppi aggregati

| c-rank | bande proposte | numero | gruppi statici aggregati | gruppi proposti aggregati |
|---:|:---:|---:|---:|---:|
| 0 | 25:221 | 197 | 7,819,825 | 6,506,480 |
| 1 | 222:441 | 220 | 6,724,928 | 6,239,653 |
| 2 | 442:679 | 238 | 6,287,105 | 6,206,126 |
| 3 | 680:929 | 250 | 6,057,266 | 6,162,113 |
| 4 | 930:1185 | 256 | 5,859,218 | 6,101,508 |
| 5 | 1186:1448 | 263 | 5,702,256 | 6,052,253 |
| 6 | 1449:1721 | 273 | 5,534,698 | 5,972,543 |
| 7 | 1722:2000 | 279 | 5,443,476 | 5,928,185 |

| statistica | statico | proposto |
|:---|---:|---:|
| minimo | 5,443,476 | 5,928,185 |
| massimo | 7,819,825 | 6,506,480 |
| range | 2,376,349 | 578,295 |
| media | 6,178,596.5 | 6,146,107.625 |
| deviazione standard popolazione | 733,358.25 | 169,886.04 |
| CV | 11.869% | 2.764% |
| max/min | 1.4365 | 1.0976 |

Il massimo aggregato diminuisce di 1,313,345 gruppi, cioè 16.795%. La somma
proposta (49,168,861) è inferiore a quella statica (49,428,772) dello 0.526%:
è un effetto atteso della non additività, perché cambiare intervalli cambia
anche il numero totale di gruppi locali.

I confini sono quasi identici all'oracle offline: rank 0, 6 e 7 coincidono;
per i tagli intermedi la proposta sposta il limite di una sola banda verso
l'alto, salvo l'ultimo taglio a 1448 che coincide. La riduzione misurata del
16.795% conserva l'80.79% del limite piecewise-constant di 20.79% (scarto di
3.995 punti percentuali). L'approssimazione è quindi sorprendentemente accurata
come prima proposta, ma lascia un gradiente residuo monotono di circa 9.76%
fra rank 0 e rank 7. Una o poche iterazioni sui confini sono giustificate se si
vuole avvicinare il CV al rumore; non serve progettare subito un algoritmo
`cost(a,b,q)` completamente diverso.

### Timer: costo misurato e beneficio soltanto previsto

| run | `Xo (procedure)` min/max | range | CV | `Xo (REDUX)` min/max | prepass max | diagnostico intervalli max | Xo+prepass critico |
|:---|:---|---:|---:|:---|---:|---:|---:|
| T01 legacy | 3197.292 / 5736.258 s | 2538.967 s | 20.186% | 0.644 / 2549.431 s | 0 | 0 | 5736.258 s |
| T02 weighted | 3174.054 / 5797.772 s | 2623.718 s | 20.737% | 0.634 / 2627.305 s | 29.336 s | 0 | 5827.108 s |
| T03 diagnostico | 3158.617 / 5792.910 s | 2634.292 s | 20.812% | 0.644 / 2638.028 s | 62.629 s | 33.385 s | 5855.539 s |

Il timer del diagnostico è incluso nel timer del prepass, non va sommato una
seconda volta. Rispetto a T02 il costo effettivamente pagato dal prepass cresce
di 33.293 s, praticamente tutto spiegato dai 33.385 s del conteggio interval-
aware; il massimo `Xo (procedure)` produttivo cambia di soli -4.862 s, entro il
rumore fra run. Il cammino critico Xo+prepass peggiora quindi di 28.431 s
(0.49%) rispetto a T02 e di 119.281 s (2.08%) rispetto al legacy. Il tempo
totale arrotondato è 02h35m, contro 02h34m T02 weighted e 02h33m T01 legacy.
Questi sono i soli effetti temporali misurati: T03 non misura alcun beneficio
produttivo della proposta.

Usando soltanto come modello la relazione già osservata gruppi/timer, il fit
dei rank T03 è

```text
Xo (procedure) [s] = -2926.42 + 0.00110559 * gruppi aggregati
```

con R²=0.99556 ed errore RMS 54.2 s. Inserendo il massimo proposto di 6,506,480
si ottengono circa 4267 s, ossia ~1526 s (26.3%) meno del massimo produttivo
T03. È una stima controfattuale, non un tempo misurato. Assume che la relazione
resti lineare, che gruppi di intervalli diversi abbiano uguale costo medio e
che comunicazioni, GEMM, memoria e rumore restino comparabili. L'incertezza è
almeno dell'ordine dell'RMS del fit e può essere maggiore per effetti di
intervallo; il dato serve a motivare un test produttivo futuro, non a dichiarare
un'accelerazione conseguita.

### Memoria e scalabilità del diagnostico

Non compaiono warning di memoria, fallimenti di allocazione o picchi anomali.
Il report master registra massimi di 2.566206 GiB host e 2.350015 GiB device.
Le grandi allocazioni produttive `WF%c` restano proporzionali alle 245--249
bande statiche: al primo Xo il totale host tracciato varia circa da 2.411 a
2.445 GiB e il device da 2.304 a 2.337 GiB fra rank. La proposta non essendo
applicata non cambia queste allocazioni.

Le allocazioni temporanee del diagnostico non superano la soglia di stampa di
375.89 MiB e non lasciano crescita persistente visibile. Il timer interval-aware
è quasi identico su tutti i rank (33.3831--33.3845 s, range 1.4 ms), segno che
la fase collettiva scala in modo uniforme su questi otto rank. Costa lo 0.576%
del massimo `Xo (procedure)`; l'intero prepass costa l'1.081%. Non emerge un
problema di memoria o di squilibrio interno al prepass nel caso misurato.

### Equivalenza fisica

Tutti i confronti di `o-OUTPUT.qp` contengono gli stessi 420 stati, con identità
esatta delle coppie `(k,banda)` e della colonna `Eo`. Per le cinque colonne
numeriche `(k,banda,Eo,E-Eo,Sc|Eo)` le differenze massime/RMS sono:

| confronto T03 contro | k | banda | Eo | `E-Eo` max / RMS [eV] | `Sc|Eo` max / RMS [eV] |
|:---|:---:|:---:|:---:|:---:|:---:|
| T02 weighted | 0 / 0 | 0 / 0 | 0 / 0 | 8e-6 / 1.17e-6 | 1e-6 / 6.90e-8 |
| T01 weighted | 0 / 0 | 0 / 0 | 0 / 0 | 2.8e-5 / 6.41e-6 | 5e-6 / 7.62e-7 |
| T01 legacy | 0 / 0 | 0 / 0 | 0 / 0 | 1.47e-4 / 3.48e-5 | 2.8e-5 / 1.17e-5 |

Le differenze sono entro le tolleranze già adottate e della stessa scala dei
confronti T01/T02; non vi è regressione fisica.

### Errori, warning e decisione

La ricerca in report e otto log non trova errori, abort, NaN, segmentation
fault, `MPI_ABORT` o messaggi di failure. L'unico warning è `[x,Vnl]`, una volta
nel report e una volta per log, già noto e non collegato alla modifica. Il
messaggio provvisorio `247/1976` conserva la stessa ambiguità documentata in
T02, ma le righe `[X-WB]`, la corrispondenza esatta `[X-WB-I]`/`[X-CG]` e le
allocazioni delle funzioni d'onda confermano la maschera produttiva corretta.

I dati non giustificano fermare la linea né progettare ora una funzione di
costo radicalmente diversa. Giustificano l'adozione dell'approccio interval-
aware: la proposta è stabile su tutti i q, vicinissima all'oracle e conserva
un beneficio reale ampio dopo il ricalcolo non additivo. Tuttavia il residuo
CV 2.76% e il gradiente monotono suggeriscono, come passo tecnicamente più
solido, **una o poche iterazioni diagnostiche di raffinamento locale dei
confini**, ricalcolando i costi esatti degli intervalli adiacenti. Se si decide
di privilegiare semplicità e rapidità, applicare direttamente questa proposta
in un successivo test produttivo è già difendibile; l'analisi favorisce però
prima il raffinamento limitato. In accordo con il vincolo della sessione, non è
stata applicata la proposta, non è stato predisposto T04 e non è stato creato
alcun commit.

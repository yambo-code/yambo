# Progressi: distribuzione MPI contigua pesata delle bande di Xo

Ultimo aggiornamento: 2026-10-14.

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

## Raffinamento diagnostico successivo a T03

Avviata una singola iterazione di raffinamento, ancora non produttiva. Dai
costi esatti aggregati della proposta `[X-WB-I]` viene ricostruita una nuova
densità piecewise-constant; il partizionatore minimax produce una candidata
raffinata e un terzo conteggio degli intervalli completi ne misura il costo
reale. Le nuove righe `[X-WB-R]` riportano confronto per q, confini, totali e
un criterio esplicito di accettazione: la candidata è accettata soltanto se il
suo massimo aggregato esatto è inferiore a quello della proposta precedente.
La maschera produttiva `[X-WB]` resta invariata.

La build incrementale MPI con `profile/gcc-14.3.0` è completata. Un primo test
Al_bulk a quattro rank nella directory
`/home/nicola/tmp/codex/yambo5-xo-weighted-refine-local` è terminato
regolarmente. La partizione produttiva resta `2:6, 7:11, 12:15, 16:20`; la
prima proposta resta `2:5, 6:10, 11:15, 16:20`, massimo aggregato 704. Per la
granularità del caso minuscolo, la seconda iterazione torna alla partizione
produttiva e porta il massimo a 715: deve quindi essere marcata come rifiutata.
Il comportamento conferma la necessità del criterio di accettazione e non
indica un errore collettivo. L'output fisico conserva stati ed energie e mostra
soltanto variazioni di arrotondamento già osservate nei run precedenti.

Dopo l'aggiunta del criterio è stata ripetuta build e prova con job
`weighted_refine_accept_gcc143`: il report stampa correttamente
`refinement accepted F proposed aggregate max 704 refined aggregate max 715`.
Tutti i quattro rank raggiungono `Game Over`; il timer interval-aware è 0.0014
s e il prepass massimo 0.0032 s. Non sono comparsi errori, NaN o abort MPI.

L'analisi T03 è stata pubblicata nel commit documentale `41eea65cc`; il
raffinamento diagnostico e la relativa documentazione nel commit applicativo
`b4f568464c64e4a250d18b8fe6e6c05cec155fbc`. T04 è predisposto in
`/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata/`
`T04_anatase_interval_refine_5ebe585da_b4f568464` come singolo run weighted
diagnostico sull'input anatase completo. La richiesta mantiene esplicitamente
la partizione produttiva T02/T03 e raccoglie `[X-WB-R]`, timer, memoria,
`o-OUTPUT.qp`, report e tutti gli otto log per-rank.

## Analisi T04: seconda iterazione interval-aware

### Provenienza, artefatti e completamento

La provenienza attendibile dichiarata dall'utente è il binario costruito dal
commit applicativo `b4f568464c64e4a250d18b8fe6e6c05cec155fbc`; le stringhe
di branch, revisione e hash stampate da Yambo sono state ignorate. Il branch
di analisi è `tech-xo-weighted-band-distribution`, con HEAD al commit
documentale `758c4a478`; la cronologia rilevante è `41eea65cc` (analisi T03),
`b4f568464` (raffinamento diagnostico) e `758c4a478` (preparazione T04).
Prima dell'analisi il branch era allineato a `origin` e il worktree conteneva
soltanto i due file non tracciati `prossimo_prompt_post_T03.md` e
`prossimo_prompt_post_T04.md`, lasciati invariati.

La directory T04 contiene l'input effettivo, un report, `o-OUTPUT.qp` e tutti
gli otto log `CPU_1`--`CPU_8`. L'input ha SHA-256
`bde9fc279770e9480091c2dc8816c1cce8ecfd8ef449d3b5dafe054a53c46695`
ed è byte-identico a T03. Tutti gli otto log contengono una e una sola sezione
`Game Over`; il report registra inizio 2026-10-11 02:11, fine 04:47 e tempo
totale 02h35m (profilo 9326.1846 s). Non mancano artefatti e il run è
terminato regolarmente.

### Partizione produttiva e riproduzione integrale di T03

La partizione produttiva `[X-WB]` è ancora esattamente quella marginale di
T02 e T03, inclusi pesi marginali e transizioni accettate:

| c-rank | bande | numero | gruppi marginali | transizioni |
|---:|:---:|---:|---:|---:|
| 0 | 25:273 | 249 | 13,560,352 | 38,724,480 |
| 1 | 274:521 | 248 | 13,545,929 | 38,568,960 |
| 2 | 522:769 | 248 | 13,560,914 | 38,568,960 |
| 3 | 770:1017 | 248 | 13,570,588 | 38,568,960 |
| 4 | 1018:1262 | 245 | 13,530,561 | 38,102,400 |
| 5 | 1263:1507 | 245 | 13,544,664 | 38,102,400 |
| 6 | 1508:1754 | 247 | 13,535,283 | 38,413,440 |
| 7 | 1755:2000 | 246 | 13,563,924 | 38,257,920 |

Tutte le 240 coppie `(q,c-rank)` di `static exact groups` e tutte le 240 di
`proposed exact groups` coincidono esattamente con T03: zero valori diversi,
differenza massima zero e somma delle differenze zero. Coincidono anche i
confini della prima proposta `25:221, 222:441, 442:679, 680:929, 930:1185,
1186:1448, 1449:1721, 1722:2000`, i relativi numeri di bande
`197,220,238,250,256,263,273,279`, i massimi e le riduzioni di tutti i 30
q-point e i totali aggregati. Il commit di raffinamento non ha quindi alterato
alcun risultato della diagnostica precedente.

Inoltre i 240 `static exact groups` T04 coincidono uno per uno con i successivi
gruppi produttivi `[X-CG]`: 240/240 uguali, zero discrepanze, differenza
massima, somma assoluta e somma con segno tutte nulle. Questo, insieme alla
partizione `[X-WB]` invariata e alle allocazioni `WF%c` ancora proporzionali a
245--249 bande, conferma che né `[X-WB-I]` né `[X-WB-R]` è stato passato a
`PARALLEL_WF_distribute`.

### Risultati completi del raffinamento per q-point

Le celle elencano i rank 0--7. I massimi ricalcolati dai valori per-rank
coincidono esattamente con quelli stampati; le percentuali stampate coincidono
con il calcolo indipendente entro il solo arrotondamento a due decimali
(scarto massimo 0.00488 punti percentuali).

| q | proposed exact groups r0--r7 | refined exact groups r0--r7 | max proposto | max raffinato | riduzione |
|---:|:---|:---|---:|---:|---:|
| 1 | 96885,100681,104374,106184,106882,107712,108450,109190 | 92075,98854,103090,105469,107343,109171,110937,112470 | 109190 | 112470 | -3.00% |
| 2 | 256230,242346,238773,236019,233271,229937,226419,223948 | 245342,239315,237055,235389,234316,233045,230796,229414 | 256230 | 245342 | 4.25% |
| 3 | 249664,236620,233930,231093,228411,225652,222344,220076 | 238960,233785,231905,230580,229438,228689,226687,225545 | 249664 | 238960 | 4.29% |
| 4 | 179984,177922,179321,179346,178400,177981,176812,176038 | 171751,175328,177513,178758,179181,180386,180477,180819 | 179984 | 180819 | -0.46% |
| 5 | 247898,235541,232516,229933,227142,224903,221191,219092 | 237275,232503,230734,229166,228029,227763,225374,224433 | 247898 | 237275 | 4.29% |
| 6 | 279378,261064,256618,252493,248969,245800,241037,238323 | 267698,258026,254441,251944,250104,248972,245432,244022 | 279378 | 267698 | 4.18% |
| 7 | 284991,264864,259651,255494,251708,248220,242858,240217 | 273146,262156,257896,254786,252812,251425,247472,245878 | 284991 | 273146 | 4.16% |
| 8 | 310619,285188,277833,272901,267939,263572,257304,253681 | 298101,282335,276044,272157,269066,267015,261912,259544 | 310619 | 298101 | 4.03% |
| 9 | 189164,185428,186389,186478,185031,184548,183261,182106 | 180488,182673,184427,185806,185822,187038,187015,186896 | 189164 | 187038 | 1.12% |
| 10 | 245343,233198,231053,228674,225479,223464,219540,217675 | 234715,230353,229070,227950,226391,226515,223728,223082 | 245343 | 234715 | 4.33% |
| 11 | 292028,270778,264909,260808,256399,253002,247127,244134 | 280071,267932,263034,260068,257339,256297,251673,249869 | 292028 | 280071 | 4.09% |
| 12 | 187281,184038,185269,185245,183978,183201,181853,181037 | 178670,181340,183530,184338,184791,185697,185628,185877 | 187281 | 185877 | 0.75% |
| 13 | 131369,133286,136411,137983,138142,138882,138796,139269 | 124912,130999,134873,137359,138734,140792,141789,143253 | 139269 | 143253 | -2.86% |
| 14 | 138770,140371,143403,144095,144467,144468,144759,144667 | 132156,138053,141722,143524,145192,146499,147796,148907 | 144759 | 148907 | -2.87% |
| 15 | 238210,227386,225739,223113,221199,218694,215988,214250 | 227877,224559,223855,222473,222228,221666,220184,219640 | 238210 | 227877 | 4.34% |
| 16 | 249391,236694,233978,231164,228599,225617,222181,220112 | 238800,233752,231979,230581,229734,228558,226541,225509 | 249391 | 238800 | 4.25% |
| 17 | 235461,225351,223862,221361,219534,216843,214149,212087 | 225197,222546,221936,220713,220383,219823,218381,217477 | 235461 | 225197 | 4.36% |
| 18 | 191963,188137,188888,188826,187789,187031,185229,184427 | 183277,185395,187122,188069,188756,189384,188891,189254 | 191963 | 189384 | 1.34% |
| 19 | 247956,235374,232239,229920,227131,225161,221171,218912 | 237195,232567,230539,229057,228248,227994,225453,224269 | 247956 | 237195 | 4.34% |
| 20 | 310530,285001,277783,272628,268064,263660,257226,253749 | 297967,282067,275882,271881,269098,267066,261842,259650 | 310530 | 297967 | 4.05% |
| 21 | 229694,220506,219015,217424,215222,213439,210569,208734 | 219487,217568,216962,216789,216015,216246,214809,213957 | 229694 | 219487 | 4.44% |
| 22 | 186834,183575,184729,184831,183527,182780,181413,180888 | 178124,180960,182906,184057,184408,185162,185150,185715 | 186834 | 185715 | 0.60% |
| 23 | 245552,232978,230857,228609,225547,223313,219776,217526 | 235013,230139,229018,227846,226484,226276,224002,222901 | 245552 | 235013 | 4.29% |
| 24 | 176334,174163,175911,176180,174842,174797,173471,173321 | 168066,171588,174133,175523,175741,177016,177058,178010 | 176334 | 178010 | -0.95% |
| 25 | 233208,223404,221890,220211,217765,216213,212673,211212 | 223021,220478,219929,219541,218639,218875,216935,216523 | 233208 | 223021 | 4.37% |
| 26 | 138762,140289,143277,144394,144381,144806,144796,144824 | 132070,137979,141610,143739,144989,146654,147853,148954 | 144824 | 148954 | -2.85% |
| 27 | 256467,242362,238872,236054,233108,230097,226274,224117 | 245529,239516,237041,235547,234092,233148,230629,229481 | 256467 | 245529 | 4.26% |
| 28 | 192051,188270,189039,188796,187870,186880,185257,184435 | 183437,185552,187243,188167,188667,189178,189080,189232 | 192051 | 189232 | 1.47% |
| 29 | 188510,185048,186074,186355,184933,184621,182610,181726 | 179877,182258,184223,185493,185698,186992,186500,186503 | 188510 | 186992 | 0.81% |
| 30 | 95953,99790,103523,105501,105779,106959,108009,108412 | 91152,97896,102240,104914,106364,108403,110417,111671 | 108412 | 111671 | -3.01% |

Il beneficio per q ha media 2.080%, mediana 4.070%, minimo -3.006% e massimo
4.444%: 23 q sono positivi, nessuno nullo e 7 negativi (q=1, 4, 13, 14,
24, 26 e 30). Il segno misto non contraddice l'accettazione aggregata: una
partizione statica unica ottimizza il totale sui q, non ogni q separatamente.

### Confini, aggregati e criterio di accettazione

| c-rank | prima proposta | bande | raffinata | bande | gruppi proposti | gruppi raffinati |
|---:|:---:|---:|:---:|---:|---:|---:|
| 0 | 25:221 | 197 | 25:210 | 186 | 6,506,480 | 6,221,449 |
| 1 | 222:441 | 220 | 211:425 | 215 | 6,239,653 | 6,158,472 |
| 2 | 442:679 | 238 | 426:659 | 234 | 6,206,126 | 6,151,952 |
| 3 | 680:929 | 250 | 660:907 | 248 | 6,162,113 | 6,141,684 |
| 4 | 930:1185 | 256 | 908:1164 | 257 | 6,101,508 | 6,128,102 |
| 5 | 1186:1448 | 263 | 1165:1431 | 267 | 6,052,253 | 6,131,745 |
| 6 | 1449:1721 | 273 | 1432:1711 | 280 | 5,972,543 | 6,090,441 |
| 7 | 1722:2000 | 279 | 1712:2000 | 289 | 5,928,185 | 6,078,755 |

| statistica | prima proposta | raffinata |
|:---|---:|---:|
| minimo | 5,928,185 | 6,078,755 |
| massimo | 6,506,480 | 6,221,449 |
| range | 578,295 | 142,694 |
| media | 6,146,107.625 | 6,137,825.000 |
| deviazione standard popolazione | 169,886.04 | 41,078.81 |
| CV | 2.764% | 0.669% |
| max/min | 1.09755 | 1.02347 |
| somma totale | 49,168,861 | 49,102,600 |

Il massimo aggregato diminuisce di 285,031 gruppi, cioè 4.381%. La somma
totale diminuisce di 66,261 gruppi (-0.135%) passando dalla proposta al
raffinamento e di 326,172 (-0.660%) rispetto allo statico produttivo: non è
conservata perché il numero di gruppi locali non è additivo rispetto ai tagli.

La riga riepilogativa stampa
`refinement accepted T proposed aggregate max 6506480 refined aggregate max
6221449`. Il calcolo indipendente sui 240 valori produce gli stessi massimi e
il booleano `6221449 < 6506480` è vero. Il sorgente usa esattamente questa
condizione stretta; il criterio è stato quindi applicato correttamente.

I sette confini superiori si spostano dalla prima proposta di
`-11,-16,-20,-22,-21,-17,-10` bande. Rispetto alla partizione produttiva gli
spostamenti totali sono `-63,-96,-110,-110,-98,-76,-43`, tutti nella stessa
direzione: la seconda iterazione non torna verso la partizione marginale e
non oscilla. I numeri di bande cambiano dalla proposta al raffinamento di
`-11,-5,-4,-2,+1,+4,+7,+10`, trasferendo ancora lavoro dai rank bassi a quelli
alti. I carichi raffinati sono quasi monotoni decrescenti; l'unica inversione
è il piccolo aumento rank 4->5 di 3,643 gruppi (0.059%). Il gradiente fra gli
estremi è ormai 2.29%, contro 9.76% della proposta, e il CV scende sotto l'1%.
Questa è evidenza di convergenza utile, non di oscillazione. Una terza
iterazione avrebbe al più un margine residuo piccolo e non è giustificata dal
costo di un ulteriore conteggio o dal rischio di sovracorrezione per i q già
negativi.

### Timer misurati e stime controfattuali

| run | `Xo (procedure)` min/max | range | CV | `Xo (REDUX)` min/max | diagnostico max | prepass max | Xo+prepass critico |
|:---|:---|---:|---:|:---|---:|---:|---:|
| T01 legacy | 3197.292 / 5736.258 s | 2538.967 s | 20.186% | 0.644 / 2549.431 s | 0 | 0 | 5736.258 s |
| T02 weighted | 3174.054 / 5797.772 s | 2623.718 s | 20.737% | 0.634 / 2627.305 s | 0 | 29.336 s | 5827.108 s |
| T03 prima proposta | 3158.617 / 5792.910 s | 2634.292 s | 20.812% | 0.644 / 2638.028 s | 33.385 s | 62.629 s | 5855.538 s |
| T04 raffinamento | 3169.932 / 5785.373 s | 2615.441 s | 20.749% | 0.640 / 2618.802 s | 51.870 s | 81.056 s | 5866.428 s |

Il timer diagnostico è incluso nel prepass. Il terzo conteggio aggiunge
direttamente circa 18.485 s al diagnostico (51.8698-33.3845 s) e 18.427 s al
massimo del prepass (81.0562-62.6288 s); la differenza di 0.058 s è rumore
nelle altre parti del prepass. `Xo (procedure)` produttivo, che usa ancora la
stessa partizione, cambia di -7.537 s al massimo rispetto a T03; i valori dei
singoli rank oscillano sia in aumento sia in diminuzione. `Xo (REDUX)` cambia
coerentemente in senso opposto. Queste variazioni sono normale rumore fra run,
non effetto del raffinamento diagnostico. Il cammino critico misurato peggiora
di 10.890 s rispetto a T03, 39.320 s rispetto a T02 e 130.170 s rispetto al
legacy; il tempo totale arrotondato resta 02h35m, come T03.

Sul T04 il fit lineare fra gruppi produttivi aggregati e timer per rank è

```text
Xo (procedure) [s] = -2907.98 + 0.00110283 * gruppi aggregati
```

con R²=0.99583 ed errore RMS 52.33 s. Inserendo il massimo della prima
proposta, 6,506,480, si stimano 4267.6 s; inserendo quello raffinato,
6,221,449, si stimano 3953.2 s. Rispetto al massimo produttivo T04 di 5785.4 s
sono riduzioni previste rispettivamente di circa 1517.8 s (26.24%) e 1832.2 s
(31.67%); il vantaggio incrementale previsto del raffinamento è circa 314.3 s
(7.36% rispetto alla stima della prima proposta). Non sono tempi misurati.
Il modello assume linearità invariata, uguale costo medio per gruppo dopo il
cambio degli intervalli e costi comparabili di GEMM, comunicazioni e memoria.
L'incertezza statistica minima è dell'ordine dei 52 s RMS, ma quella reale può
essere maggiore perché il fit ha soltanto otto rank e non osserva ancora i
nuovi intervalli in produzione.

### Memoria e scalabilità del terzo conteggio

Il timer interval-aware T04 è 51.8673--51.8698 s sugli otto rank, range 2.5 ms;
il prepass è 81.0549--81.0562 s, range 1.3 ms. La fase è quindi uniforme e non
introduce uno squilibrio MPI visibile. Il terzo conteggio da circa 18.485 s
vale lo 0.32% del massimo `Xo (procedure)`; l'intero diagnostico vale lo 0.90%
e il prepass completo l'1.40%. Il suo costo è piccolo rispetto al beneficio
produttivo previsto, ma non serve mantenerlo come ulteriore iterazione una
volta scelti i confini.

Non compaiono warning di memoria, fallimenti di allocazione o crescita
persistente. Il massimo master è 2.566209 GiB host e 2.350015 GiB device,
contro 2.566206/2.350015 GiB in T03: differenza host trascurabile e device
identico. Le allocazioni produttive iniziali `WF%c` variano secondo le
245--249 bande statiche da circa 2.411 a 2.445 GiB host e 2.304 a 2.337 GiB
device, come T03; le temporanee del diagnostico non superano la soglia di
stampa di 375.89 MiB e vengono deallocate prima del calcolo produttivo.

### Equivalenza fisica e anomalie

Tutti gli `o-OUTPUT.qp` confrontati contengono gli stessi 420 stati, con
identità esatta delle coppie `(k,banda)` e di `Eo`. Le differenze massime/RMS
per le cinque colonne numeriche sono:

| confronto T04 contro | k | banda | Eo | `E-Eo` max / RMS [eV] | `Sc|Eo` max / RMS [eV] |
|:---|:---:|:---:|:---:|:---:|:---:|
| T03 | 0 / 0 | 0 / 0 | 0 / 0 | 7e-6 / 1.45e-6 | 1e-6 / 2.24e-7 |
| T02 weighted | 0 / 0 | 0 / 0 | 0 / 0 | 8e-6 / 1.73e-6 | 1e-6 / 2.34e-7 |
| T01 weighted | 0 / 0 | 0 / 0 | 0 / 0 | 2.7e-5 / 6.32e-6 | 5e-6 / 7.32e-7 |
| T01 legacy | 0 / 0 | 0 / 0 | 0 / 0 | 1.54e-4 / 3.49e-5 | 2.9e-5 / 1.17e-5 |

Le differenze sono entro le tolleranze già usate e coerenti con il diverso
ordine delle riduzioni MPI; non emerge alcuna regressione fisica.

La ricerca nel report e negli otto log non trova errori, abort, NaN,
segmentation fault, `MPI_ABORT`, failure o anomalie MPI. L'unico warning è il
preesistente `[x,Vnl]`, una volta nel report e una volta in ciascun log. Le
righe `Time ordering fails/rest` sono normali statistiche numeriche, non
messaggi di fallimento. Non vi sono messaggi diagnostici incoerenti: i massimi,
la percentuale, i totali e il booleano di accettazione sono internamente
coerenti.

### Decisione dopo T04

Il raffinamento supera entrambi i criteri concordati: riduce il massimo del
4.381%, ben oltre circa l'1%, e porta il CV allo 0.669%, sotto l'intervallo
obiettivo 1--1.5%. Va quindi preferito alla prima proposta T03. La seconda
iterazione si muove in modo coerente, non torna verso la partizione marginale
e lascia soltanto un residuo molto piccolo; non vi è evidenza per una terza
iterazione né per progettare ora un diverso `cost(a,b,q)`.

La partizione raffinata è sufficientemente bilanciata, stabile e verificata
per essere la candidata da applicare produttivamente in un test successivo.
Resta però una previsione: T04 non misura il tempo con questi confini. Prima di
concludere sulle prestazioni servirà un run produttivo dedicato che applichi i
confini raffinati e confronti timer, memoria e output fisico. In accordo con il
vincolo della sessione, tale applicazione non è stata implementata, il test
successivo non è stato preparato e non è stato creato alcun commit.

## Applicazione produttiva successiva a T04

Dopo la discussione dei risultati T04 è stata autorizzata l'applicazione
produttiva della migliore partizione interval-aware. `X_weighted_bands_setup`
installa ora `refined_partition` in `PAR_IND_CON_BANDS_X` quando
`refined_aggregate_max < proposed_aggregate_max`; in caso contrario installa
`q_partition`, cioè la prima proposta interval-aware. La copia avviene su ogni
rank prima di `PARALLEL_WF_distribute` usando gli helper esistenti
`PP_indexes_reset` e `PAR_INDEX_copy`.

Per eliminare l'ambiguità dei report, `[X-WB-M]` identifica ora la partizione
marginale iniziale, mentre `[X-WB]` riporta esclusivamente intervalli, conteggi
e sorgente della maschera produttiva finale. `[X-WB-I]` e `[X-WB-R]` restano
le misure esatte usate per costruire e accettare la scelta.

La build MPI incrementale con `profile/gcc-14.3.0` è completata. Il test locale
Al_bulk a quattro rank, job `weighted_production_refined_gcc143` nella
directory `/home/nicola/tmp/codex/yambo5-xo-weighted-refine-local`, è terminato
regolarmente su tutti i rank. In questo piccolo caso il raffinamento torna alla
partizione marginale e viene correttamente rifiutato (`704 -> 715`); il ramo di
fallback installa produttivamente la prima proposta
`2:5, 6:10, 11:15, 16:20`. I successivi gruppi `[X-CG]` coincidono per tutti
gli otto q-point e quattro rank con i `proposed exact groups`, provando che la
nuova maschera è effettivamente passata alla distribuzione delle funzioni
d'onda. Non compaiono errori, NaN o abort MPI.

L'output locale conserva gli stessi 32 stati e identiche colonne `(k,banda,Eo)`
del precedente diagnostico. Il diverso ordine produttivo delle riduzioni porta
differenze massime/RMS di 1.0e-4/4.34e-5 eV in `E-Eo` e 3.0e-5/5.88e-6 eV in
`Sc|Eo`, della scala già osservata nei confronti fra partizioni MPI diverse.

Il ramo accettato è lo stesso percorso di copia, selezionato dal booleano già
verificato in T04; la sua efficacia prestazionale su anatase resta da misurare
nel test produttivo T05.

L'analisi T04 è stata pubblicata nel commit documentale `6febb1d79`; la
selezione produttiva nel commit applicativo
`9e36122b73d7d04d379139a3174c787509d459b9`. T05 è predisposto in
`/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata/`
`T05_anatase_refined_production_b4f568464_9e36122b7`, con manifest, istruzioni
di copia, input anatase verificato e directory vuote per report, output e otto
log. La richiesta usa esplicitamente l'hash applicativo pubblicato e richiede
di ignorare i metadati Git stampati dal report.

## Analisi T05: partizione raffinata in produzione

La provenienza attendibile è il binario del commit applicativo
`9e36122b73d7d04d379139a3174c787509d459b9`; le stringhe Git stampate da
Yambo sono state ignorate. Il branch corrente è
`tech-xo-weighted-band-distribution` a `7eabcf15f`, allineato a `origin`.
Prima dell'analisi erano presenti soltanto i tre prompt non tracciati
`prossimo_prompt_post_T03.md`, `prossimo_prompt_post_T04.md` e
`prossimo_prompt_post_T05.md`.

T05 contiene un input, un report, `o-OUTPUT.qp` e gli otto log `CPU_1`--`CPU_8`.
L'input pesa 3013 byte e ha SHA-256
`bde9fc279770e9480091c2dc8816c1cce8ecfd8ef449d3b5dafe054a53c46695`,
identico byte per byte a T04, T03, T02 weighted e T01 weighted. Ogni log ha
una sola sezione `Game Over`; il report termina regolarmente in 02h06m
(7598.3286 s sul master).

Il prepass riproduce integralmente T04. `[X-WB-M]` coincide numericamente con
la partizione marginale T02--T04: confini `25:273,274:521,522:769,770:1017,
1018:1262,1263:1507,1508:1754,1755:2000`, bande
`249,248,248,248,245,245,247,246`, pesi `13560352,13545929,13560914,
13570588,13530561,13544664,13535283,13563924` e transizioni
`38724480,38568960,38568960,38568960,38102400,38102400,38413440,38257920`.
Tutte le 286 righe `[X-WB-I]` e tutte le 287 `[X-WB-R]` sono identiche a T04
dal marcatore in poi, comprese le 240 coppie q/rank, confini, massimi per q,
totali e accettazione. Per ciascuno dei tre confronti: differenze 0, massimo
0, somma assoluta 0 e somma con segno 0.

I massimi aggregati ricalcolati sono 6,506,480 (proposto) e 6,221,449
(raffinato); `6221449 < 6506480` conferma `refinement accepted T`.
`production partition source refined interval-aware` installa gli intervalli
`25:210,211:425,426:659,660:907,908:1164,1165:1431,1432:1711,1712:2000`,
con `186,215,234,248,257,267,280,289` bande. Copertura 25:2000 completa,
contigua, ordinata e senza sovrapposizioni. Le 240 righe `[X-CG]` coincidono
esattamente con `refined exact groups`: discrepanze 0, massimo 0, somma
assoluta 0 e somma con segno 0. La maschera raffinata è quindi stata realmente
usata in produzione.

### Gruppi produttivi completi

CV usa la deviazione standard di popolazione.

|q|r0|r1|r2|r3|r4|r5|r6|r7|min|max|range|CV|max/min|
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|1|92075|98854|103090|105469|107343|109171|110937|112470|92075|112470|20395|6.052%|1.222|
|2|245342|239315|237055|235389|234316|233045|230796|229414|229414|245342|15928|2.016%|1.069|
|3|238960|233785|231905|230580|229438|228689|226687|225545|225545|238960|13415|1.731%|1.059|
|4|171751|175328|177513|178758|179181|180386|180477|180819|171751|180819|9068|1.640%|1.053|
|5|237275|232503|230734|229166|228029|227763|225374|224433|224433|237275|12842|1.681%|1.057|
|6|267698|258026|254441|251944|250104|248972|245432|244022|244022|267698|23676|2.819%|1.097|
|7|273146|262156|257896|254786|252812|251425|247472|245878|245878|273146|27268|3.221%|1.111|
|8|298101|282335|276044|272157|269066|267015|261912|259544|259544|298101|38557|4.256%|1.149|
|9|180488|182673|184427|185806|185822|187038|187015|186896|180488|187038|6550|1.197%|1.036|
|10|234715|230353|229070|227950|226391|226515|223728|223082|223082|234715|11633|1.540%|1.052|
|11|280071|267932|263034|260068|257339|256297|251673|249869|249869|280071|30202|3.490%|1.121|
|12|178670|181340|183530|184338|184791|185697|185628|185877|178670|185877|7207|1.290%|1.040|
|13|124912|130999|134873|137359|138734|140792|141789|143253|124912|143253|18341|4.213%|1.147|
|14|132156|138053|141722|143524|145192|146499|147796|148907|132156|148907|16751|3.659%|1.127|
|15|227877|224559|223855|222473|222228|221666|220184|219640|219640|227877|8237|1.105%|1.038|
|16|238800|233752|231979|230581|229734|228558|226541|225509|225509|238800|13291|1.723%|1.059|
|17|225197|222546|221936|220713|220383|219823|218381|217477|217477|225197|7720|1.034%|1.035|
|18|183277|185395|187122|188069|188756|189384|188891|189254|183277|189384|6107|1.081%|1.033|
|19|237195|232567|230539|229057|228248|227994|225453|224269|224269|237195|12926|1.670%|1.058|
|20|297967|282067|275882|271881|269098|267066|261842|259650|259650|297967|38317|4.230%|1.148|
|21|219487|217568|216962|216789|216015|216246|214809|213957|213957|219487|5530|0.730%|1.026|
|22|178124|180960|182906|184057|184408|185162|185150|185715|178124|185715|7591|1.321%|1.043|
|23|235013|230139|229018|227846|226484|226276|224002|222901|222901|235013|12112|1.563%|1.054|
|24|168066|171588|174133|175523|175741|177016|177058|178010|168066|178010|9944|1.784%|1.059|
|25|223021|220478|219929|219541|218639|218875|216935|216523|216523|223021|6498|0.877%|1.030|
|26|132070|137979|141610|143739|144989|146654|147853|148954|132070|148954|16884|3.696%|1.128|
|27|245529|239516|237041|235547|234092|233148|230629|229481|229481|245529|16048|2.049%|1.070|
|28|183437|185552|187243|188167|188667|189178|189080|189232|183437|189232|5795|1.040%|1.032|
|29|179877|182258|184223|185493|185698|186992|186500|186503|179877|186992|7115|1.253%|1.040|
|30|91152|97896|102240|104914|106364|108403|110417|111671|91152|111671|20519|6.178%|1.225|

Le somme sui 30 q sono `6221449,6158472,6151952,6141684,6128102,6131745,
6090441,6078755`: minimo 6,078,755, massimo 6,221,449, range 142,694,
media 6,137,825, deviazione standard 41,078.81, CV 0.6693%, max/min
1.02347 e totale 49,102,600. Vi è una sola inversione residua r4->r5 di
3,643 gruppi (0.059%). L'obiettivo sotto circa 1--1.5% è raggiunto.

Dal proposto al raffinato il massimo scende di 285,031 (-4.381%) e il CV da
2.764% a 0.669%; dalla partizione marginale T02/T04 il massimo scende da
7,819,825 (-20.44%) e il CV da 11.869%. T01 weighted aveva 7,819,825 e
11.832%, T01 legacy 7,771,736 e 11.525%. Il totale cambia da 49,428,772
marginale a 49,168,861 proposto e 49,102,600 raffinato: la piccola variazione
del totale misura la non additività dei nuovi tagli, mentre la riduzione del
massimo misura il beneficio dei confini.

### Timer, previsione e costi secondari

|rank|diagnostico|prepass|Xo procedure|Xo REDUX|procedure+prepass|
|---:|---:|---:|---:|---:|---:|
|0|51.1647|80.3495|3794.8725|269.6361|3875.2220|
|1|51.1658|80.3490|3768.3453|298.5503|3848.6943|
|2|51.1661|80.3494|3811.8767|254.8190|3892.2261|
|3|51.1662|80.3494|3817.0516|249.5409|3897.4010|
|4|51.1667|80.3499|3823.9315|241.9001|3904.2814|
|5|51.1667|80.3500|3840.6933|225.0917|3921.0433|
|6|51.1666|80.3498|3847.5258|218.2269|3927.8756|
|7|51.1659|80.3491|3879.2430|186.4786|3959.5921|

Il diagnostico varia 2.0 ms e il prepass 1.0 ms. Il massimo `Xo (procedure)`
è 3879.243 s, range 110.898 s e CV 0.828%, contro
5785.373/2615.441/20.749% in T04. `Xo (REDUX)` è 186.479--298.550 s, range
112.072 s: l'attesa massima scende di 2320.252 s (-88.60%) e diventa molto
più uniforme. Il rank critico della procedura passa da r0 a r7; il massimo
REDUX è r1. Il lieve gradiente residuo è opposto a quello dei gruppi.

Il cammino critico completo è 3959.592 s. Rispetto a T04 migliora di
1906.836 s (-32.50%); rispetto a T03, T02, T01 weighted e T01 legacy di
1895.946 (-32.38%), 1867.515 (-32.05%), 1866.814 (-32.04%) e 1776.665 s
(-30.97%). Il massimo della sola procedura migliora rispettivamente di
1906.130, 1913.667, 1918.529, 1937.633 e 1857.015 s. Il tempo totale passa da
9326.199 s T04 a 7598.343 s: -1727.856 s (-18.53%); contro T03, T02, T01
weighted e legacy: -1711.525 (-18.38%), -1698.163 (-18.27%), -1700.342
(-18.29%) e -1609.734 s (-17.48%).

Il prepass costa 80.350 s (2.07% del massimo produttivo), 0.706 s meno di T04;
il diagnostico è 0.703 s più rapido. Rispetto al legacy il break-even richiede
di conservare appena 80.350/1857.015 = 4.33% del risparmio Xo: il beneficio
osservato è 23.11 volte il costo. Rispetto a T04 il risparmio della procedura
supera il prepass intero di 1825.780 s.

La previsione T04 di 3953.2 s non era una misura. Il massimo T05 osservato,
3879.243 s, è 73.96 s più basso (-1.87%), circa 1.41 volte l'RMS storico di
52.33 s: il modello ha previsto bene il cammino critico assoluto.

Il nuovo fit univariato T05 sugli otto rank è:

```text
Xo (procedure) [s] = 7579.2344 - 0.000611991 * gruppi aggregati
```

R²=0.63074, RMS=19.235 s; residui r0--r7
`+23.107,-41.961,-2.420,-3.529,-4.961,+14.030,-4.415,+20.150` s. Il segno
negativo non significa che più gruppi accelerino Xo: il range dei gruppi è
ormai solo 2.35%, mentre le bande crescono da 186 a 289 e sono anticorrelate
ai gruppi. Un fit separato dà `time=3609.697+0.863343*bande`, R²=0.76878 e
RMS=15.221 s. Nel residuo dominano quindi lavoro per banda,
transizioni/GEMM/accessi alle WF e costi fissi. Il modello gruppi/tempo T03/T04
resta valido per il grande guadagno e il massimo assoluto, ma non discrimina
più i piccoli scarti nel regime quasi bilanciato.

### Memoria, output fisico e anomalie

Le allocazioni Xo iniziali `WF%c` r0--r7 sono `1.768774,2.013033,2.173065,
2.290983,2.366788,2.451015,2.560511,2.636315` GiB, su host e device, coerenti
con le bande crescenti. T04 era circa 2.27--2.30 GiB: T05 riduce i rank bassi
e porta il massimo a r7, circa +14.7% rispetto al massimo Xo T04. Il totale
tracciato al massimo è circa 2.782 GiB host e 2.674 GiB device sul rank 7. Le
allocazioni successive `WF%c` (0.80--1.05 GiB e 2.223601 GiB) sono liberate
normalmente; nessuna crescita persistente. Il report master indica picchi
memstat 2.165179 GiB host e 2.160261 GiB device. Nessun warning di memoria.

Tutti gli `o-OUTPUT.qp` contengono gli stessi 420 stati, con identità esatta
di `k`, banda ed `Eo`. Differenze T05 massimo/RMS:

|contro|`E-Eo` [eV]|`Sc|Eo` [eV]|
|:---|:---:|:---:|
|T04|7.56e-4 / 2.870e-4|4.07e-4 / 3.157e-4|
|T03|7.55e-4 / 2.870e-4|4.07e-4 / 3.157e-4|
|T02 weighted|7.56e-4 / 2.869e-4|4.07e-4 / 3.156e-4|
|T01 weighted|7.48e-4 / 2.870e-4|4.06e-4 / 3.157e-4|
|T01 legacy|7.30e-4 / 2.808e-4|3.95e-4 / 3.055e-4|

Le differenze aumentano rispetto ai confronti con uguali confini, come atteso
dal cambiamento maggiore dell'ordine delle riduzioni MPI, ma restano sub-meV
e dentro le tolleranze fisiche già usate. Nessuno stato mancante o valore
fuori scala.

La ricerca integrale non trova errori, abort, NaN, segmentation fault,
failure o anomalie MPI. L'unico warning è `[x,Vnl]`, una volta nel report e
una per log. `Time ordering fails/rest` è una statistica normale. I marcatori
`[X-WB-M]`, `[X-WB-I]`, `[X-WB-R]` e `[X-WB]` sono coerenti.

### Decisione dopo T05

La selezione produttiva è riuscita: conteggi previsti riprodotti, partizione
realmente usata, CV 0.669%, massimo Xo entro 1.87% dalla previsione, guadagno
netto materiale anche col prepass ed equivalenza fisica. Non è giustificata
una terza iterazione.

Per ora conviene mantenere i tre conteggi nella versione di validazione: T05
è un solo sistema e 80 s sono piccoli rispetto ai 1857--1906 s risparmiati.
Dopo almeno un secondo benchmark e un caso con raffinamento rifiutato, si può
rendere opzionale la diagnostica dettagliata o evitare conteggi ridondanti,
preservando ciò che serve alla selezione. Prima dell'integrazione servono:
regressione col flag disabilitato, build CPU-only/MPI senza GPU, caso con
gerarchia `q/g/k/c` non banale, stress memoria con più bande sul rank finale e
verifica tester delle tolleranze QP. Non sono stati modificati algoritmo o
diagnostica, preparato T06 né creati commit.

## Piano di validazione successivo a T05

Prima dei nuovi benchmark va corretta l'osservabilità della distribuzione.
Il riepilogo `247/1976` emesso da `PARALLEL_global_indexes` descrive la
maschera equinumerosa costruita prima del prepass, non quella passata a
`PARALLEL_WF_distribute`. Con `X_WeightedBands` attivo il messaggio iniziale
deve quindi essere marcato come provvisorio e, dopo la selezione interval-aware,
deve essere emesso un secondo riepilogo parallelo con il conteggio finale
locale. Il percorso e il messaggio legacy restano invariati col flag spento.

La correzione di reporting precede la validazione, nell'ordine:

1. verificare localmente coerenza fra riepilogo finale, `n_of_elements`,
   intervalli `[X-WB]` e somma globale delle bande, sia per raffinamento
   accettato sia per fallback;
2. regressione col flag disabilitato;
3. build e prova MPI CPU-only;
4. caso con gerarchia `q/g/k/c` non banale;
5. secondo benchmark rappresentativo, controllo memoria e verifica tester
   delle tolleranze QP;
6. soltanto dopo questi dati, decidere se ridurre o rendere opzionale la
   diagnostica dettagliata.

### Correzione del reporting e verifica locale

Il reporting è stato corretto senza cambiare la partizione. Quando
`X_WeightedBands` è attivo, il riepilogo prodotto durante
`PARALLEL_global_indexes` usa ora l'etichetta `CON bands (provisional)`.
Subito dopo la copia della partizione interval-aware selezionata viene emesso
su ogni rank `CON bands [X-WB final]`, usando il `n_of_elements` della maschera
che sarà passata a `PARALLEL_WF_distribute`. Col flag disabilitato resta
esattamente l'etichetta storica `CON bands` e non viene aggiunto alcun secondo
messaggio.

La build MPI con `profile/gcc-14.3.0` e `make yambo` è completata. Il caso
locale Al_bulk a quattro rank è stato rilanciato come
`weighted_reporting_gcc143` in
`/home/nicola/tmp/codex/yambo5-xo-weighted-refine-local`. Il raffinamento è
stato correttamente rifiutato (`704 -> 715`) e il fallback ha prodotto
`2:5,6:10,11:15,16:20`. I log mostrano conteggi provvisori `5,5,5,4` e
conteggi finali `4,5,5,5` su 19 bande, in esatta corrispondenza con gli
intervalli `[X-WB]`; la somma finale è 19. Tutti i rank terminano con
`Game Over`. L'output QP differisce dal precedente run soltanto per normali
arrotondamenti nell'ultima cifra stampata e per intestazione/data.

È stato inoltre eseguito `reporting_legacy_gcc143` con lo stesso input privo
di `X_WeightedBands`. I quattro log contengono soltanto il messaggio storico
con `5,5,5,4/19`, nessuna stringa `provisional`, `final` o `[X-WB]`, e tutti
terminano regolarmente. Il ramo con raffinamento accettato attraversa lo
stesso punto di reporting comune dopo le due alternative di copia; la sua
verifica runtime esplicita resta associata al prossimo benchmark che lo
seleziona naturalmente, senza forzare o modificare l'algoritmo.

### Validazione locale MPI CPU-only

È stata creata una worktree isolata in
`/home/nicola/tmp/codex/yambo5-xo-weighted-cpu-local`, basata su `7eabcf15f`
e con le sole modifiche sorgente correnti riportate localmente. La
configurazione usa GCC 14.3.0, OpenMPI 5.0.8, OpenMP, ScaLAPACK, SLEPc e HDF5
parallelo; `config/report` conferma `GPU support ( no_gpu )` e non contiene
macro CUDA. `make yambo` è completato.

Sul database Al_bulk sono stati eseguiti due run con quattro rank MPI,
`OMP_NUM_THREADS=1` e job directory distinte: `cpu_only_weighted_gcc143`, con
`X_WeightedBands`, e `cpu_only_legacy_gcc143`, col flag assente. Entrambi
terminano con `Game Over` su tutti i rank e senza errori, NaN o abort MPI.

Il weighted riproduce i precedenti conteggi locali: marginale
`2:6,7:11,12:15,16:20`, proposta `2:5,6:10,11:15,16:20`, massimo proposto
704, raffinato 715 e fallback correttamente selezionato. Le 32 coppie q/rank
produttive coincidono con i `proposed exact groups`. Il reporting mostra
`5,5,4,5/19` provvisorio e `4,5,5,5/19` finale; il legacy mostra soltanto il
messaggio storico `5,5,5,4/19`.

| variante | `Xo (procedure)` r0--r3 [s] | `Xo (REDUX)` r0--r3 [s] | diagnostico [s] | prepass [s] |
|:---|:---|:---|:---|:---|
| legacy | 0.0538, 0.0553, 0.0551, 0.0519 | 0.0084, 0.0057, 0.0059, 0.0086 | -- | -- |
| weighted | 0.0462, 0.0566, 0.0593, 0.0549 | 0.0173, 0.0068, 0.0033, 0.0028 | 0.0014 su tutti | 0.0033, 0.0028, 0.0028, 0.0028 |

La scala di pochi millisecondi rende i timer dominati da costi fissi e rumore:
il caso verifica correttezza e portabilità CPU, non il guadagno prestazionale.
Gli `o-*.qp` hanno gli stessi 32 stati e identiche colonne `(k,banda,Eo)`;
weighted contro legacy differisce al massimo/RMS di `1.0e-4/5.61e-5` eV
nella larghezza e `3.0e-5/5.60e-6` eV nell'ultima colonna, coerentemente col
diverso ordine delle riduzioni MPI.

La validazione locale CPU-only è superata. La misura prestazionale CPU resta
demandata ai due run X-only legacy/weighted di anatase su Leonardo-DCGP. Se
non verrà prodotto `o-OUTPUT`, l'equivalenza fisica sarà verificata sui
database NetCDF e sui frammenti del driver X, separando metadati e ordine dei
frammenti dai valori fisici confrontabili.

## Analisi T06: anatase CPU-only X-only su Leonardo-DCGP

### Provenienza, artefatti e confrontabilità

La provenienza attendibile dichiarata dall'operatore è branch `5.4` per i due
run legacy e commit pubblicato
`bb316782914745149558a48802e572865550448f` per il candidato weighted. Le
stringhe Git dei report non sono state usate. La sessione contiene input,
report, tutti i log per-rank e `ndb.pp` più 30 frammenti per ciascun run. Il
calcolo comune dei dipoli contiene input, report, 16 log e `ndb.dipoles`.
L'assenza di `o-OUTPUT` è attesa per questo X-only e non è una mancanza.

I checksum cluster/copia/ricomputati coincidono senza eccezioni: 20/20 file
dipoli, 51/51 file del run 16x14 e 43/43 file per ciascuno dei due run 8x1.
Il database X principale `ndb.pp` e `ndb.RIM` sono byte-identici fra tutti i
run; i frammenti differiscono perché cambia l'ordine delle riduzioni. Il
report registra esplicitamente la scrittura `[WR./OUTPUT//ndb.pp_fragment_q]`
e tutti i frammenti hanno checksum propri: non emerge riuso di un database X
preesistente. Il database dipoli comune ha SHA-256
`6f25138834a404ef925e5b3b68ab2a443d62acd1c4929e01d61355cd1a0f8816`.

Baseline e candidato sono direttamente confrontabili: un nodo, otto rank,
un thread per rank, gerarchia `q/g/k/c/v=1/1/1/8/1`, build
MPI+OpenMP+ScaLAPACK+SLEPc+HDF5 parallel I/O, precisione single e medesimo
input fisico. Il `diff` degli input contiene soltanto `X_WeightedBands` nel
candidato. Entrambi gli script usano lo stesso modulo Intel oneAPI 2024.1,
Intel MPI 2021.12.1, `OMP_NUM_THREADS=1`, `srun -n 8 --cpu-bind=cores` e la
stessa allocazione esclusiva DCGP. `--cpus-per-task=14` riserva un core group
intero ma Yambo conferma un solo thread; non sono disponibili mappe hwloc
indipendenti, tuttavia report, log e comando sono coerenti.

Tutti gli otto log principali e i sedici log di validazione contengono una
sola `Game Over`. Non compaiono errori, abort, NaN, problemi HDF5/NetCDF o
warning nei run 8x1. Il run 16x14 termina regolarmente in 2288.399 s, ma
riporta 240 `Empty workload` (15 per rank): è una caratteristica/anomalia del
percorso legacy ibrido con 16 rank, non un problema introdotto dal weighted.
Poiché cambiano insieme nodi, rank e thread, i suoi 2268.963 s massimi di
`Xo (procedure)` non costituiscono uno scaling controllato rispetto a 8x1.

### Reporting, prepass e partizione realmente usata

Nel weighted i conteggi provvisori sono `247` su tutti i rank e quelli
`CON bands [X-WB final]` sono `186,215,234,248,257,267,280,289`; coincidono
con `n_of_elements`, con `[X-WB]` e sommano a 1976 bande (25:2000). Nei due
legacy resta esclusivamente il messaggio storico `CON bands`, senza marcatori
weighted. La copertura finale è completa, contigua, ordinata e senza
sovrapposizioni.

Il confronto integrale del prepass T06 contro T05 dà:

| famiglia | righe | numeri | differenze | massimo | somma assoluta | somma con segno |
|:---|---:|---:|---:|---:|---:|---:|
| `[X-WB-M]` | 16 | 48 | 0 | 0 | 0 | 0 |
| `[X-WB-I]` | 286 | 1128 | 0 | 0 | 0 | 0 |
| `[X-WB-R]` | 287 | 1130 | 0 | 0 | 0 | 0 |
| `[X-WB]` | 9 | 32 | 0 | 0 | 0 | 0 |

Sono quindi identici confini, bande, marginali, transizioni, tutte le 240
coppie q/rank statiche/proposte/raffinate, massimi, totali e accettazione.
Il criterio indipendente conferma `6221449 < 6506480`, dunque seleziona la
raffinata. Le 240 coppie `[X-CG]` coincidono una per una con i gruppi raffinati:
0 discrepanze, massimo 0, somma assoluta 0 e somma con segno 0. Questo prova
che la maschera selezionata è stata realmente usata.

### Tabella completa `[X-CG]` CPU weighted

`std` è la deviazione standard di popolazione; `inv` conta gli incrementi
rango-rango (quindi è zero per una sequenza decrescente).

|q|r0|r1|r2|r3|r4|r5|r6|r7|min|max|range|mean|std|CV|max/min|inv|trend|total|
|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|:--|--:|
|1|92075|98854|103090|105469|107343|109171|110937|112470|92075|112470|20395|104926.125|6350.090|6.052%|1.2215|7|inc|839409|
|2|245342|239315|237055|235389|234316|233045|230796|229414|229414|245342|15928|235584.000|4748.539|2.016%|1.0694|0|dec|1884672|
|3|238960|233785|231905|230580|229438|228689|226687|225545|225545|238960|13415|230698.625|3992.590|1.731%|1.0595|0|dec|1845589|
|4|171751|175328|177513|178758|179181|180386|180477|180819|171751|180819|9068|178026.625|2919.026|1.640%|1.0528|7|inc|1424213|
|5|237275|232503|230734|229166|228029|227763|225374|224433|224433|237275|12842|229409.625|3855.467|1.681%|1.0572|0|dec|1835277|
|6|267698|258026|254441|251944|250104|248972|245432|244022|244022|267698|23676|252579.875|7120.061|2.819%|1.0970|0|dec|2020639|
|7|273146|262156|257896|254786|252812|251425|247472|245878|245878|273146|27268|255696.375|8234.746|3.221%|1.1109|0|dec|2045571|
|8|298101|282335|276044|272157|269066|267015|261912|259544|259544|298101|38557|273271.750|11629.126|4.256%|1.1486|0|dec|2186174|
|9|180488|182673|184427|185806|185822|187038|187015|186896|180488|187038|6550|185020.625|2214.492|1.197%|1.0363|5|mixed|1480165|
|10|234715|230353|229070|227950|226391|226515|223728|223082|223082|234715|11633|227725.500|3507.193|1.540%|1.0521|1|mixed|1821804|
|11|280071|267932|263034|260068|257339|256297|251673|249869|249869|280071|30202|260785.375|9102.240|3.490%|1.1209|0|dec|2086283|
|12|178670|181340|183530|184338|184791|185697|185628|185877|178670|185877|7207|183733.875|2369.917|1.290%|1.0403|6|mixed|1469871|
|13|124912|130999|134873|137359|138734|140792|141789|143253|124912|143253|18341|136588.875|5753.837|4.213%|1.1468|7|inc|1092711|
|14|132156|138053|141722|143524|145192|146499|147796|148907|132156|148907|16751|142981.125|5231.064|3.659%|1.1268|7|inc|1143849|
|15|227877|224559|223855|222473|222228|221666|220184|219640|219640|227877|8237|222810.250|2463.009|1.105%|1.0375|0|dec|1782482|
|16|238800|233752|231979|230581|229734|228558|226541|225509|225509|238800|13291|230681.750|3973.933|1.723%|1.0589|0|dec|1845454|
|17|225197|222546|221936|220713|220383|219823|218381|217477|217477|225197|7720|220807.000|2283.148|1.034%|1.0355|0|dec|1766456|
|18|183277|185395|187122|188069|188756|189384|188891|189254|183277|189384|6107|187518.500|2026.914|1.081%|1.0333|6|mixed|1500148|
|19|237195|232567|230539|229057|228248|227994|225453|224269|224269|237195|12926|229415.250|3831.040|1.670%|1.0576|0|dec|1835322|
|20|297967|282067|275882|271881|269098|267066|261842|259650|259650|297967|38317|273181.625|11554.313|4.230%|1.1476|0|dec|2185453|
|21|219487|217568|216962|216789|216015|216246|214809|213957|213957|219487|5530|216479.125|1580.441|0.730%|1.0258|1|mixed|1731833|
|22|178124|180960|182906|184057|184408|185162|185150|185715|178124|185715|7591|183310.250|2422.381|1.321%|1.0426|6|mixed|1466482|
|23|235013|230139|229018|227846|226484|226276|224002|222901|222901|235013|12112|227709.875|3560.168|1.563%|1.0543|0|dec|1821679|
|24|168066|171588|174133|175523|175741|177016|177058|178010|168066|178010|9944|174641.875|3115.086|1.784%|1.0592|7|inc|1397135|
|25|223021|220478|219929|219541|218639|218875|216935|216523|216523|223021|6498|219242.625|1922.999|0.877%|1.0300|1|mixed|1753941|
|26|132070|137979|141610|143739|144989|146654|147853|148954|132070|148954|16884|142981.000|5285.123|3.696%|1.1278|7|inc|1143848|
|27|245529|239516|237041|235547|234092|233148|230629|229481|229481|245529|16048|235622.875|4827.455|2.049%|1.0699|0|dec|1884983|
|28|183437|185552|187243|188167|188667|189178|189080|189232|183437|189232|5795|187569.500|1950.922|1.040%|1.0316|6|mixed|1500556|
|29|179877|182258|184223|185493|185698|186992|186500|186503|179877|186992|7115|184693.000|2313.567|1.253%|1.0396|6|mixed|1477544|
|30|91152|97896|102240|104914|106364|108403|110417|111671|91152|111671|20519|104132.125|6433.509|6.178%|1.2251|7|inc|833057|

Aggregando i q si ottengono ancora `6221449,6158472,6151952,6141684,
6128102,6131745,6090441,6078755`: minimo 6,078,755, massimo 6,221,449,
range 142,694, media 6,137,825, std 41,078.81, CV 0.6693%, max/min
1.02347, una inversione e totale 49,102,600. I gruppi CPU sono identici a T05
GPU: l'architettura non cambia il prepass né il raggruppamento. Il legacy CPU
ha massimo 7,771,736, minimo 5,458,797, CV 11.525%, max/min 1.4237 e totale
49,421,143. La qualità del bilanciamento migliora quindi sostanzialmente anche
su CPU, ma non basta a ottimizzare il tempo.

### Timer CPU, confronto GPU e break-even

I timer riportati come `CPU` da Yambo sono contatori elapsed per-rank che
includono attese; `Xo (REDUX)` isola il principale tempo di attesa MPI. Il
wall globale è `[Time-Profile]`. `io_X`/`io_fragment` misurano la scrittura,
non vanno confusi col costo computazionale di `Xo (procedure)`.

|rank|legacy procedure|legacy REDUX|weighted diagnostic|weighted prepass|weighted procedure|weighted REDUX|weighted proc+REDUX|
|--:|--:|--:|--:|--:|--:|--:|--:|
|0|18910.3865|0.1344|28.9904|45.3318|14493.5819|4611.8148|19105.3967|
|1|18003.6621|909.9505|28.9904|45.3305|15977.6094|3130.6313|19108.2407|
|2|17450.0695|1463.5015|28.9904|45.3305|16597.2638|2510.9886|19108.2524|
|3|17296.8080|1616.8014|28.9904|45.3305|17298.9673|1809.2749|19108.2422|
|4|17174.7012|1738.9117|28.9904|45.3305|17716.6126|1391.6378|19108.2504|
|5|17045.2704|1868.3417|28.9904|45.3305|18247.1881|861.0163|19108.2044|
|6|16688.9852|2224.6174|28.9904|45.3305|18670.9062|437.2580|19108.1642|
|7|16659.3318|2254.2746|28.9904|45.3305|19107.9288|0.1455|19108.0743|

La baseline ha `procedure` min/max 16659.332/18910.387 s, range 2251.055 s
e CV 4.006%; il fit con i gruppi ha correlazione 0.9963, R² 0.9926 e RMS
60.1 s. Il weighted ha min/max 14493.582/19107.929 s, range 4614.347 s e CV
8.275%, perché il rank critico diventa r7. Il fit con le bande è
`time=6302.151+44.37897*bande`, correlazione 0.99868, R² 0.99737 e RMS
73.3 s; il fit con i gruppi assume segno spurio negativo. A gruppi quasi
uniformi, il costo secondario dominante su CPU è dunque il lavoro per banda
(transizioni/GEMM/accesso alle WF), molto più forte che su T05 GPU.

Il prepass costa 45.332 s, di cui 28.990 s di diagnostica interval-aware.
Il massimo della procedura aumenta però di 197.542 s (+1.045%) anziché
diminuire. Il cammino critico `procedure+prepass` è 19153.261 s contro
18910.387 s, +242.874 s (+1.284%); non esiste break-even positivo per questa
partizione CPU. Il wall totale conferma: 19179.920 contro 18940.818 s,
+239.102 s (+1.262%). `procedure+REDUX` è quasi uniforme in entrambi i run,
ma passa da circa 18913.6 a 19108.2 s. `io_X` massimo passa 3.174->2.969 s e
`io_fragment` 2.365->2.719 s: differenze irrilevanti rispetto alla regressione.

T05 GPU aveva massimo `procedure` 3879.243 s, CV 0.828% e REDUX
186.479--298.550 s: sulla GPU i gruppi bilanciati riducevano il cammino
critico, mentre su CPU il maggior numero di bande sui rank alti domina. I
tempi assoluti CPU/GPU non sono confrontati come hardware equivalente; il
confronto riguarda soltanto correlazioni, rank critico e costo dominante.

### Memoria host

La baseline alloca 2.282561 GiB di `WF%c` su ogni rank e arriva a 2.424478
GiB tracciati. Il weighted alloca `1.768774,2.013033,2.173065,2.290983,
2.366788,2.451015,2.560511,2.636315` GiB, con massimo tracciato 2.778334 GiB
su r7: +0.353856 GiB (+14.60%) rispetto alla baseline, ma sotto la soglia
SAVE stampata di 3.7589 GiB. Tutte le allocazioni sono liberate e il totale
finale torna a circa 33.5 MiB; non vi sono warning o crescita persistente. Il
maggior numero di bande è sostenibile nel caso DCGP misurato.

### Confronto integrale dei database NetCDF

I frammenti sono associati al q-point tramite la variabile interna `X_Q_q`,
non tramite nome o ordine. Entrambi gli insiemi coprono esattamente q=1..30.
Schema NetCDF4/HDF5, dimensioni, tipi, attributi e ordine delle variabili sono
identici per `ndb.pp` e per tutti i frammenti. Il database principale è
numericamente e testualmente identico in tutte le variabili. In ogni frammento
`FREQ_PARS_sec_iq` (1 elemento) e `FREQ_sec_iq` (4 elementi) sono identici;
la sola variabile fisica differente è `X_Q_q`, float32 con forma
`(2,753,753,2)`: ultimo asse reale/immaginario, 2,268,036 componenti. Non vi
sono NaN, fill mismatch o valori mascherati.

|q|elementi|differenti esatti|max abs|max rel|RMS|posizione max `(freq,g,g,re/im)`|
|--:|--:|--:|--:|--:|--:|:--|
|1|2268036|2259707|5.37038e-05|2.0000|1.50076e-07|(0,2,2,0)|
|2|2268036|2264053|1.73867e-04|2.0000|4.73101e-07|(0,1,1,0)|
|3|2268036|2263798|1.73092e-04|2.0000|4.77829e-07|(0,6,6,0)|
|4|2268036|2262572|1.24872e-04|1.9998|3.39083e-07|(0,3,3,0)|
|5|2268036|2263875|1.72913e-04|2.0000|4.81689e-07|(0,1,1,0)|
|6|2268036|2264189|2.11716e-04|2.0000|5.48740e-07|(0,6,6,0)|
|7|2268036|2264200|2.02775e-04|1.9999|5.41279e-07|(0,3,3,0)|
|8|2268036|2264574|2.23577e-04|1.9999|6.03646e-07|(0,7,7,0)|
|9|2268036|2262777|1.31845e-04|2.0000|3.64146e-07|(0,1,1,0)|
|10|2268036|2263712|1.95980e-04|1.9999|5.03678e-07|(0,5,5,0)|
|11|2268036|2264528|2.19762e-04|2.0000|6.15770e-07|(0,3,3,0)|
|12|2268036|2262835|1.37269e-04|1.9999|3.78155e-07|(0,3,3,0)|
|13|2268036|2261126|1.03533e-04|2.0000|2.77777e-07|(0,3,3,0)|
|14|2268036|2261424|8.49366e-05|2.0000|2.40437e-07|(0,3,3,0)|
|15|2268036|2263751|1.76191e-04|1.9999|4.50797e-07|(0,1,1,0)|
|16|2268036|2263791|1.70588e-04|1.9999|4.59728e-07|(0,1,1,0)|
|17|2268036|2263561|1.64628e-04|1.9999|4.34714e-07|(0,3,3,0)|
|18|2268036|2262815|1.32561e-04|2.0000|3.60515e-07|(0,3,3,0)|
|19|2268036|2263716|1.75178e-04|2.0000|4.62331e-07|(0,6,6,0)|
|20|2268036|2264512|2.28047e-04|1.9999|5.88769e-07|(0,6,6,0)|
|21|2268036|2263609|1.87814e-04|1.9999|4.70328e-07|(0,7,7,0)|
|22|2268036|2262852|1.51634e-04|1.9997|4.05284e-07|(0,0,0,0)|
|23|2268036|2263649|1.85907e-04|2.0000|5.06601e-07|(0,5,5,0)|
|24|2268036|2262417|1.39177e-04|2.0000|3.80925e-07|(0,12,12,0)|
|25|2268036|2263671|1.93417e-04|1.9999|5.16237e-07|(0,0,0,0)|
|26|2268036|2261556|8.90791e-05|2.0000|2.32465e-07|(0,5,5,0)|
|27|2268036|2263884|1.64866e-04|1.9999|4.64040e-07|(0,6,6,0)|
|28|2268036|2262559|1.10418e-04|1.9998|3.33537e-07|(0,5,5,0)|
|29|2268036|2262816|1.33753e-04|2.0000|3.55766e-07|(0,5,5,0)|
|30|2268036|2259093|5.65350e-05|2.0000|1.42580e-07|(0,10,10,0)|

Il massimo relativo vicino a 2 è dovuto a componenti quasi nulle con segno
opposto e non è una misura stabile; massimo assoluto e RMS sono le metriche
significative. Su 68,041,080 componenti nessuna supera una tolleranza assoluta
di `3e-4`, scelta conservativa ma solo 1.32 volte il massimo osservato e molto
inferiore alla scala delle componenti fisiche dominanti. Gli RMS sono
`1.43e-7`--`6.16e-7`, della scala attesa dal diverso ordine di somme float32.
Schema, frequenze e metadati fisici sono esatti; non vi sono differenze fuori
scala. I database sono quindi fisicamente equivalenti per questa precisione.

### Decisione dopo T06

T06 conferma correttezza algoritmica, riproducibilità del prepass,
applicazione reale della partizione, forte bilanciamento dei gruppi,
equivalenza fisica e sostenibilità della memoria CPU. Non conferma però il
beneficio netto: sulla CPU DCGP la partizione raffinata è 1.26% più lenta
perché ottimizza i gruppi trasferendo troppe bande ai rank alti. La validazione
principale non può quindi essere dichiarata conclusa nel senso prestazionale
multiarc; non serve una terza iterazione dei confini, che ottimizzerebbe ancora
la metrica sbagliata per CPU.

Il caso con gerarchia `q/g/k/c` non banale resta utile come verifica di
correttezza collettiva, ma non è necessario per spiegare T06 e non va usato
come tentativo di recuperare questa regressione. Prima dell'integrazione
servono una decisione esplicita sulla politica: mantenere l'euristica attuale
solo dove i gruppi dominano (o introdurre in futuro un criterio CPU che tenga
conto anche delle bande), regressione col flag spento sul benchmark completo,
caso con gerarchia non banale e almeno un sistema ulteriore.

I tre conteggi sono preziosi per la validazione ma costano 45.3 s e non
producono beneficio CPU. La raccomandazione è non rimuoverli ancora nel branch
di studio; prima dell'integrazione rendere opzionale la diagnostica dettagliata
e conservare soltanto i conteggi indispensabili alla selezione, dopo aver
deciso quale metrica/architettura abilita la partizione. Non sono stati
modificati algoritmo o diagnostica, non è stato preparato un test remoto e non
è stato creato alcun commit.

## Preparazione T07: dipendenza dalla topologia MPI/OpenMP

Dopo la discussione T06 è stato deciso di verificare in modo controllato
l'ipotesi che OpenMP modifichi l'impatto dello sbilanciamento. Il confronto
principale mantiene un nodo e otto rank di T06, passando da 1 a 14 thread per
rank. Il punto 4 MPI x 28 thread comprende sia legacy sia weighted: eseguire
soltanto il weighted misurerebbe il cambio di topologia ma non isolerebbe il
beneficio o la regressione della feature.

È stata predisposta, senza modifiche al codice e senza commit, la sessione
`/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata/`
`T07_anatase_cpu_openmp_policy_bb3167829`, con manifest, istruzioni di
raccolta, metadati e input effettivi per:

- legacy 1 nodo, 8 MPI x 14 OpenMP;
- weighted 1 nodo, 8 MPI x 14 OpenMP;
- legacy 1 nodo, 4 MPI x 28 OpenMP;
- weighted 1 nodo, 4 MPI x 28 OpenMP.

Le varianti weighted sono riferite al commit pubblicato
`bb316782914745149558a48802e572865550448f`; quelle legacy al branch `5.4`
con hash da dichiarare dall'operatore. Il manifest richiede gli stessi binari
nelle due topologie, database X inizialmente assente, dipoli T06 in sola
lettura e confronto NetCDF completo. Il verdetto primario è 8x14; 4x28 serve
come secondo punto sul rapporto MPI/OpenMP e non come scaling puro.

## Analisi T07: politica CPU/OpenMP

### Provenienza, integrità e confrontabilità

La provenienza attendibile è quella dichiarata dall'operatore: lo stesso
binario del branch `5.4` per i due legacy e lo stesso binario del commit
pubblicato `bb316782914745149558a48802e572865550448f` per i due weighted.
Le stringhe Git stampate da Yambo non sono state usate. I quattro input
copiati sono intenzionalmente byte-identici e contengono tutti la riga
`X_WeightedBands`: nel binario 5.4 la variabile non esiste, non è riconosciuta
e non può attivare alcun algoritmo; i log confermano infatti il solo percorso
storico. Questa scelta dello script di lancio non altera il confronto.

Ogni run contiene un input, un report, lo script scheduler effettivo,
`ndb.pp`, `ndb.RIM`, 30 frammenti e il numero atteso di log (8 oppure 4).
Checksum cluster, checksum dopo la copia e hash ricomputati coincidono per
43/43 file in ciascun run 8x14 e 39/39 in ciascun run 4x28. `ndb.pp` ha sempre
SHA-256 `e36d24ce...e95b` e `ndb.RIM` `3b790a6d...a384`; tutti i frammenti
sono presenti e hanno checksum individuali. Ogni log contiene una sola
`Game Over`; i report registrano la creazione di `ndb.pp` e dei frammenti.
Non emerge riuso di un database X precedente. I campi predisposti nei
`run_metadata.md` per hash legacy e assenza preventiva del database sono
rimasti vuoti, ma la provenienza è stata confermata direttamente
dall'operatore e la sequenza di scrittura è verificabile nei report.

Le coppie sono confrontabili: stesso nodo esclusivo, stessa fisica X-only,
stessi dipoli T06 in sola lettura, CPU-only, Intel oneAPI 2024.1, Intel MPI
2021.12.1 e stesse librerie; cambia soltanto il binario legacy/weighted. Gli
script e i report concordano su `8(CPU)-14(threads)` con `-n 8`,
`--cpus-per-task=14`, `OMP_NUM_THREADS=14`, oppure su
`4(CPU)-28(threads)` con i corrispondenti valori 4/28; il binding è
`--cpu-bind=cores`. L'assenza di `o-OUTPUT` è normale per questi run X-only.

### Reporting delle bande e prepass

Nel legacy compare soltanto `CON bands`: 247 bande per rank in 8x14 e 494
in 4x28. Nel weighted ogni rank mostra sia `CON bands (provisional)`
(247 oppure 494) sia `CON bands [X-WB final]`. I conteggi finali sono:

| topologia | intervalli finali | conteggi |
|:---|:---|:---|
| 8x14 | 25:210, 211:425, 426:659, 660:907, 908:1164, 1165:1431, 1432:1711, 1712:2000 | 186, 215, 234, 248, 257, 267, 280, 289 |
| 4x28 | 25:403, 404:874, 875:1406, 1407:2000 | 379, 471, 532, 594 |

In entrambi i casi la somma è 1976 e gli intervalli coprono 25:2000 una sola
volta, in ordine, senza vuoti o sovrapposizioni. I conteggi coincidono fra
`n_of_elements`, `[X-WB]` e reporting finale. Le selezioni sono entrambe
raffinate: 8x14 accetta `6221449 < 6506480`; 4x28 accetta
`10872002 < 11317613`.

Il confronto numerico integrale del prepass 8x14 con T06 8x1 e T05 GPU è:

| famiglia | righe | numeri confrontati | diversi | massimo | somma assoluta | somma con segno |
|:---|---:|---:|---:|---:|---:|---:|
| `[X-WB-M]` | 16 | 48 | 0 | 0 | 0 | 0 |
| `[X-WB-I]` | 286 | 1098 | 0 | 0 | 0 | 0 |
| `[X-WB-R]` | 287 | 1100 | 0 | 0 | 0 | 0 |
| `[X-WB]` | 9 | 32 | 0 | 0 | 0 | 0 |

Sono identici marginali, transizioni accettate, intervalli statici/proposti/
raffinati, massimi, aggregati e decisione. Non esiste quindi alcuna dipendenza
dal numero di thread o dall'architettura per quantità che devono essere
deterministiche: non c'è un primo punto di divergenza. Per 4x28 il controllo
indipendente trova quattro intervalli contigui validi, accettazione coerente e
copertura completa; numero e confini cambiano legittimamente perché i rank `c`
passano da otto a quattro.

Tutte le 240 coppie `[X-CG]` 8x14 coincidono coi gruppi raffinati selezionati;
lo stesso vale per tutte le 120 coppie 4x28. In entrambi i confronti:
discrepanze 0, massimo 0, somma assoluta 0, somma con segno 0. La partizione
selezionata è stata quindi realmente usata.

### Gruppi `[X-CG]`

La tabella completa 8x14 è numericamente identica alla tabella T06 riportata
nella sezione precedente (tutte le 240 celle, statistiche per q e aggregati):
questo è un confronto esatto, non un rinvio qualitativo. L'aggregato è
`6221449,6158472,6151952,6141684,6128102,6131745,6090441,6078755`, con
minimo 6,078,755, massimo 6,221,449, range 142,694, media 6,137,825,
deviazione standard 41,078.808, CV 0.6693%, max/min 1.02347, una inversione
e totale 49,102,600. Il legacy 8x14 riproduce anch'esso esattamente i gruppi
legacy T06: massimo 7,771,736, minimo 5,458,797, CV 11.525%, max/min 1.42371
e totale 49,421,143. Thread e architettura non cambiano i conteggi.

Tabella completa weighted 4x28 (`std` di popolazione; `inv` incrementi fra
rank adiacenti):

|q|r0|r1|r2|r3|min|max|range|mean|std|CV|max/min|inv|trend|total|
|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|:--|--:|
|1|174689|192405|203201|212009|174689|212009|37320|195576.000|13914.975|7.115%|1.21364|3|inc|782304|
|2|422190|405627|403245|397834|397834|422190|24356|407224.000|9090.326|2.232%|1.06122|0|dec|1628896|
|3|412714|398440|396703|392124|392124|412714|20590|399995.250|7697.092|1.924%|1.05251|0|dec|1599981|
|4|309390|315259|321194|324132|309390|324132|14742|317493.750|5666.195|1.785%|1.04765|3|inc|1269975|
|5|410591|396699|395270|390352|390352|410591|20239|398228.000|7516.029|1.887%|1.05185|0|dec|1592912|
|6|454549|430252|425579|417512|417512|454549|37037|431973.000|13807.981|3.196%|1.08871|0|dec|1727892|
|7|462827|435694|429970|420823|420823|462827|42004|437328.500|15647.876|3.578%|1.09981|0|dec|1749314|
|8|497632|460873|451850|439505|439505|497632|58127|462465.000|21674.248|4.687%|1.13226|0|dec|1849860|
|9|322963|326736|331822|333790|322963|333790|10827|328827.750|4253.239|1.293%|1.03352|3|inc|1315311|
|10|407014|394224|392750|388146|388146|407014|18868|395533.500|6997.137|1.769%|1.04861|0|dec|1582134|
|11|472690|443279|436195|426127|426127|472690|46563|444572.750|17339.881|3.900%|1.10927|0|dec|1778291|
|12|320175|324944|329870|331846|320175|331846|11671|326708.750|4532.909|1.387%|1.03645|3|inc|1306835|
|13|231880|247049|256488|263523|231880|263523|31643|249735.000|11850.396|4.745%|1.13646|3|inc|998940|
|14|244134|257698|267042|273698|244134|273698|29564|260643.000|11097.301|4.258%|1.12110|3|inc|1042572|
|15|396050|385995|385980|382871|382871|396050|13179|387724.000|4972.546|1.282%|1.03442|0|dec|1550896|
|16|412299|398510|396657|391876|391876|412299|20423|399835.500|7591.974|1.899%|1.05212|0|dec|1599342|
|17|392540|383145|383143|380047|380047|392540|12493|384718.750|4689.266|1.219%|1.03287|0|dec|1538875|
|18|327283|330872|335741|337176|327283|337176|9893|332768.000|3935.392|1.183%|1.03023|3|inc|1331072|
|19|410635|396387|395369|390424|390424|410635|20211|398203.750|7523.245|1.889%|1.05177|0|dec|1592815|
|20|497428|460901|451788|439486|439486|497428|57942|462400.750|21603.667|4.672%|1.13184|0|dec|1849603|
|21|383746|376099|376753|374541|374541|383746|9205|377784.750|3534.285|0.936%|1.02458|1|mixed|1511139|
|22|319579|324256|329144|331521|319579|331521|11942|326125.000|4598.219|1.410%|1.03737|3|inc|1304500|
|23|406831|394191|392577|388185|388185|406831|18646|395446.000|6930.846|1.753%|1.04803|0|dec|1581784|
|24|303652|310830|316190|319629|303652|319629|15977|312575.250|6031.005|1.929%|1.05262|3|inc|1250301|
|25|389006|380509|380783|378119|378119|389006|10887|382104.250|4117.247|1.078%|1.02879|1|mixed|1528417|
|26|244034|257944|266987|273691|244034|273691|29657|260664.000|11108.987|4.262%|1.12153|3|inc|1042656|
|27|422511|405910|403474|397616|397616|422511|24895|407377.750|9242.572|2.269%|1.06261|0|dec|1629511|
|28|327538|330761|335494|337192|327538|337192|9654|332746.250|3820.403|1.148%|1.02947|3|inc|1330985|
|29|322268|326396|331838|333390|322268|333390|11122|328473.000|4424.818|1.347%|1.03451|3|inc|1313892|
|30|173164|191232|201742|210788|173164|210788|37624|194231.500|13994.301|7.205%|1.21727|3|inc|776926|

Aggregato weighted 4x28: `10872002,10714031,10677973,10693725`, minimo
10,677,973, massimo 10,872,002, range 194,029, media 10,739,482.75,
std 78,638.951, CV 0.7322%, max/min 1.01817, una inversione, totale
42,957,931. Il legacy 4x28 ha `13363476,10865110,9781982,9124766`, CV
14.8312%, max/min 1.46453 e totale 43,135,334. Il weighted migliora quindi
fortemente la qualità del bilanciamento anche con quattro intervalli. Il
totale cambia rispetto a 8 rank perché il raggruppamento è non additivo; non
va interpretato come perdita di lavoro fisico. Rispetto a T05 GPU e T06 CPU
8-rank i conteggi 8x14 sono esatti; il confronto riguarda qualità e gruppi,
non tempi assoluti fra hardware.

### Timer, break-even ed effetto OpenMP

Timer per rank (secondi):

|topologia/variante|rank|procedure|REDUX|procedure+REDUX|diagnostico|prepass|
|:---|--:|--:|--:|--:|--:|--:|
|8x14 legacy|0--7|4044.642, 3604.663, 3436.999, 3339.857, 3258.508, 3204.549, 3129.704, 3111.384|0.150, 442.751, 610.423, 707.560, 788.902, 842.871, 917.679, 936.014|4044.792, 4047.413, 4047.422, 4047.417, 4047.410, 4047.420, 4047.383, 4047.398|--|--|
|8x14 weighted|0--7|3199.966, 3267.824, 3317.479, 3349.621, 3438.562, 3484.847, 3510.385, 3526.256|324.508, 261.484, 211.855, 179.699, 90.754, 44.479, 18.888, 3.050|3524.474, 3529.308, 3529.333, 3529.320, 3529.316, 3529.325, 3529.274, 3529.306|29.150|45.488|
|4x28 legacy|0--3|3494.582, 3110.179, 2769.849, 2709.839|0.150, 386.803, 727.314, 787.293|3494.732, 3496.982, 3497.163, 3497.132|--|--|
|4x28 weighted|0--3|2754.456, 3172.443, 3510.018, 3405.757|795.114, 380.226, 42.718, 146.666|3549.570, 3552.669, 3552.736, 3552.423|63.818|96.455|

In 8x14 il massimo di `Xo (procedure)` scende di 518.386 s (-12.817%).
Incluso il prepass, il cammino computazionale critico migliora di 472.897 s
(-11.692%); il budget di break-even era 518.386 s e il prepass ne usa solo
45.489 s. Il massimo `procedure+REDUX` scende 4047.422 -> 3529.333 s
(-12.801%). Il wall X-only passa 4074.0105 -> 3601.2612 s: -472.7493 s,
-11.604%. `io_X` massimo cresce 2.712 -> 4.951 s e `io_fragment` 2.187 ->
4.391 s, troppo poco per cambiare il verdetto; `io_WF` e `WF_load_FFT`
restano di pochi secondi.

In 4x28 il massimo di `procedure` cresce invece 3494.582 -> 3510.018 s
(+0.442%). Non esiste margine di break-even: col prepass il cammino sale di
111.891 s (+3.202%). Il massimo `procedure+REDUX` aumenta 3497.163 ->
3552.736 s (+1.589%); aggiungendo il prepass arriva a 3649.191 s. Il wall
passa 3542.9800 -> 3694.3767 s: +151.3967 s, +4.273%. Il diagnostico/prepass
quasi raddoppia rispetto a 8 rank (29.15/45.49 -> 63.82/96.46 s), coerente
con blocchi locali più grandi. L'I/O resta marginale.

Rispetto a T06 8x1, 8x14 non è strong scaling a risorse costanti: nodi e rank
sono uguali, ma cambiano thread e occupazione. Tuttavia il segnale è netto.
Nel legacy il massimo/range/CV di `procedure` passa da
18910.387/2251.055/4.006% a 4044.642/933.258/8.710%; nel weighted da
19107.929/4614.347/8.275% a 3526.256/326.290/3.333%. Il rank critico resta
r0 nel legacy e passa da r7 T06 a r7 anche in T07 weighted, ma la penalità per
banda è molto attenuata. La massima quota REDUX sul percorso sincronizzato
passa nel weighted da circa 24.1% T06 (r0) a 9.2% T07 (r0); nel legacy è
circa 23.1% T07 sul rank in attesa. OpenMP quindi non si limita ad accelerare
uniformemente il calcolo locale: a 8x14 riduce abbastanza il costo per banda
da far tornare dominante il beneficio del bilanciamento dei gruppi. Questo
risultato non si trasferisce a 4x28, dove meno rank amplificano blocchi,
prepass e memoria e il wall peggiora.

### Fit del costo

|run|predittori|correlazione|coefficienti (`intercetta`, gruppi, bande)|R2|RMS [s]|
|:---|:---|:---|:---|---:|---:|
|legacy 8x14|gruppi|0.99956|869.479; 4.08215e-4|0.99911|8.665|
|weighted 8x14|gruppi|-0.92573|19025.484; -2.54791e-3|0.85698|42.757|
|weighted 8x14|bande|0.97952|2535.986; 3.44486|0.95945|22.767|
|weighted 8x14|entrambi|--|-2871.576; 8.39333e-4; 4.48083|0.96568|20.946|
|legacy 4x28|gruppi|0.98094|950.248; 1.92034e-4|0.96225|60.835|
|weighted 4x28|gruppi|-0.81769|35636.132; -3.01928e-3|0.66861|167.156|
|weighted 4x28|bande|0.91367|1559.405; 3.34264|0.83479|118.023|
|weighted 4x28|entrambi|--|9630.481; -7.24231e-4; 2.74910|0.84694|113.601|

Nei legacy le bande sono costanti e il relativo fit non è identificabile.
Nei modelli a due variabili gruppi e bande sono anticorrelati e la matrice è
mal condizionata; coefficienti separati vanno quindi letti con cautela. I
residui weighted 8x14 sono, per il fit bande,
`23.24,-8.81,-24.61,-40.69,17.25,29.08,9.84,-5.30` s e, per il fit con
entrambe, `16.24,7.01,-23.00,-44.97,15.05,13.47,15.42,0.77` s. In T06 la
pendenza era 44.379 s/banda e R2 0.99737; in T07 8x14 scende a 3.445
s/banda, pur restando la banda il miglior predittore singolo. I soli gruppi
non tornano un predittore affidabile del rank critico weighted (coefficiente
spurio negativo). Con quattro soli punti i fit 4x28 sono descrittivi, non una
base robusta per inferenza o soglie automatiche.

### Memoria host e warning

In 8x14 il legacy alloca 2.282561 GiB di `WF%c` su ogni rank; il weighted
`1.768774,2.013033,2.173065,2.290983,2.366788,2.451015,2.560511,2.636315`
GiB. I picchi tracciati sono 2.424478 e 1.910793--2.778334 GiB. In 4x28 il
legacy alloca 4.362976 GiB/rank; il weighted
`3.394361,4.169253,4.683040,5.205249` GiB, con picchi tracciati fino a
5.347268 GiB su r3. L'ordine e la crescita sono coerenti con 379,471,532,594
bande e mostrano il costo locale maggiore dei blocchi a quattro rank.

Tutte le allocazioni `WF%c` sono liberate; il tracciato finale torna a circa
33.4--33.5 MiB. Non compaiono fallimenti, NaN, abort o crescita persistente.
I run 4x28, legacy e weighted allo stesso modo, producono 60 warning
`Empty workload` (15 per rank); gli 8x14 non ne producono. È quindi una
caratteristica preesistente della topologia ibrida, non della feature. Le
occorrenze testuali di HDF5 sono informazioni di configurazione, non errori.

### Equivalenza fisica NetCDF

I frammenti sono stati associati a q tramite i nomi/metadata interni
`X_Q_q`, `FREQ_PARS_sec_iq` e `FREQ_sec_iq`, non assumendo l'ordine dei file.
Ogni insieme copre q=1..30. In entrambe le coppie legacy/weighted `ndb.pp` e
`ndb.RIM` sono byte-identici. Per tutti i frammenti schema NetCDF4/HDF5,
dimensioni, tipi, attributi, fill value e ordine delle variabili coincidono;
`FREQ_PARS_sec_iq` e `FREQ_sec_iq` sono esatti. Non vi sono NaN o fill-value
mismatch. L'unica variabile differente è `X_Q_q`, float32, forma
`(2,753,753,2)`, 2,268,036 componenti reali.

|coppia|diff esatte per q|max assoluto globale|RMS per q|max relativo puntuale|tolleranza|
|:---|:---|---:|:---|---:|:---|
|8x14 legacy/weighted|2,260,769--2,264,504|2.02358e-4 (q8)|9.36e-8--4.89e-7|circa 2 sui quasi-zeri|3e-4 superata da 0 elementi|
|4x28 legacy/weighted|2,263,138--2,265,680|2.04742e-4 (q6)|2.67e-7--9.67e-7|circa 2 sui quasi-zeri|3e-4 superata da 0 elementi|

Il massimo relativo puntuale è dominato da componenti quasi nulle con segno
opposto, come in T06; massimo assoluto e RMS sono significativi. La tolleranza
assoluta `3e-4` resta motivata dal formato float32 e dall'ordine diverso delle
riduzioni ed è più restrittiva dei massimi legacy/weighted osservati.

Dettaglio q-per-q della sola variabile fisica diversa; `diff` è il numero di
componenti non bit-identiche e `pos` usa `(freq,g,g,re/im)`. Il massimo
relativo puntuale è circa 2 per ogni q a causa dei quasi-zeri; la colonna
relativa al valore di massimo assoluto non viene usata come sostituto di tale
massimo puntuale.

|q|diff 8x14|maxabs 8x14|RMS 8x14|pos 8x14|diff 4x28|maxabs 4x28|RMS 4x28|pos 4x28|
|--:|--:|--:|--:|:--|--:|--:|--:|:--|
|1|2261051|3.33786e-5|9.36017e-8|(0,2,2,0)|2263211|6.96778e-5|2.66923e-7|(0,7,7,0)|
|2|2264007|1.30415e-4|3.22946e-7|(0,1,1,0)|2265263|1.83254e-4|8.10976e-7|(0,16,16,0)|
|3|2263745|1.42336e-4|3.35126e-7|(0,10,10,0)|2265189|1.79708e-4|7.51818e-7|(0,32,32,0)|
|4|2263044|9.87053e-5|2.35873e-7|(0,2,2,0)|2264703|1.49518e-4|5.61149e-7|(0,29,29,0)|
|5|2263958|1.34110e-4|3.39830e-7|(0,10,10,0)|2265388|1.81437e-4|8.13813e-7|(0,17,17,0)|
|6|2264295|1.75536e-4|4.07312e-7|(0,1,1,0)|2265567|2.04742e-4|9.67350e-7|(0,13,13,0)|
|7|2264291|1.83821e-4|4.22491e-7|(0,3,3,0)|2265514|1.92642e-4|9.21489e-7|(0,4,4,0)|
|8|2264489|2.02358e-4|4.89271e-7|(0,3,3,0)|2265680|1.83940e-4|8.62730e-7|(0,29,29,0)|
|9|2263417|9.94205e-5|2.39911e-7|(0,1,1,0)|2265012|1.58250e-4|6.45902e-7|(0,15,15,0)|
|10|2264021|1.55568e-4|3.78501e-7|(0,1,1,0)|2265231|1.63436e-4|7.57400e-7|(0,17,17,0)|
|11|2264333|1.99556e-4|4.79033e-7|(0,3,3,0)|2265587|1.88172e-4|8.94134e-7|(0,4,4,0)|
|12|2263214|1.24574e-4|2.92170e-7|(0,7,7,0)|2264692|1.63108e-4|6.66085e-7|(0,15,15,0)|
|13|2262101|8.31485e-5|2.05214e-7|(0,5,5,0)|2263941|1.11401e-4|4.38933e-7|(0,2,2,0)|
|14|2262441|4.71175e-5|1.41877e-7|(0,3,3,0)|2264185|1.16706e-4|4.65475e-7|(0,18,18,0)|
|15|2263850|1.07706e-4|2.86442e-7|(0,10,10,0)|2265272|1.83284e-4|7.45588e-7|(0,27,27,0)|
|16|2263818|1.34110e-4|3.20926e-7|(0,10,10,0)|2265297|1.77354e-4|8.06657e-7|(0,17,17,0)|
|17|2263695|1.24156e-4|3.16221e-7|(0,3,3,0)|2265343|1.85877e-4|7.98335e-7|(0,25,25,0)|
|18|2263410|1.03474e-4|2.50939e-7|(0,1,1,0)|2264799|1.51694e-4|6.30745e-7|(0,25,25,0)|
|19|2263964|1.15424e-4|3.15238e-7|(0,9,9,0)|2265359|1.74522e-4|7.58195e-7|(0,18,18,0)|
|20|2264504|1.74940e-4|4.42277e-7|(0,5,5,0)|2265517|1.84029e-4|8.52264e-7|(0,16,16,0)|
|21|2263690|1.28657e-4|3.34449e-7|(0,1,1,0)|2265262|1.82241e-4|7.78689e-7|(0,15,15,0)|
|22|2263115|1.01149e-4|2.73913e-7|(0,6,6,0)|2264801|1.43886e-4|6.27446e-7|(0,9,9,0)|
|23|2263959|1.55807e-4|3.82259e-7|(0,3,3,0)|2265235|1.84357e-4|7.91587e-7|(0,19,19,0)|
|24|2262965|1.18613e-4|2.93901e-7|(0,5,5,0)|2264752|1.38700e-4|5.98060e-7|(0,17,17,0)|
|25|2263644|1.63376e-4|3.84605e-7|(0,2,2,0)|2265237|1.81377e-4|8.30069e-7|(0,25,25,0)|
|26|2262131|4.85778e-5|1.46247e-7|(0,5,5,0)|2264066|1.19120e-4|4.33754e-7|(0,18,18,0)|
|27|2264042|1.06156e-4|3.18801e-7|(0,10,10,0)|2265348|1.94311e-4|8.27492e-7|(0,27,27,0)|
|28|2262972|9.01520e-5|2.27182e-7|(0,10,10,0)|2264860|1.64300e-4|6.60434e-7|(0,28,28,0)|
|29|2263211|8.64267e-5|2.44680e-7|(0,6,6,0)|2264893|1.67847e-4|6.43970e-7|(0,10,10,0)|
|30|2260769|3.82662e-5|1.08035e-7|(0,2,2,0)|2263138|8.77082e-5|3.10735e-7|(0,21,21,0)|

Il confronto weighted 8x14/4x28 misura la sensibilità normale alla diversa
topologia di riduzione: massimo assoluto globale 4.15683e-4 (q8), RMS
3.78e-7--1.31e-6, nessun NaN/fill mismatch e schema/frequenze esatti. Una
tolleranza descrittiva di `5e-4` copre questo confronto fra topologie; non
sostituisce la soglia più stretta `3e-4` usata per il verdetto primario entro
ciascuna coppia. Tutti i database T07 sono fisicamente equivalenti.

### Decisione dopo T07

T07 smentisce una policy semplicemente “GPU contro CPU”: sulla CPU DCGP
weighted è chiaramente vantaggioso a 8 MPI x 14 OpenMP (-11.60% wall), ma
chiaramente svantaggioso a 8x1 (+1.26%, T06) e a 4x28 (+4.27%). Esiste dunque
una dipendenza reale dal rapporto MPI/OpenMP e dalla dimensione degli
intervalli. I soli gruppi non forniscono però una soglia automatica robusta:
la banda resta un costo secondario dominante e 4x28 mostra che aumentare i
thread non garantisce il beneficio.

La raccomandazione è mantenere `X_WeightedBands` esplicito e disabilitato per
default, documentando per ora il regime GPU validato e il regime CPU 8x14
favorevole, senza introdurre automatismi per architettura o numero di thread.
Non serve una terza iterazione dei confini: il problema residuo è la metrica
di costo, non la convergenza. Prima dell'integrazione restano: regressione del
flag spento sul benchmark completo, gerarchia `q/g/k/c` non banale per la
correttezza collettiva, almeno un altro sistema e una decisione su diagnostica
dettagliata. I conteggi completi sono utili in validazione ma costano 45.5 s
in 8x14 e 96.5 s in 4x28; dopo la decisione di policy conviene valutarne una
modalità opzionale e conservare soltanto quelli necessari alla selezione.

In questa attività non sono stati modificati algoritmo, policy o diagnostica,
non è stato predisposto un test remoto successivo e non è stato creato alcun
commit.

## Preparazione T08--T10 e test locali di robustezza

Dopo la discussione T07 sono state predisposte, senza modificare il codice e
senza commit, tre sessioni separate sotto la radice dei risultati:

- `T08_anatase_full_flagoff_bb3167829`: regressione completa anatase fra
  binario 5.4 e candidato `bb3167829`, entrambi senza `X_WeightedBands`, un
  nodo, 8 MPI e 14 thread per rank. L'input include X, HF e GW e serve a
  dimostrare che il flag spento conserva il comportamento storico completo.
- `T09_anatase_2n_q2c8_bb3167829`: coppia X-only legacy/weighted, due nodi,
  16 MPI, 14 thread per rank e gerarchia `q/g/k/c/v=2/1/1/8/1`. Serve a
  verificare collettive, partizione e database con livelli `q` e `c` entrambi
  non banali e comunicazione inter-node; non è uno scaling controllato.
- `T10_second_system_bb3167829`: il sistema scelto è GrCo-small, interfaccia
  grafene/cobalto, tramite il workflow JUBE full-GW disponibile. Non verrà
  estratto un X-only artificiale: Xo è dichiarata predominante e verrà
  analizzata con i suoi timer dedicati, mentre wall e output finali copriranno
  l'intero GW. Sono dichiarati 2000 bande totali, 40 di valenza, 1980 di
  conduzione, 7 k-point e 7 q-point. Poiché 40+1980=2020, input e log dovranno
  chiarire la convenzione effettiva prima di interpretare gli intervalli.
  Input e scheduler restano quelli realmente generati da JUBE e non vengono
  ricostruiti in anticipo.

Ogni sessione contiene manifest, istruzioni, metadati e directory separate
per input, scheduler, report, log e database. T08 e T09 includono input e
script scheduler pronti, con controllo non distruttivo che rifiuta una job
directory contenente già `OUTPUT/ndb.pp`; T10 attende gli artefatti effettivi
del workflow JUBE. Il prompt di ripresa dopo clear è
`/home/nicola/tmp/codex/prompts/prossimo_prompt_post_T10.md`.

I test locali sono conservati in
`/home/nicola/tmp/codex/yambo5-xo-weighted-robustness-local` e usano il
piccolo Al_bulk già validato, il binario CPU-only corrente e copie isolate dei
database. La sintesi autonoma con esiti, classificazione dei fallimenti e
stack del restart parziale è in
`/home/nicola/tmp/codex/yambo5-xo-weighted-robustness-local/RESULTS.md`:

- un solo rank `c`: terminazione regolare, otto q-point, una `Game Over`,
  nessuna riga `[X-WB]`, come previsto perché il nuovo percorso richiede più
  di un rank `c`; `Xo (procedure)` 0.1659 s;
- quattro rank `c` e una sola banda di conduzione effettiva: il run viene
  rifiutato prima del prepass dal controllo storico
  `USER parallel structure does not fit ... 4 CPU for 1 elements (ROLE is
  'c')`. Il supporto interno del partizionatore per rank eccedenti non è
  raggiungibile attraverso questa configurazione produttiva; non è un errore
  introdotto da weighted;
- database `ndb.em1d` completo: il percorso lifetimes locale termina, ma
  ricalcola prepass e tutti gli otto frammenti anche con I/O X abilitato. Non
  costituisce quindi un fixture adatto a verificare lo skip/restart produttivo
  di `ndb.pp`;
- copia isolata di `ndb.em1d` priva del solo frammento 8: segmentation fault
  in `X_dielectric_matrix.F:280`. Lo stesso crash e lo stesso stack si
  riproducono col flag assente, prima di qualsiasi riga `[X-WB]`; è un
  comportamento storico del restart parziale `ndb.em1d`, non una regressione
  della feature. Gli originali non sono stati modificati.

Pesi nulli e più rank che elementi restano coperti dal test sintetico
esaustivo del partizionatore. Un vero restart parziale di `ndb.pp` potrà essere
aggiunto come test separato se ritenuto requisito d'integrazione, ma il fixture
lifetimes locale non può validarlo. Nessuna correzione al comportamento
storico di restart è stata applicata in questa fase.

## Analisi conclusiva T08--T10 e robustezza locale

### Stato Git, provenienza e integrità

L'analisi è stata svolta sul branch
`tech-xo-weighted-band-distribution`, con `HEAD` e remoto al commit candidato
`bb316782914745149558a48802e572865550448f`. Il worktree conteneva già la
modifica non committata a questo diario prodotta dall'analisi T06--T07; è stata
preservata. Non sono stati modificati algoritmo, policy o diagnostica e non è
stato creato alcun commit.

La provenienza usata è quella dichiarata dall'operatore e nei metadati di
raccolta: branch `5.4` per i legacy e commit `bb3167829` per i candidati. Le
stringhe Git stampate da Yambo non sono state usate. I campi operativi dei
`run_metadata.md` sono rimasti in gran parte vuoti; la provenienza dichiarata
dall'operatore resta pertanto l'evidenza autorevole.

| sessione/run | file cluster | file locali verificati | mismatch SHA-256 |
|:---|---:|---:|---:|
| T08 legacy | 47 | 47 | 0 |
| T08 candidato flag-off | 47 | 47 | 0 |
| T09 legacy | 51 | 51 | 0 |
| T09 weighted | 51 | 51 | 0 |
| T10 legacy | 25 | 25 | 0 |
| T10 weighted | 25 | 25 | 0 |

I 35 database T08 legacy sono stati aggiunti dopo la prima ispezione. Il file
`sha256sums_local.txt` legacy non è stato rigenerato e contiene ancora solo 12
righe, ma gli SHA-256 dei 35 file sono stati ricalcolati direttamente e
coincidono tutti con `sha256sums_cluster.txt`. Ogni log T08, T09 e T10 contiene
una sola `Game Over`; non compaiono abort, deadlock, NaN o errori MPI/HDF5.

### T08: regressione completa con flag spento

Gli input legacy e candidato sono byte-identici e non contengono
`X_WeightedBands`. Entrambi usano 8 MPI x 14 thread,
`q/g/k/c/v=1/1/1/8/1`, 30 q-point e bande X 1:2000. Non compare alcun
marcatore `[X-WB]`, timer weighted o reporting provvisorio/finale: il candidato
percorre il ramo storico, con 247 delle 1976 bande di conduzione per rank.

| misura | legacy 5.4 | candidato flag-off | variazione |
|:---|---:|---:|---:|
| wall full-GW | 35855.6719 s | 35736.2109 s | -0.33% |
| `Xo (procedure)` massimo | 4002.5679 s | 4029.9293 s | +0.68% |
| `Xo (REDUX)` massimo | 913.2236 s | 930.9542 s | +17.7306 s |
| `WF%c` per rank | 2.282561 GiB | 2.282561 GiB | 0 |
| picco tracciato X | 2.424498 GiB | 2.424498 GiB | 0 |

I 240 conteggi `[X-CG]`, allocazioni e liberazioni seguono la stessa
struttura. Ogni run produce soltanto gli otto warning preesistenti `[x,Vnl]`.
Gli output QP coprono gli stessi 420 stati, con `Eo` identico; differenze
massime 5.7e-5 eV su `E-Eo` e 4e-6 eV su `Sc|Eo`, RMS 1.12e-5 e 1.24e-6 eV.

Il confronto NetCDF integrale dà schema, dimensioni, tipi e attributi
coincidenti. `ndb.RIM` è esatto. In `ndb.pp`, `ndb.HF_and_locXC` e
`ndb.dipoles` le sole differenze segnalate sono NaN nella stessa posizione di
campi `PARS`/`X_PARS_3`, per cui `NaN != NaN` non rappresenta una differenza
fisica. Nei 30 frammenti frequenze e metadati sono esatti; la sola variabile
fisica diversa è `X_Q_q` float32, con massimo assoluto globale 3.88222e-6
(q=1), RMS 2.00e-9--1.16e-8 e zero componenti oltre 3e-4.
`ndb.QP` conserva schema e stati: `QP_E` differisce al massimo di 2.08616e-6
nelle unità interne e `QP_Z` di 5.19156e-5. HF, X e GW sono fisicamente
equivalenti. **T08 è superato:** il flag spento conserva il comportamento
storico salvo normale variabilità temporale, ordine delle riduzioni e
metadati non fisici.

### T09: gerarchia q/c multi-nodo

La coppia usa due nodi, 16 MPI, 14 thread e gerarchia `2/1/1/8/1`; gli input
differiscono soltanto per `X_WeightedBands`. Ogni rank tratta 15 q-point: i
rank 1--8 coprono q=1--15 e i rank 9--16 q=16--30. Ogni rank emette 15 righe
`[X-CG]` e termina, senza warning o collettive divergenti.

| gruppo q | intervalli finali c-rank 0--7 | conteggi |
|:---|:---|:---|
| q=1--15 | 25:209, 210:423, 424:657, 658:905, 906:1162, 1163:1429, 1430:1710, 1711:2000 | 185,214,234,248,257,267,281,290 |
| q=16--30 | 25:211, 212:427, 428:662, 663:910, 911:1167, 1168:1433, 1434:1712, 1713:2000 | 187,216,235,248,257,266,279,288 |

Entrambe le partizioni sono raffinate, contigue, ordinate, senza vuoti o
sovrapposizioni e coprono 25:2000 una volta. Reporting finale,
`n_of_elements` e `[X-WB]` coincidono. Il precedente `247/1976` è marcato
`provisional`. Le differenze di una o due bande rispetto a T05--T07 sono
attese perché ciascun gruppo costruisce i pesi su metà dei q-point; sui q
comuni i conteggi restano deterministici. Le 240 coppie `[X-CG]` coincidono
integralmente con i costi della candidata raffinata selezionata.

| misura | legacy | weighted |
|:---|---:|---:|
| wall X-only | 2057.2905 s | 1806.9293 s |
| `Xo (procedure)` massimo | 2039.9166 s | 1765.1145 s |
| prepass massimo | -- | 22.9863 s |
| diagnostica massima | -- | 14.6937 s |
| `WF%c` | 2.282561 GiB | 1.760351--2.644738 GiB |
| picco tracciato | 2.424478 GiB | massimo 2.786712 GiB |

Il wall cala del 12.17%, senza trattare T09 come scaling controllato. Le
allocazioni sono liberate. `ndb.RIM` e le parti invarianti di `ndb.pp` sono
esatte; nei frammenti la sola differenza fisica è `X_Q_q`, massimo globale
2.03967e-4, RMS 9.32e-8--4.95e-7 e zero elementi oltre 3e-4. **T09 è
superato** per collettive, reporting, copertura, selezione, applicazione,
determinismo ed equivalenza fisica.

### T10: GrCo-small full-GW

Entrambi i run full-GW usano otto MPI, un thread e `1/1/1/8/1`; gli input
differiscono soltanto per il flag. L'input richiede bande X/GW 1:2000 e sette
q-point. Il report risolve senza assunzioni la convenzione apparentemente
incoerente dei conteggi dichiarati: 2000 bande e 77 elettroni; spin UP con 39
piene, 40:44 metalliche e 45:2000 vuote; spin DN con 33 piene, 34:40
metalliche e 41:2000 vuote. La maschera c comune è quindi 34:2000, 1967
bande, mentre la maschera v ne comprende 44. I due insiemi si sovrappongono
sulle bande metalliche e non vanno sommati. I riassunti preliminari 40+1980 e
44+1956 non descrivono la maschera produttiva.

| c-rank | intervallo finale | bande |
|---:|:---:|---:|
| 0 | 34:237 | 204 |
| 1 | 238:456 | 219 |
| 2 | 457:691 | 235 |
| 3 | 692:935 | 244 |
| 4 | 936:1190 | 255 |
| 5 | 1191:1450 | 260 |
| 6 | 1451:1720 | 270 |
| 7 | 1721:2000 | 280 |

La copertura è completa e contigua; reporting finale e `[X-WB]` coincidono.
Il raffinamento è accettato perché il massimo aggregato esatto scende
1,087,827 -> 1,055,450. Le 56 coppie `[X-CG]` coincidono con la candidata
raffinata selezionata.

| misura | legacy | weighted | variazione |
|:---|---:|---:|---:|
| `Xo (procedure)` massimo | 4726.9092 s | 4442.0093 s | -6.03% |
| prepass massimo | -- | 5.4506 s | -- |
| `Xo (REDUX)` massimo | 819.8397 s | 418.0057 s | -401.8340 s |
| wall full-GW | 7080.9570 s | 6829.6079 s | -3.55% |
| `WF%c` | 1.238079--1.286893 GiB | 1.051702--1.437770 GiB | redistribuito |
| picco massimo | 1.486189 GiB | 1.637099 GiB | +0.150910 GiB |

Le allocazioni sono liberate. Le categorie e molteplicità dei warning sono
identiche nelle due varianti: metallicità, livelli spin fusi, Drude assente,
`[x,Vnl]` e due `Empty workload` su CPU 4.

I database hanno schema e metadati coincidenti. Nei sette frammenti la sola
differenza fisica è `X_Q_q`, RMS 9.90e-8--2.99e-7. Il massimo è 3.33905e-4 a
q=2: soltanto tre delle 15,251,220 componenti superano 3e-4; gli altri q
restano sotto 1.18e-4. Il superamento minimo della soglia descrittiva anatase,
senza NaN/fill mismatch, è compatibile con riduzioni float32 della matrice più
grande e resta esplicitamente registrato. `ndb.QP` conserva stati e schema:
massimi 2.3425e-5 su `QP_E` e 9.1970e-5 su `QP_Z` nelle unità interne.
`o-OUTPUT.qp` conserva 16 stati/spin, `Eo` e spin; massimi 6.38e-4 eV su
`E-Eo` e 6.71e-4 eV su `Sc|Eo`, RMS 2.17e-4 e 2.45e-4 eV. **T10 è superato**
come secondo sistema reale, con equivalenza fisica e beneficio sia Xo sia
full-GW.

### Robustezza locale

| caso | esito | classificazione |
|:---|:---|:---|
| un rank `c` | termina, nessun `[X-WB]` | skip previsto |
| più rank `c` che bande | abort `4 CPU for 1 elements` prima del prepass | controllo storico |
| restart completo `ndb.em1d` | termina ma ricalcola prepass e frammenti | fixture non valida lo skip `ndb.pp` |
| restart parziale weighted | segfault prima di `[X-WB]` | comportamento storico `ndb.em1d` |
| restart parziale flag-off | stesso segfault e stack | indipendente dalla feature |

Rank eccedenti e pesi nulli sono già coperti dal test sintetico esaustivo del
partizionatore. Un restart parziale produttivo `ndb.pp` resta non coperto; il
fixture lifetimes `ndb.em1d` non può validarlo.

### Decisione finale per 5.4

Il candidato `bb3167829` soddisfa i criteri di correttezza: flag spento
equivalente al percorso storico, partizioni contigue valide, applicazione
esatta della candidata selezionata, determinismo, collettive q/c corrette,
equivalenza fisica su anatase e GrCo-small, memoria liberata e nessun nuovo
warning. La prestazione conferma che il flag deve restare esplicito e
disabilitato per default: è favorevole su GPU T05, CPU 8x14 T07, T09 e T10,
ma sfavorevole su CPU 8x1 T06 e 4x28 T07. Non esiste evidenza per una policy
automatica.

La decisione finale è **favorevole all'integrazione in 5.4 come opzione
sperimentale opt-in, default `.false.`**, mantenendo documentati i regimi
validati e quelli sfavorevoli. Il restart parziale `ndb.pp` non verificato è
un limite di copertura da registrare, non una regressione osservata né un
blocco, perché il solo crash riprodotto appartiene anche al percorso storico
`ndb.em1d`. Non sono richieste modifiche ad algoritmo, policy o diagnostica
prima della discussione con l'operatore; in particolare non va introdotta
alcuna attivazione automatica sulla base dei timer raccolti.

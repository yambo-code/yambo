# Progressi: distribuzione MPI contigua pesata delle bande di Xo

Ultimo aggiornamento: 2026-10-09.

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

Compilare e verificare localmente la correzione marginale. Non predisporre T02
finché la correzione e l'analisi T01 non sono consolidate in un commit
pubblicato e non è stata concordata una nuova richiesta remota controllata.

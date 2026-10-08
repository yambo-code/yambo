# Progressi: distribuzione MPI contigua pesata delle bande di Xo

Ultimo aggiornamento: 2026-10-08.

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
- T01 predisposto; richiesta pronta per due run q=1 sullo stesso binario
  (`X_WeightedBands` assente/presente).

## Directory dei risultati

- Radice prevista:
  `/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata`.
- Sessione predisposta:
  `T01_q1_smoke_cf76e8fdf`, completa di manifest, istruzioni, input e directory
  vuote per report/output/log di entrambe le varianti.

## Analisi e problemi

- Il percorso attuale costruisce la maschera `c` in
  `PARALLEL_global_indexes` e distribuisce le funzioni d'onda più tardi in
  `X_dielectric_matrix`, lasciando il punto necessario per sostituire la sola
  maschera prima della distribuzione.
- La logica adattiva del coarse grid è attualmente accoppiata ad allocazioni,
  strutture globali e messaggi; va estratto un nucleo condiviso.

## Prossima attività

Pubblicare la correzione della partizione statica e le verifiche locali,
predisporre T01 sul relativo hash già remoto e attendere i risultati prima di
preparare T02.

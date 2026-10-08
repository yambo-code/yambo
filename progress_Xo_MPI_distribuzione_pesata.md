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
- Restano da eseguire il confronto runtime X-CG/prepass e piccoli casi MPI con
  database; questi saranno completati prima della richiesta T01.

## Commit pubblicati e richieste di test

- Commit documentale pubblicato: `48bef44a4`.
- Nessuna richiesta di test ancora emessa.

## Directory dei risultati

- Radice prevista:
  `/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata`.
- Nessuna sessione TNN ancora predisposta.

## Analisi e problemi

- Il percorso attuale costruisce la maschera `c` in
  `PARALLEL_global_indexes` e distribuisce le funzioni d'onda più tardi in
  `X_dielectric_matrix`, lasciando il punto necessario per sostituire la sola
  maschera prima della distribuzione.
- La logica adattiva del coarse grid è attualmente accoppiata ad allocazioni,
  strutture globali e messaggi; va estratto un nucleo condiviso.

## Prossima attività

Consolidare e pubblicare il commit d'implementazione, eseguire un piccolo caso
MPI locale con almeno due rank `c` nelle modalità legacy e pesata, confrontare
transizioni e `[X-CG]`, quindi pubblicare le verifiche e predisporre T01 sul
relativo hash.

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

## Build e verifiche locali

- Non ancora eseguite.

## Commit pubblicati e richieste di test

- Nessun commit ancora pubblicato.
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

Leggere integralmente le routine interessate e le interfacce MPI/input,
definire le modifiche minime ai moduli e implementare prima il motore X-CG e la
partizione minimax verificabili isolatamente.

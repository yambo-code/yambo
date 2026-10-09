# Piano: distribuzione MPI contigua pesata delle bande di Xo

Base: branch `5.4`, commit `a4a1dfaa9800917b36bd5aee613d27ff99c60c73`.
Branch di lavoro: `tech-xo-weighted-band-distribution`.

Il diario operativo e lo stato aggiornato delle verifiche sono in
[`progress_Xo_MPI_distribuzione_pesata.md`](progress_Xo_MPI_distribuzione_pesata.md).

## Obiettivo

Introdurre l'opzione logica `X_WeightedBands`, disabilitata per default, che
per `X_dielectric_matrix` sostituisce esclusivamente la distribuzione
equinumerosa delle bande `c` con una partizione contigua minimax guidata da un
prepass dei gruppi X-CG. Il percorso storico e `OPTICS_driver` restano
invariati quando l'opzione è disabilitata.

## Fasi

1. Conservare analisi, piano e diario in un commit documentale autonomo.
2. Estrarre da `FREQUENCIES_coarse_grid` un motore di raggruppamento privo di
   effetti collaterali e verificarne l'equivalenza col wrapper corrente.
3. Implementare e testare la partizione contigua minimax deterministica,
   inclusi pesi nulli, rank eccedenti e metadati completi degli indici.
4. Aggiungere `X_WeightedBands` all'input e alle opzioni parallele, con default
   `.false.`.
5. Inserire in `X_dielectric_matrix`, dopo i controlli dei database e prima di
   `PARALLEL_WF_distribute`, il prepass su tutti i q-point pendenti del gruppo
   `q`; riprodurre i filtri di `X_eh_setup`, distribuire ciclicamente le bande
   sul comunicatore `c`, ridurre pesi interi a 64 bit e applicare la maschera
   finale soltanto a `PAR_IND_CON_BANDS_X(X%whoami)`.
6. Aggiungere timer e riepilogo per rank; mantenere i dettagli per banda al
   livello debug.
7. Compilare la configurazione MPI ed eseguire verifiche sintetiche e piccoli
   casi MPI, confrontando percorso legacy, motore X-CG e transizioni accettate.
8. Aggiornare la documentazione finale, creare commit logici e pubblicarli su
   `origin/tech-xo-weighted-band-distribution`.
9. Predisporre T01 sotto
   `/home/nicola/tmp/risultati-test-codex-Xo-distribuzione-pesata`, riferito a
   un commit già pubblicato. Analizzare T01 prima di predisporre T02.

## Criteri di verifica

- Ogni banda è assegnata esattamente una volta; gli intervalli non vuoti sono
  contigui, ordinati e non sovrapposti.
- Il massimo carico della partizione coincide con l'ottimo nei casi sintetici.
- `X_WeightedBands=.false.` percorre il codice storico senza modifiche.
- Il prepass accetta le stesse transizioni di `X_eh_setup` e il motore condiviso
  riproduce i gruppi `[X-CG]`.
- La correttezza fisica remota è valutata esclusivamente su `o-OUTPUT.qp`, con
  le tolleranze correnti del tester.
- Il benchmark anatase riduce il tempo Xo totale includendo il prepass.

## Vincoli Git e test remoti

Non usare rebase, force-push o riscrittura della cronologia. Ogni richiesta di
test remoto deve indicare un hash già pubblicato. Ogni sessione remota usa una
nuova directory `TNN_<scopo>_<git-short>` con manifest, istruzioni di copia e
sottodirectory `legacy/` e `weighted/` complete.

Per T01 anatase completo, la provenienza dichiarata dall'utente è:

- legacy: branch `5.4`, commit
  `a4a1dfaa9800917b36bd5aee613d27ff99c60c73`;
- weighted: branch `tech-xo-weighted-band-distribution`, implementazione T01
  al commit `cf76e8fdf366f678e4c39f33ece00d5f253c0676`.

L'indicazione di branch/revisione stampata nei report Yambo non è attendibile
per stabilire la provenienza del binario e non deve essere usata nelle analisi.
La provenienza va registrata dal comando di build/esecuzione o confermata da
chi ha effettuato il run.

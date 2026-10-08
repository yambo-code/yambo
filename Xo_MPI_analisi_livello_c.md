# Analisi MPI di Xo: livello delle bande di conduzione (`c`)

Questo documento raccoglie l'analisi della distribuzione MPI del carico di lavoro nel driver della polarizzabilità irriducibile (`Xo`), con particolare attenzione al livello di parallelizzazione sulle bande di conduzione. Non contiene modifiche né implementazioni di nuovi algoritmi di bilanciamento.

## Conclusioni principali

- La parallelizzazione `c` distribuisce blocchi contigui di indici di banda di conduzione, bilanciandone soltanto il numero. Il rank 0 riceve il blocco con gli indici più bassi e, quando esiste un resto, è fra i primi rank a ricevere una banda aggiuntiva.
- Nel run fornito ogni rank possiede esattamente 247 bande di conduzione ed esattamente 1.280.448 transizioni grezze per q-point.
- Il raggruppamento per energia di transizione viene eseguito localmente, dopo che le maschere MPI hanno già selezionato le bande appartenenti a ciascun rank. Non esiste un raggruppamento globale seguito da redistribuzione.
- Il primo numero nel messaggio `[X-CG] R(p) Tot o/o(of R)` è `coarse_grid_N`: il numero locale di gruppi di energie di transizione e, in questo run, il numero di iterazioni principali della coarse grid di Xo. Il secondo numero è il numero locale di transizioni grezze. Il valore finale `100` non è il rapporto fra i primi due numeri.
- P1 ha molti più gruppi di coarse grid di P8: sommando i 30 q-point, 7.771.736 contro 5.458.797, cioè un rapporto pari a 1,424.
- Il corrispondente rapporto dei tempi `Xo (procedure)` è 5736,26/3197,29 = 1,794. Considerando tutti gli otto rank, la correlazione di Pearson fra il numero di gruppi e il tempo Xo è 0,9976.
- P1 è lento perché il suo blocco di bande di conduzione più basse produce meno degenerazione energetica, quindi più gruppi distinti. Non è lento perché possiede più bande o più transizioni grezze, né principalmente perché è il rank MPI 0.
- L'all-reduce finale di Xo rende direttamente visibile lo sbilanciamento: P1 trascorre soltanto 0,64 s in `Xo (REDUX)`, mentre P8 vi trascorre 2549,43 s. La somma fra calcolo e riduzione è circa 5747 s per ogni rank.

## Provenienza del sorgente e del run

La revisione autorevole per il run fornito è `a4a1dfaa9`, sul branch `5.4`. Il checkout usato per questa analisi coincide esattamente con quel commit e i file sorgente rilevanti non presentano differenze locali. Sia i riferimenti al sorgente sia l'interpretazione dei tempi osservati descrivono quindi l'implementazione che ha prodotto il run.

Dati del run:

```text
/home/nicola/tmp/anatase_on_leonardo-booster_2node_8mpi_8gpus
```

Il run ha usato 8 rank MPI, 8 GPU NVIDIA A100 e una mappatura di un rank MPI per GPU. La struttura parallela di X era:

```text
X_and_IO_CPU   = "1 1 1 8 1"
X_and_IO_ROLEs = "q g k c v"
```

Quindi q, G, k e v non erano distribuiti, mentre tutti gli otto rank partecipavano alla parallelizzazione `c`.

## Flusso di esecuzione

```text
PARALLEL_global_indexes
  `- maschera di proprietà di blocchi contigui di bande c
       |
       v
X_irredux
  |- X_eh_setup(-iq): costruzione delle energie delle transizioni grezze locali
  |- FREQUENCIES_coarse_grid: ordinamento e raggruppamento delle energie locali
  |- X_eh_setup(+iq): popolamento dei descrittori ordinati delle transizioni
  |
  `- per ogni gruppo locale della coarse grid
       |- X_irredux_residuals
       |    |- generazione e memorizzazione dell'oscillatore di ogni transizione grezza
       |    `- costruzione del residuo del gruppo con una GEMM
       |- valutazione di una funzione di Green per il gruppo
       `- accumulo del residuo del gruppo in Xo
            |
            v
       barrier MPI + MPI_Allreduce di Xo
```

## 1. Distribuzione delle bande di conduzione

La distribuzione della funzione di risposta viene inizializzata in `src/pol_function/X_dielectric_matrix.F`, intorno alle linee 229–255, prima del loop sui q-point e prima della chiamata a `X_irredux`.

`PARALLEL_global_indexes(...,"Response_G_space_and_IO",X=X)` costruisce la gerarchia `q,g,k,c,v`. La gerarchia dei comunicatori viene creata da:

- `src/parallel/PARALLEL_global_Response_G.F`, routine `PARALLEL_global_Response_G`;
- `src/parallel/PARALLEL_structure.F`, routine `PARALLEL_structure`;
- `src/parallel/PARALLEL_assign_chains_and_COMMs.F`;
- `src/parallel/PARALLEL_build_up_child_INTER_chains.F`.

Con la gerarchia `1 1 1 8 1`, il comunicatore `c` contiene tutti i rank globali nello stesso ordine dei rank globali. Il suo `CPU_id` coincide quindi con il rank MPI a base zero.

I limiti delle bande vengono determinati in `src/parallel/PARALLEL_global_dimensions.F`, intorno alle linee 61–66:

```fortran
PAR_n_c_bands = (/minval(E%nbf)+1,PAR_X_ib(2)/)
```

Per questo run isolante, il report indica 24 bande di valenza e un intervallo X che termina alla banda 2000. L'intervallo delle bande di conduzione è quindi 25–2000, per un totale di 1976 bande.

La maschera di proprietà viene costruita in `src/parallel/PARALLEL_global_indexes.F`, intorno alle linee 163–188:

```fortran
call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/PAR_n_c_bands(2)/), &
                    low_range=(/PAR_n_c_bands(1)/),                  &
                    COMM=PAR_COM_CON_INDEX_X(X_type),                &
                    CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
```

L'oggetto rilevante è `PAR_IND_CON_BANDS_X(X%whoami)`, una struttura `PP_indexes` che contiene:

- `element_1D(ic)`: vero quando il rank corrente possiede la banda globale `ic`;
- `first_of_1D(rank+1)`;
- `last_of_1D(rank+1)`;
- `n_of_elements(rank+1)`.

`X_eh_setup` verifica `element_1D(ic)` prima di accettare una banda di conduzione.

### Algoritmo di distribuzione

Il ramo `CONSECUTIVE=.TRUE.` si trova in `src/parallel/PARALLEL_index.F`, intorno alle linee 73–105. Per `N` elementi e `P` rank:

```text
base  = floor(N/P)
resto = N - base*P
```

I primi `resto` rank del comunicatore ricevono `base+1` elementi; gli altri ricevono `base` elementi. I blocchi rimangono contigui e ordinati dall'indice globale più basso a quello più alto.

Di conseguenza:

- il rank 0 riceve sempre le bande di conduzione più basse;
- i rank con indice più basso ricevono le eventuali bande di resto;
- la distribuzione è a blocchi, non ciclica né block-cyclic.

In questo caso `1976/8 = 247` esattamente, quindi non c'è resto:

| Processo | Rank MPI | Bande di conduzione | Numero |
|---|---:|---:|---:|
| P1 | 0 | 25–271 | 247 |
| P2 | 1 | 272–518 | 247 |
| P3 | 2 | 519–765 | 247 |
| P4 | 3 | 766–1012 | 247 |
| P5 | 4 | 1013–1259 | 247 |
| P6 | 5 | 1260–1506 | 247 |
| P7 | 6 | 1507–1753 | 247 |
| P8 | 7 | 1754–2000 | 247 |

La convenzione per il nome del processo è esplicitamente `P = myid + 1` in `src/modules/mod_LIVE_t.F`, intorno alle linee 233–239. P1 corrisponde quindi al rank MPI 0.

## 2. Costruzione delle transizioni grezze

`X_irredux` chiama due volte `X_eh_setup`:

1. `X_eh_setup(-iq,...)` costruisce le energie locali delle transizioni.
2. Dopo il raggruppamento, `X_eh_setup(+iq,...)` ricostruisce le stesse transizioni e ne scrive i descrittori nell'ordine energetico raggruppato.

I loop principali in `src/pol_function/X_eh_setup.F`, intorno alle linee 59–139, sono:

```fortran
do i_sp=1,n_sp_pol
  do ik_bz=1,Xk%nbz
    do iv=X%ib(1),X%ib(1)+Nv(i_sp)-1
      do ic=ic_min,X%ib(2)
```

Ogni indice viene filtrato attraverso la propria maschera MPI:

- `PAR_IND_Xk_bz%element_1D(ik_bz)`;
- `PAR_IND_VAL_BANDS_X(... )%element_1D(iv)`;
- `PAR_IND_CON_BANDS_X(... )%element_1D(ic)`.

Per una normale transizione senza terminator, l'energia è:

```fortran
E_eh = Xen%E(ic,ik,i_sp) - Xen%E(iv,ik_m_q,i_sp)
```

`ik_m_q` viene ricavato attraverso `qindx_X`. Il fattore di occupazione è:

```fortran
f_eh = Xen%f(iv,ik_m_q,i_sp) * &
       (spin_occ-Xen%f(ic,ik,i_sp)) / spin_occ
```

Le transizioni con occupazione numericamente nulla o esterne all'intervallo `X%ehe` vengono escluse.

Nel run fornito ci sono:

- una polarizzazione di spin;
- 216 k-point nella BZ completa;
- 24 bande di valenza;
- 247 bande di conduzione locali;
- nessun cutoff attivo sull'energia elettrone-lacuna;
- occupazioni isolanti a temperatura zero.

Ogni rank ha quindi:

```text
216 x 24 x 247 = 1.280.448 transizioni grezze per q
```

Questo valore coincide esattamente con il secondo campo `[X-CG]` in ogni log di processo.

## 3. Raggruppamento delle transizioni

Il raggruppamento è implementato da `FREQUENCIES_coarse_grid` in `src/common/FREQUENCIES_coarse_grid.F`.

### Criterio

I valori locali grezzi di `E_eh` vengono ordinati. Partendo da una transizione di riferimento per ciascun gruppo, una nuova transizione viene aggiunta allo stesso gruppo quando:

```fortran
abs(E_eh-E_reference) <= 1.e-5 Hartree
```

La tolleranza equivale a circa `2,72e-4 eV`.

Il criterio usa il riferimento del gruppo e non una concatenazione transitiva fra elementi adiacenti: quando inizia un nuovo gruppo, il suo primo elemento diventa il nuovo riferimento.

Quando è attivo un terminator X, sia l'energia di transizione sia l'energia dello stato iniziale devono rispettare due tolleranze separate pari a `1.e-5`. Nel run fornito `XTermKind="none"`, quindi conta soltanto `E_eh`.

### Rappresentazione dei gruppi

Gli array rilevanti sono:

- `ordered_grid_index(raw_index)`: posizione della transizione originale nell'ordine energetico;
- `coarse_grid_index(raw_index)`: indice del suo gruppo coarse;
- `bare_grid_N(group)`: numero di transizioni grezze nel gruppo;
- `coarse_grid_Pt(group)`: energia media del gruppo;
- `X_poles_tab(sorted_index,:)`: descrittore `(ik_bz,iv,ic,spin)`.

La seconda chiamata `X_eh_setup(+iq,...)` usa `ordered_grid_index` per popolare `X_poles_tab` nello stesso ordine usato da `bare_grid_N`.

Dentro `X_irredux`:

```fortran
i_bg = sum(bare_grid_N(1:i_cg-1)) + 1
```

seleziona la prima transizione ordinata del gruppo `i_cg`. Il suo descrittore viene passato a `X_GreenF_analytical` come rappresentante del gruppo. Le energie esatte delle transizioni nel gruppo differiscono al massimo della tolleranza stabilita.

### Quale lavoro viene realmente ridotto

Il raggruppamento non elimina le transizioni appartenenti al gruppo. `X_irredux_residuals` continua a iterare su tutti i `bare_grid_N(i_cg)` membri e include il residuo di ciascun oscillatore.

La riduzione riguarda il lavoro che dipende soltanto dall'energia di transizione:

- viene costruita una matrice residuo per l'intero gruppo;
- viene valutata una funzione di Green per il gruppo;
- il residuo raggruppato viene accumulato in Xo una sola volta per ogni frequenza di output.

Quindi:

```text
transizioni grezze
   -> contribuiscono tutte con i propri elementi di matrice

gruppi energetici
   -> determinano il numero di valutazioni della funzione di Green
      e di accumuli nella matrice Xo
```

Il raggruppamento avviene indipendentemente su ciascun rank dopo la distribuzione `c`. Non esiste alcuna operazione collettiva in `FREQUENCIES_coarse_grid`, né una redistribuzione basata sul numero di gruppi ottenuto.

## 4. Significato esatto di `[X-CG] R(p) Tot o/o(of R)`

Il messaggio viene generato in `src/common/FREQUENCIES_coarse_grid.F`, intorno alle linee 178–181:

```fortran
write(ch,'(3a)') '[',title,'-CG] R(p) Tot o/o(of R)  '
call msg('rs',trim(ch), &
         (/coarse_grid_N,npts, &
           int(real(coarse_grid_N)/real(dncg)*100._SP)/))
```

I campi sono:

1. `coarse_grid_N`: numero locale finale di gruppi di energie di transizione;
2. `npts`: numero locale di transizioni grezze accettate;
3. `100*coarse_grid_N/dncg`: frazione mantenuta rispetto alla griglia non degenere inizialmente rilevata.

La percentuale finale non è `coarse_grid_N/npts`.

Il run usa `X poles = 100%`. Pertanto non viene applicata alcuna ulteriore riduzione coarse oltre al raggruppamento delle degenerazioni entro `1.e-5`, e vale:

```text
coarse_grid_N = dncg
```

Questo spiega perché una riga può mostrare:

```text
[X-CG] R(p) Tot o/o(of R): 118887 1280448 100
```

anche se `118887/1280448` è soltanto il 9,28%.

Per questo run, il primo numero è contemporaneamente:

- il numero locale di gruppi energetici;
- il numero di punti della coarse grid;
- il numero di iterazioni del loop principale `i_cg=1,coarse_grid_N`;
- sostanzialmente il numero di chiamate a `X_irredux_residuals` e di accumuli raggruppati in Xo.

Non è il numero di transizioni grezze.

## 5. Lavoro costoso in Xo

### Implementazione usata dal run fornito

Alla revisione `a4a1dfaa9`, ogni iterazione `i_cg` esegue:

1. azzeramento di `Xo_res`;
2. loop su tutte le transizioni grezze in `bare_grid_N(i_cg)`;
3. chiamata a `scatter_Bamp_gpu` per costruire l'oscillatore;
4. memorizzazione dell'oscillatore di ogni transizione in `rhotw_mp_R` e, quando richiesto dalla distribuzione righe/colonne, in `rhotw_mp_C`;
5. costruzione dell'intero residuo del gruppo con una chiamata a `devxlib_xGEMM_gpu`, la cui dimensione interna è `nt=bare_grid_N(i_cg)`;
6. valutazione di `X_GreenF_analytical` una volta per il gruppo;
7. accumulo del residuo raggruppato in Xo per ogni frequenza di output.

Il run ha una matrice X di dimensione 753 e due frequenze PPA. Sono quindi importanti sia le operazioni di matrice per transizione sia quelle per gruppo.

Un modello utile del carico per l'implementazione eseguita è:

```text
lavoro ~= Nraw * (Coscillatore + Cpacking)
        + somma_gruppi CGEMM(nt=bare_grid_N(gruppo))
        + Ncg * (Cazzeramento-matrice + CGreenF
                 + Nfreq * Caccumulo-matrice)
```

Di conseguenza:

- `Nraw` da solo non basta, perché è identico su tutti i rank. Coincide con `somma_gruppi bare_grid_N(gruppo)`, quindi anche la somma delle dimensioni interne delle GEMM è identica;
- `coarse_grid_N` determina il numero di azzeramenti di `Xo_res`, chiamate GEMM, valutazioni della funzione di Green e accumuli sulle frequenze;
- a `Nraw` fissato, più gruppi significano gruppi mediamente più piccoli: più GEMM piccole, efficienza GPU potenzialmente inferiore e maggior overhead per gruppo;
- questo rafforza l'interpretazione quantitativa dei log: P1 ha in media solo 4,94 transizioni per gruppo, contro 7,04 per P8, quindi esegue più chiamate GEMM con un batching peggiore;
- `coarse_grid_N` non è comunque un modello scalare esatto del costo: contano anche la distribuzione completa delle dimensioni dei gruppi, l'efficienza delle singole GEMM, il riuso degli oscillatori per q=1 e il comportamento della memoria.

## 6. Analisi quantitativa del run fornito

Ci sono 30 chiamate Xo, una per ciascun q-point. Il report principale contiene i valori `[X-CG]` di P1 perché l'output del report è riservato a quel processo, mentre ogni log di processo contiene i propri valori locali.

Nella tabella:

- `CG q1` è il primo valore `[X-CG]`;
- `Somma CG` è la somma sui 30 q-point;
- le colonne relative sono normalizzate a P8, che ha il valore minimo;
- gli intervalli delle bande sono ricostruiti dall'algoritmo di distribuzione contigua.

| Processo | Rank MPI | Intervallo c | Numero c | CG q1 | Somma CG, 30 q | CG/P8 | Xo procedure (s) | Tempo/P8 | Xo REDUX (s) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| P1 | 0 | 25–271 | 247 | 118.887 | 7.771.736 | 1,424 | 5736,3 | 1,794 | 0,6 |
| P2 | 1 | 272–518 | 247 | 110.710 | 6.708.925 | 1,229 | 4412,2 | 1,380 | 1334,9 |
| P3 | 2 | 519–765 | 247 | 106.809 | 6.275.420 | 1,150 | 3961,8 | 1,239 | 1785,2 |
| P4 | 3 | 766–1012 | 247 | 104.521 | 6.039.288 | 1,106 | 3731,6 | 1,167 | 2015,5 |
| P5 | 4 | 1013–1259 | 247 | 103.103 | 5.893.370 | 1,080 | 3571,6 | 1,117 | 2175,1 |
| P6 | 5 | 1260–1506 | 247 | 101.640 | 5.738.892 | 1,051 | 3402,1 | 1,064 | 2344,7 |
| P7 | 6 | 1507–1753 | 247 | 99.625 | 5.534.715 | 1,014 | 3225,7 | 1,009 | 2521,0 |
| P8 | 7 | 1754–2000 | 247 | 98.702 | 5.458.797 | 1,000 | 3197,3 | 1,000 | 2549,4 |

Ulteriori osservazioni quantitative:

- Le transizioni grezze sui 30 q-point sono identiche: 38.413.440 per rank.
- P1 ha in media circa 4,94 transizioni grezze per gruppo energetico.
- P8 ha in media circa 7,04 transizioni grezze per gruppo energetico.
- P1 beneficia quindi di un raggruppamento sensibilmente minore.
- `CG(P1)/CG(P8) = 1,424`.
- `tempo(P1)/tempo(P8) = 1,794`.
- La correlazione di Pearson fra `Somma CG` e `Xo procedure` sui rank è 0,9976.
- P1 ha il maggior numero di gruppi in ognuno dei 30 q-point; i valori diminuiscono quasi monotonicamente da P1 a P8.

Il fatto che il rapporto dei tempi sia maggiore del rapporto dei gruppi conferma che `[X-CG]` è un ottimo indicatore, ma non un modello completo del numero di operazioni.

## 7. Perché P1 è il rank più lento

### Spiegazione dimostrata

P1 possiede il blocco più basso di bande di conduzione, 25–271. Per ogni q-point, le transizioni di questo blocco collassano in gruppi con meno membri rispetto alle transizioni dei blocchi di bande progressivamente più alte. P1 esegue quindi il maggior numero di iterazioni della coarse grid.

In modo equivalente:

- P1 ha più energie di transizione locali distinte;
- P1 ha più gruppi energetici;
- P1 ha gruppi mediamente più piccoli;
- P1 esegue più accumuli di matrice per gruppo e più lanci di kernel GPU.

### Il rank 0 ha un ruolo intrinsecamente speciale?

Non nella regione di calcolo misurata da `Xo (procedure)`.

Nell'implementazione usata dal run:

- `master_thread` indica il thread OpenMP master di ogni rank MPI, non il rank MPI 0;
- il run usa un solo thread X per ogni rank/GPU;
- non esiste un ramo computazionale riservato al rank 0 capace di spiegare migliaia di secondi;
- il ramo Drude non è attivo per questo calcolo isolante;
- l'I/O dei dipoli e il caricamento delle funzioni d'onda avvengono prima dell'avvio del timer `Xo (procedure)`;
- l'I/O di report e database è misurato separatamente ed è di ordini di grandezza inferiore.

P1 possiede privilegi di report e I/O in altre parti del programma, ma il report mostra soltanto circa 11 s per `io_X`, rispetto a una differenza P1/P8 di 2539 s nella procedura Xo.

### Spiegazioni alternative considerate

#### Bande aggiuntive dovute al resto

Escluso per questo run: ogni rank possiede esattamente 247 bande di conduzione.

#### Numero diverso di transizioni grezze

Escluso: ogni log riporta 1.280.448 transizioni grezze per q-point.

#### Asimmetria fra nodi o GPU

P1–P4 sono stati eseguiti su `lrdn1018`, mentre P5–P8 su `lrdn1214`. Il numero di gruppi e i tempi cambiano regolarmente con l'indice delle bande anche attraversando il confine P4/P5 fra i nodi. Ciò rende molto improbabile che il nodo o una GPU anomala siano la causa principale.

#### Asimmetria nelle comunicazioni o riduzioni MPI

È una conseguenza, non la causa iniziale. P8 trascorre più tempo nella riduzione proprio perché termina prima il proprio calcolo locale.

#### Motivo microscopico del maggior raggruppamento delle bande alte

I log dimostrano che i blocchi di bande di conduzione più alte generano meno gruppi, ma non ne identificano la causa microscopica. Per distinguere fra degenerazione delle bande, dispersione, simmetria e coincidenze energetiche in singola precisione sarebbe necessario analizzare i valori `E_eh` e l'appartenenza effettiva ai gruppi.

## 8. Sincronizzazioni e riduzioni MPI

Alla fine di `X_irredux`, il timer della procedura viene fermato e viene avviato quello della riduzione. Per ogni frequenza, il codice riduce soltanto `X_par%blc(:,:,iw)` sul comunicatore `PAR_COM_X_WORLD_RL_resolved`, come mostrato intorno alle linee 438–450 di `src/pol_function/X_irredux.F`.

`PP_redux_wait` per una matrice complessa, in `src/modules/mod_parallel_interface.F`, esegue:

1. `MPI_Barrier`;
2. `MPI_Allreduce(...,MPI_SUM,...)`;
3. un secondo `MPI_Barrier`.

Il timer misura tempo reale trascorso. Con OpenMP attivo usa `qe_cclock`, implementato tramite `gettimeofday` in `src/tools/ct_cptimer.c`.

Il run fornisce una prova diretta che i rank più veloci aspettano P1:

| Processo | Xo procedure + REDUX (s) |
|---|---:|
| P1 | 5736,9 |
| P2 | 5747,1 |
| P3 | 5747,0 |
| P4 | 5747,2 |
| P5 | 5746,7 |
| P6 | 5746,8 |
| P7 | 5746,7 |
| P8 | 5746,7 |

I tempi di riduzione dei rank più veloci compensano quasi esattamente la differenza rispetto a P1. Il tempo totale di Xo è quindi determinato dal rank più lento.

## Distinzione fra le quantità di carico

Per il run fornito:

1. **Bande di conduzione assegnate a ogni rank:** esattamente 247.
2. **Transizioni grezze accettate per rank e q-point:** esattamente 1.280.448.
3. **Gruppi energetici dopo il trattamento della degenerazione:** dipendono da rank e q; per q1 sono 118.887 su P1 e 98.702 su P8.
4. **Primo numero `[X-CG]`:** il numero finale di gruppi `coarse_grid_N`; qui coincide anche con il numero di iterazioni del loop esterno Xo.
5. **Lavoro costoso effettivo:** combinazione del lavoro fisso sulle transizioni grezze e del lavoro dipendente dai gruppi per azzeramento del residuo, valutazione della funzione di Green, accumulo in frequenza e lancio/batching dei kernel GPU.

Queste quantità non sono intercambiabili.

## Risposte dirette

### A. Il carico MPI iniziale è bilanciato soltanto in base alle bande di conduzione?

Sì, per il livello di parallelizzazione `c` selezionato. Viene bilanciato il numero di indici di banda contigui, senza considerare la validità delle transizioni, la degenerazione, i gruppi della coarse grid, il costo delle matrici o il tempo misurato. In questo run anche il numero di transizioni grezze risulta casualmente identico.

### B. Il successivo raggruppamento delle transizioni introduce uno sbilanciamento sostanziale?

Sì. Il numero totale di gruppi locali differisce del 42,4% fra P1 e P8, nonostante il numero di bande e di transizioni grezze sia identico.

### C. È questa la spiegazione principale della differenza temporale P1/P8?

Sì, con evidenze molto forti: la correlazione è 0,998, P1 ha il maggior numero di gruppi a ogni q-point e i tempi di attesa nella riduzione compensano la differenza di calcolo. Il numero di gruppi non è però un modello esatto del costo, quindi non spiega da solo perché il rapporto dei tempi sia 1,794 anziché 1,424.

### D. Perché P1/rank 0 è il più lento?

La distribuzione contigua gli assegna le bande 25–271, che producono il maggior numero di gruppi locali distinti di energie di transizione. Non esiste una responsabilità computazionale intrinseca del rank 0 in Xo che possa spiegare la differenza. Il rank 0 è esposto indirettamente perché l'algoritmo gli assegna sempre il blocco di bande più basse.

### E. In quale fase dovrebbe cambiare la distribuzione per bilanciare il lavoro effettivo?

La decisione di bilanciamento deve avvenire dopo che le energie di transizione, o pesi per banda sufficientemente accurati, sono note, ma prima del loop `i_cg` di Xo.

Sono possibili due livelli:

- Una modifica più limitata potrebbe sostituire i blocchi `c` contigui ed equinumerosi con una proprietà delle bande pesata o mista prima di `X_eh_setup`.
- Un bilanciamento esatto del lavoro sui gruppi coarse richiederebbe prima il raggruppamento delle energie e poi la distribuzione dei gruppi o delle unità di lavoro risultanti, eventualmente separando la proprietà del lavoro dalla proprietà delle bande e delle funzioni d'onda.

## Possibili direzioni per migliorare il bilanciamento

- Usare blocchi `c` non contigui o ciclici, mescolando regioni di bande basse e alte. Probabilmente ridurrebbe l'andamento monotono osservato, ma non garantirebbe il bilanciamento.
- Stimare un costo per banda dal numero di gruppi distinti di `E_c(k)-E_v(k-q)` e usare maschere esplicite pesate.
- Costruire globalmente il lavoro sulle transizioni raggruppate e distribuire direttamente i gruppi coarse. È la soluzione più fedele, ma richiede di riconsiderare la località delle funzioni d'onda, l'accesso agli oscillatori e la strategia della riduzione finale.
- Usare una stima ibrida che includa sia il numero di transizioni grezze sia quello dei gruppi, perché tutti i membri grezzi comportano comunque lavoro sui residui e `[X-CG]` non rappresenta l'intero costo del kernel.

## Classificazione delle evidenze

### Dimostrato direttamente dal sorgente e dai log

- Distribuzione `c` contigua basata sul numero di bande.
- Intervalli esatti delle bande per ogni rank nel run.
- Identico numero di transizioni grezze.
- Raggruppamento locale successivo alla distribuzione, con criterio di `1.e-5` Hartree.
- Significato esatto dei tre numeri `[X-CG]`.
- Sbilanciamento del numero di gruppi e sua correlazione con i tempi Xo.
- Sincronizzazione barrier/all-reduce e tempi complementari di attesa in `Xo (REDUX)`.
- Assenza di un ramo riservato al rank 0 che abbia un costo rilevante nella procedura Xo misurata.

### Interpretazione fortemente sostenuta dalle evidenze

- Lo sbilanciamento dei gruppi coarse è la causa dominante dello sbilanciamento osservato in `Xo (procedure)`.
- P1 è lento perché il suo blocco di bande di conduzione più basse presenta una minore degenerazione effettiva delle energie di transizione.

### Non determinato dai dati disponibili

- La ragione fisica dettagliata per cui le bande di conduzione più alte del materiale producono un maggior raggruppamento.
- Una singola formula scalare esatta del costo ricavabile dai log: non riportano la distribuzione completa delle dimensioni dei gruppi né l'efficienza di ciascuna GEMM.

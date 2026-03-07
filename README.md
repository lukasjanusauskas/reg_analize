## Regresinės analizės laboratoriniai 2 grupė 1 pogrupis 

### 1 laboratorinis

Paleidimo instrukcijos:

Norint padaryti pdf'ą (ar word'ą):

```
setwd('<path'as į 1 lab direktoriją, ne reg_analize>')
bookdown::render_book()
```

Norint paleisti - leidžiasi iš abiejų direktorijų tai funkcijos `setwd` neturėtų prireikti.

Padaryti: 
-	Šablonų paruošimas, github’as (Martynas - padaryta)
-	Duomenų aprašymas (Lukas - šeštadienį),
-	Pradinė analizė - bent 5 stebėjimai kiekvienai kateogrijų kombinacijai, išrinkti kategorinius kintamuosius (Lukas ir Martynas - daroma),
-	Klasių balansavimo problema (Lukas),
-	Prielaidų tikrinimas kiekvienam modeliui - vienas žmogus vienam modeliui (Edvin - Logit, Lukas - Probit, Nikita - log-log),
-	Multikolinearumas (Martynas),
-	Išskirtys - kuko matas ir Pirsono liekanos (Edvin),
-	Po prielaidų train-test split’ą padaryti
-	Parametrų vertinimas - paklausti ko tiksliai reikia, kokie reikalavimai
-	Požymių reikšmingumas (Martynas),
-	Sąveikos (Edvin),
-	Slenksčio parinkimas (Martynas),
-	Klasifikavimo matrica po kiekvieno modelio (Kiekvienas savo modelį),
-	ROC kreivė (Kiekvienas savo modelį),
-	Interpretacija - geriausiam modeliui (Lukas).

### Pradinė analizė ir duomenų paruošimas

Paruošimo etapas:

1. Duomenų paruošimas faktoriais
2. Praleistų reikšmių tyrimas: parašyti, kad nebuvo nereikia tos lentelės
3. Loginių išskirčių tyrimas: požymių min-max ir t.t.

Duomenų analizė:

1. Klasės reikšmių pasiskirstymas;
2. Kategorinių kintamųjų dažniausia vertė, unikalių verčių skaičius;
3. Histogramos natūralių skaičių duomenims;
4. Kategorinių užkoduotų kintamųjų požymių dažnių lentelės pagal sėkmingo apsilankymo reikšmes ir bendrai;
5. Smuiko grafikai;
6. Stulpeliniai grafikai;

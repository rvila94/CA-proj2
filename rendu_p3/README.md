**How to use**
Vous pouvez simplement vous placer dans le dossier et exécuter la commande suivante :
```bash
dune build
dune exec ./cam_test.exe
```
Pour des raisons de temps, ce dossier contient les mêmes fichiers que celui de la partie 1 et 2, mais changés pour
l'implémentation de la partie 3.

Sur le dune build, il prend souvent un peu de temps quand il est à 96%, mais il passe.

Vous trouverez dans le dossier eclat-master le compilateur de ECLAT, et les fichiers du projet de la partie 3.
Vous pouvez donc faire 
```bash
cd eclat-master/eclat-compiler
./eclat -arg="true;true;false" code/code.ecl code/main.ecl
```
Pour que vous puissiez lire le code sans faire des allers-retours désagréables, tous les fichiers sont aussi en double
présent le dossier rendu. 
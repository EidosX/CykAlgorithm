# TP 2 de language formel

NOM: Diego Imbert  
Repo: https://github.com/RedRikudo/StackAutomata

## Lancer le programme

Dans la racine du projet, ```./bin/stack-automata path/to/automata expression```

## Structure du projet

La fonction main se trouve dans `./app`.  
`./src` contient des modules utilisés par main.  
Le dossier `tests/res` contient des automates de test.  
Les options de compilations et les dépendances sont dans `package.yaml`.

Les fonctions du TP1 se trouvent dans src/Automata/Automata.hs
Les fonctions du TP2 se trouvent dans src/Grammar/CNFMaker.hs et Grammar.hs
La fonction du TP3 se trouve dans src/Grammar/CYK.hs et la fonction main dans app/Main.hs

## Build (Pas nécessaire)

> Ce programme utilise le build system `stack 2.5.1` et le compilateur `ghc 8.10.4`.  
> Ces outils et les dépendances étant longs à télecharger, je fournirais le TP terminé et pré-compilé dans `./bin` (linux x64 et windows x64).

Dans la racine du projet, ```stack run -- path/to/automata expression```
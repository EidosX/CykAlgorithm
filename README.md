# TP 2 de language formel

NOM: Diego Imbert
Repo: https://github.com/RedRikudo/StackAutomata

> Ce programme utilise le build system `stack 2.5.1` et le compilateur `ghc 8.10.4`.  
> Ces outils et les dépendances étant longs à télecharger, je fournirais le TP terminé et pré-compilé dans `./bin` (linux x64 et windows x64).

## Lancer le programme

Dans la racine du projet, ```./bin/stack-automata path/to/automata expression```

## Structure du projet

La fonction main se trouve dans `./app`.  
`./src` contient des modules utilisés par main.  
Le dossier `automatas` contient des automates de test.  
Les options de compilations et les dépendances sont dans `package.yaml`.
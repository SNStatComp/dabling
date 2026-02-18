# dabling
AI-assisted classification

Dabling is a research project of Statistics Netherlands.

This repo contains a shiny dashboard developed within the joint international project [ESSnet AIML4OS Work Package 10](https://github.com/AIML4OS/WP10) that shows what can be done with the Dabling concept for classification.

The user can give in an OpenAI API Key that will be used for semantic text enrichtment in the classification task. The costs for this API key are for the user. With this demo, it will never exceed <TDOD> tokens, which is less then <TODO> Eur.

This first version of the dashboard uses:
- the AGNews dataset from the [textdata R-package](https://cran.r-project.org/package=textdata). The AGNews dataset is a collection of news articles, where each article is labeled with a topic (e.g. politics, sports, etc.).
- The AG’s news topic classification. This dataset is constructed by choosing 4 largest classes from the original corpus. Each class contains 30,000 training samples and 1,900 testing samples. The total number of training samples is 120,000 and testing 7,600. Version 3, Updated 09/09/2015

A classification to show the concept. Additional classifications and examples will be added later.

Comments and suggestions welcome at:
- Jeldrik Bakker: j \<dot\> bakker \<at\> cbs.nl
- Olav ten Bosch: o \<dot\> tenbosch \<at\> cbs.nl

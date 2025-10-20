# Project Directory Tree

Show in New Window

    ├── analysis
    │   ├── nonparametric
    │   │   ├── kde.html
    │   │   └── kde.Rmd
    │   ├── nonrankbased
    │   │   ├── nonrankbased.html
    │   │   └── nonrankbased.Rmd
    │   ├── parametric
    │   │   ├── parametric.html
    │   │   └── parametric.Rmd
    │   └── references.bib
    ├── data
    │   └── mean_travel_time_ranking_2011.rds
    ├── figures
    │   ├── ind_ranks_plot.png
    │   └── kde_ranks_plot.png
    ├── R
    │   ├── compute_ci.R
    │   ├── compute_metrics.R
    │   └── implement.R
    ├── references
    │   └── KleinWrightWieczorek2020.pdf
    ├── writeup
    │   ├── child
    │   │   ├── abstract.Rmd
    │   │   ├── appendix.Rmd
    │   │   ├── introduction.Rmd
    │   │   ├── methodology.Rmd
    │   │   ├── result.Rmd
    │   │   └── rrl.Rmd
    │   ├── results
    │   │   └── alpha0p05_k10.csv
    │   ├── harvard-educational-review.csl
    │   ├── proposal.log
    │   ├── proposal.pdf
    │   ├── proposal.Rmd
    │   └── references.bib
    ├── README.md
    ├── README.Rmd
    └── renv.lock
    
  
# To-Do

- [ ] Using Klein's result, check what happens when there are ties
- [ ] Prepare code to run over the weekend (choose a different correlation structure)
- [x] Read full text - [Klein](https://github.com/ShaineRosewel/kde-ranking/tree/master/references)
- [x] Structure writeup template: Use `multipleStrategies` repo as pattern to edit content
- [x] Update texLive and test that citation works - [refer to stackoverflow solution](https://stackoverflow.com/questions/79087384/pandoc-rstudio-failing-simple-rmarkdown-document-with-undefined-control-sequenc?noredirect=1#comment139455462_79087384)
- [x] Study seeding when using doPar
- [x] Review the way you coded nonrankbased

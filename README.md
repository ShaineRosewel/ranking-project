# Project Directory Tree

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
    │   ├── kde_ranks_plot.png
    ├── R
    │   ├── algo1_nonparametric.R
    │   ├── algo1_nonrankbased.R
    │   ├── algo1_parametric.R
    │   ├── algo2_helper.R
    ├── references
    │   └── KleinWrightWieczorek2020.pdf
    ├── writeup
    │   ├── child
    │   │   ├── abstract.Rmd
    │   │   ├── appendix.Rmd
    │   │   ├── introduction.Rmd
    │   │   ├── methodology.Rmd
    │   │   └── rrl.Rmd
    │   ├── harvard-educational-review.csl
    │   ├── proposal.pdf
    │   ├── proposal.Rmd
    │   └── references.bib
    ├── README.md
    ├── README.Rmd
    └── renv.lock
    
  
# To-Do

- [ ] Read full text - [Klein](https://github.com/ShaineRosewel/kde-ranking/tree/master/references)
- [ ] Structure writeup template: Use `multipleStrategies` repo as pattern to edit content
- [x] Update texLive and test that citation works - [refer to stackoverflow solution](https://stackoverflow.com/questions/79087384/pandoc-rstudio-failing-simple-rmarkdown-document-with-undefined-control-sequenc?noredirect=1#comment139455462_79087384)
- [ ] Study seeding when using doPar
- [ ] Review the way you coded nonrankbased

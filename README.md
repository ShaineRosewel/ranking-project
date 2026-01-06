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
    │   │   ├── alpha0p05_k10.csv
    │   │   ├── simulation_results_ocorr.RData
    │   │   └── simulation_results.RData
    │   ├── harvard-educational-review.csl
    │   ├── proposal.log
    │   ├── proposal.pdf
    │   ├── proposal.Rmd
    │   └── references.bib
    ├── README.html
    ├── README.md
    ├── README.Rmd
    └── renv.lock

# Timeline

- Each `K` needs to be ran for 3 different cases of `sd` and 5 `R` (3 for equicorrelation and 2 for block). We consider 16 for allowance. This should all be done by February, this already has (excessive) buffer.
  - [ ] K=50 for 16 cases ~10 days
  - [ ] K=40 for 16 cases ~8 days
  - [ ] K=30 for 16 cases ~6 days
  - [ ] K=20 for 16 cases ~4 days
  - [ ] K=10 for 6 cases ~1 day
  - [x] K=10: 1 day for 9/16 cases done
- Application to real data. This should be done by mid-March.
  
# To-Do
- [ ] Prepare codes for block correlation assumption
- [ ] Prepare codes for read data application
- [x] Read full text - [Klein](https://github.com/ShaineRosewel/kde-ranking/tree/master/references)
- [x] Structure writeup template: Use `multipleStrategies` repo as pattern to edit content
- [x] Update texLive and test that citation works - [refer to stackoverflow solution](https://stackoverflow.com/questions/79087384/pandoc-rstudio-failing-simple-rmarkdown-document-with-undefined-control-sequenc?noredirect=1#comment139455462_79087384)
- [x] Study seeding when using doPar
- [x] Review the way you coded nonrankbased

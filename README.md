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
  - [ ] for real data application, use only those with at least 1% votes; for se - use that of binomial dist 
  - [x] add case for 3 blocks setting should make the difference obvious - 5 days
  - [x] K=50 for 9/9 cases (~18 hours per case)
  - [x] K=40 for 9/9 cases (~13 hours per case)
  - [x] K=30 for 9/9 cases (~8 hours per case)
  - [x] K=20 for 9/9 cases (~5 hours per case)
  - [x] K=10 for 9/9 cases (~3 hours per case)
  - [x] K=50: staggered runs for 9/16 case (~18 hours per case)
  - [x] K=20: 3.25 days for 9/16 cases done (~5 hours per case)
  - [x] K=20: 2 days for 9/16 cases done (~5 hours per case)
  - [x] K=10: 1 day for 9/16 cases done (~3 hours per case)
- Remaining cases will take around 15 days when ran non-stop. For allowance, we set to 20 days, including studies of which block correlations will be used.
- Write-up for results: 3 days.
- Follow changes from overleaf to Rmarkdown: 2 days
- Application to real data including writeup: 5 days. 
- Submit to Sir Mike for checking: 15 days
- This should be done March.
  
# To-Do
- [x] Prepare codes for block correlation assumption
- [ ] Prepare codes for read data application
- [x] Read full text - [Klein](https://github.com/ShaineRosewel/kde-ranking/tree/master/references)
- [x] Structure writeup template: Use `multipleStrategies` repo as pattern to edit content
- [x] Update texLive and test that citation works - [refer to stackoverflow solution](https://stackoverflow.com/questions/79087384/pandoc-rstudio-failing-simple-rmarkdown-document-with-undefined-control-sequenc?noredirect=1#comment139455462_79087384)
- [x] Study seeding when using doPar
- [x] Review the way you coded nonrankbased

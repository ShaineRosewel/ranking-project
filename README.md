# Project Directory Tree

    ├── analysis
    │   ├── kde.html
    │   ├── kde.Rmd
    │   └── references.bib
    ├── data
    │   ├── kde_intervals_10.rds
    │   ├── kde_intervals_20.rds
    │   ├── kde_intervals_30.rds
    │   ├── kde_intervals_40.rds
    │   ├── kde_intervals_51.rds
    │   └── mean_travel_time_ranking_2011.rds
    ├── figures
    │   ├── ind_ranks_plot.png
    │   └── kde_ranks_plot.png
    ├── output
    │   ├── algo2.rds
    │   ├── coverage_probability_10.rds
    │   ├── coverage_probability_20.rds
    │   ├── coverage_probability_30.rds
    │   ├── coverage_probability_40.rds
    │   ├── coverage_probability_51.rds
    │   └── iters
    ├── R
    │   ├── algo1_helper.R
    │   └── algo2_helper.R
    ├── README.html
    ├── README.md
    ├── README.Rmd
    ├── renv.lock

# Experiments to-do

- [] Using the same DGP, have the theta's overlap, retain the SDs  
- [] Using a different bs approach for simulation - ie, generate an entire bs sample from MVRnorm instead of individually sampling from each K centered normal distribution for each state  

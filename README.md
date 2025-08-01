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
    │   ├── kde_ranks_plot.png
    │   ├── onethird0p05sd_twothrid1p20sd_even2p0f.png
    │   ├── onethird1p20sd_twothird0p05sd_even0p05f.png
    │   ├── onethird1p20sd_twothird0p05sd_even0p5f.png
    │   ├── onethird1p20sd_twothird0p05sd_even1p0f.png
    │   ├── onethird1p20sd_twothird0p05sd_even2p0f.png
    │   ├── sample_data_monte_carlo_1p2sd_2p0f.png
    │   ├── simple_data_monte_carlo_0p01sd_2p0f.png
    │   ├── simple_data_monte_carlo_0p05sd_2p0f.png
    │   ├── simple_data_monte_carlo_0p1sd_0p5f.png
    │   ├── simple_data_monte_carlo_0p1sd_2p0f.png
    │   ├── simple_data_monte_carlo_0p7sd_0p5f.png
    │   ├── simple_data_monte_carlo_0p7sd_2p0f.png
    │   ├── simple_data_monte_carlo_0p7sd_5p0f.png
    │   ├── simple_data_monte_carlo_0p7sd_7p0f.png
    │   └── simple_data_monte_carlo_0p9sd_2p0f.png
    ├── R
    │   ├── algo1_helper.R
    │   └── algo2_helper.R
    ├── README.md
    ├── README.Rmd
    └── renv.lock

# Experiments to-do

- [x] Using the same DGP, have the theta's overlap, retain the SDs
- [x] Using the same DGP, set to a higher SD that allows overlap of samples  
- [x] Using a different bs approach for simulation - ie, generate an entire bs sample from MVRnorm instead of individually sampling from each K centered normal distribution for each state: does not preserve the position of the estimate - this lead to a lower coverage ie for SD=0.7 that uses the data generating fcn, from 0.72 using the original bs meth, it became 0.69 for K=10, 100 reps

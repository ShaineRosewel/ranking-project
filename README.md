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
    │   ├── original_data_minus_the_sorting.png
    │   ├── original_data_sorted_S_dec.png
    │   ├── original_data_sorted_S_inc.png
    │   ├── original_data_sorting_bs_accdg_to_theta_k.png
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
    ├── references
    │   └── KleinWrightWieczorek2020.pdf
    ├── README.md
    └── renv.lock
    
# Runtime Notes

This is for 5000 replications with 1000 bootstrap samples.

| K  | hh:mm:ss   |
|----|------------|
| 10 | 00:35:57.42 |
| 20 | 01:08:57.12 |
| 30 | 01:37:38.38 |
| 40 | 02:05:10.57 |
| 51 | 02:40:11.92 |

  
# To-Do

- [ ] Read full text - [Klein](https://github.com/ShaineRosewel/kde-ranking/tree/master/references)
- [ ] Structure writeup template: Use `multipleStrategies` repo as pattern to edit content
- [x] Update texLive and test that citation works
- [ ] Check if inverse can have limited data driven search space

# Experiments Summary

- Coverage starts to diminish when $\theta$ s overlap.
- Sorting is what lowers the coverage but without it T1, T2, & T3 are only as good as Klein's.

**For K=10, 100 reps**

Fixed even spacing between $\theta$ s; increasing SD

| $\theta$ s      | SD    | Coverage |
|----------------------|-------|----------|
| seq(1, 2.0*10, 10)   | 0.05  | 0.89     |
| seq(1, 2.0*10, 10)   | 0.10  | 0.89     |
| seq(1, 2.0*10, 10)   | 0.70  | 0.72     |
| seq(1, 2.0*10, 10)   | 0.90  | 0.50     |
| seq(1, 2.0*10, 10)   | 1.20  | 0.42     |


Decreasing even spacing between $\theta$s; fixed, higher SD

| $\theta$ s      | SD    | Coverage |
|----------------------|-------|----------|
| seq(1, 9.0*10, 10)   | 0.70  | 0.89     |
| seq(1, 7.0*10, 10)   | 0.70  | 0.89     |
| seq(1, 5.0*10, 10)   | 0.70  | 0.89     |
| seq(1, 2.0*10, 10)   | 0.70  | 0.72     |
| seq(1, 0.5*10, 10)   | 0.70  | 0.22     |

 
Various controlled setting:
 
| $\theta$ s      | SD                      | Coverage |
|----------------------|----------------------------------------|----------|
| seq(1, 2.00*10, 10)  | 1/3 with 0.05, 2/3 with 1.20            | 0.42     |
| seq(1, 0.05*10, 10)  | 1/3 with 1.20, 2/3 with 0.05            | 0.21     |
| seq(1, 0.50*10, 10)  | 1/3 with 1.20, 2/3 with 0.50            | 0.89     |
| seq(1, 1.00*10, 10)  | 1/3 with 1.20, 2/3 with 0.50            | 0.89     |
| seq(1, 2.00*10, 10)  | 1/3 with 1.20, 2/3 with 0.50            | 0.89     |
| Original (unsorted) | Original (unsorted)                      | 0.89     |
| Original (sorted dec S) | dec                      | 0.75     |
| Original (sorted inc S) | inc                    | 0.82     |
| Original (sorted get every 5th) | sorted accdgly                | 0.66     |
| BS sorted accdg to estimate $\hat{\theta}$ | sorted accdg to estimate $\hat{\theta}$               | 0.89     |

Please refer to [figures folder](https://github.com/ShaineRosewel/kde-ranking/tree/master/figures) to see results for other K values (20, 30, 40, 51)


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
  
# To-Do

- [ ] Read full text - Klein
- [ ] Structure writeup template

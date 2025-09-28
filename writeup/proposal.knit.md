---
number_sections: true
fig_caption: yes
output:
  bookdown::pdf_document2:
    toc: false
    highlight: kate # monochrome
fontsize: 12pt 
indent: true
header-includes:
- \usepackage{amsmath}
- \numberwithin{equation}{section}
- \usepackage{setspace}\doublespacing
- \usepackage{pdflscape}
- \newcommand{\blandscape}{\begin{landscape}}
- \newcommand{\elandscape}{\end{landscape}}
- \usepackage{indentfirst}
- \usepackage{xparse}
- \usepackage{algorithm}
- \usepackage{algpseudocode}
- \usepackage{float}
- \renewcommand{\ttfamily}{\rmfamily}
papersize: a4
bibliography: /home/realiseshewon/PDev/kde-ranking/writeup/references.bib
csl: /home/realiseshewon/PDev/kde-ranking/writeup/harvard-educational-review.csl
---



\title{\vspace{20mm}TITLE HERE}
\maketitle
\thispagestyle{empty} 
\vspace*{95px}
\begin{center}
A Thesis Proposal Presented to\\
The Faculty of the SS\\
univ\\
\vspace*{100px}
In Partial Fulfillment\\
of the Requirements for the Degree of\\
degree\\
1st Semester A.Y. 2025-2026

\vspace*{100px}
by\\
Nmae
\end{center}


\newpage



``` r
knitr::opts_chunk$set(
  echo    = FALSE,
  message = FALSE,
  warning = FALSE
)
```


# Abstract {-}





\newpage

\tableofcontents

\newpage


# Methodology


Let $\theta_1, \theta_2, \dots, \theta_K$ be the true parameter values and $\hat \theta_1, \hat \theta_2, \dots, \hat \theta_K$ be the estimates obtained.

## Parametric bootstrap

Let $\hat \theta_1, \hat \theta_2, \dots, \hat \theta_K$ be independent but not identically distributed estimates. For this study, it is assumed that $\hat{\theta}_k \sim N \left(\theta_k, \sigma_k^2 \right)$, $k = 1, 2, \dots, K$, where $\sigma^2_k$ is known. Denote the corresponding ordered values by $\hat \theta_{(1)}, \hat \theta_{(2)}, \dots, \hat \theta_{(K)}$.


\begin{algorithm}[H]
	\caption{Computation of Joint Confidence Region via Parametric Bootstrap} 
	\label{alg:parametricbs_ci}
	\begin{algorithmic}[1]
		\For {$b = 1, 2, \dots, B$}
				\State Generate $\hat\theta^*_{bk} \sim N \left( \hat\theta_k, \sigma_k^2 \right)$, $i = 1, 2, \dots, K$ and let $\hat{\theta}_{b(1)}, \hat{\theta}_{b(2)}, \dots, \hat{\theta}_{b(K)}$ be the corresponding ordered values
		    \Statex \begin{minipage}{\linewidth}
          \centering
          \begin{tabular}{|c|c|c|c|c|}
            \hline
             & $k = 1$ & $k = 2$ & $\dots$ & $k = K$ \\
            \hline
            $b = 1$ & $\hat{\theta}^*_{1(1)}$ & $\hat{\theta}^*_{1(2)}$ & \dots & $\hat{\theta}^*_{1(K)}$ \\
            \hline
            $b = 2$ & $\hat{\theta}^*_{2(1)}$ & $\hat{\theta}^*_{2(2)}$ & \dots & $\hat{\theta}^*_{2(K)}$ \\
            \hline
            $\vdots$ & \vdots & \vdots & \dots & \vdots\\
            \hline
            $b = B$ & $\hat{\theta}^*_{B(1)}$ & $\hat{\theta}^*_{B(2)}$ & \dots & $\hat{\theta}^*_{B(K)}$ \\
            \hline
          \end{tabular}
        \end{minipage}
        \State Compute 
        \Statex \begin{minipage}{\linewidth}
        \centering
$\hat\sigma^*_{b(k)} = \sqrt{\text{kth ordered value among} \ \left\{ \hat{\theta}^{*2}_{b1} + \sigma_1^2, \hat{\theta}^{*2}_{b2} + \sigma_2^2, \dots, \hat{\theta}^{*2}_{bK} + \sigma_K^2 \right\} - \hat {\theta}^{*2}_{(k)}}$
        \end{minipage}
				\State Compute $t^*_b = \underset{1 \leq k \leq K}{\max} \Bigg| \frac{\hat\theta^*_{b(k)} - \hat\theta^*_{k}}{\sigma^*_{b(k)}} \Bigg|$
		\EndFor
		\State Compute the $\left(1-\alpha\right)$-sample quantile of $t^*_1, t^*_2, \dots, t^*_B$, call this $\hat{t}$.
		\State The joint confidence region of $\theta_{(1)}, \theta_{(2)}, \dots, \theta_{(K)}$ is given by 
		\Statex \begin{minipage}{\linewidth}
    \centering
$\mathfrak{R} = \left[ \hat\theta_{(1)} \pm \hat t \times \hat\sigma_{(1)}  \right] \times \left[ \hat\theta_{(2)} \pm \hat t \times \hat\sigma_{(2)}  \right] \times \dots \times \left[ \hat\theta_{(K)} \pm \hat t \times \hat\sigma_{(K)}  \right]$
    \end{minipage}
		 where $\hat \sigma_{(k)}$ is computed as
		\Statex \begin{minipage}{\linewidth}
    \centering
$\hat\sigma_{(k)} = \sqrt{\text{kth ordered value among} \ \left\{ \hat{\theta}^{2}_{1} + \sigma_1^2, \hat{\theta}^{2}_{2} + \sigma_2^2, \dots, \hat{\theta}^{2}_{K} + \sigma_K^2 \right\} - \hat {\theta}^{2}_{(k)}}$
\end{minipage}
	\end{algorithmic} 
\end{algorithm}



\begin{algorithm}[H]
	\caption{Computation of Coverage Probability for Parametric Bootstrap} 
	\label{alg:parametricbs_cov}
	For given values of $\theta_1, \theta_2, \dots, \theta_K$ and thus $\theta_{(1)}, \theta_{(2)}, \dots, \theta_{(K)}$
	\begin{algorithmic}[1] % Start algorithmic block
			\For {$\text{replications} = 1, 2, \dots, 5000$}
    		\State Generate $\hat\theta_k \sim N(\theta_k, \sigma^2_k)$, for $k = 1, 2, \dots, K$
    		\State Compute the rectangular confidence region $\mathfrak{R}$ using Algorithm \ref{alg:parametricbs_ci}.
    		\State Check if $\left( \theta_{(1)}, \theta_{(2)}, \dots, \theta_{(K)}\right) \in \mathfrak{R}$ and compute 
    		\Statex \begin{minipage}{\linewidth}
        \centering
    		$T_1 = \frac{1}{K} \sum^K_{k=1} \Big | \Lambda_{Ok} \Big|$\\
    		$T_2 = \prod^K_{k=1} \Big | \Lambda_{Ok} \Big|$\\
    		$T_3 = 1 - \frac{K + \sum^K_{k=1} \big | \Lambda_{Ok} \big|}{K^2}$\\
    		\end{minipage}
    	\EndFor
    \State Compute the proportion of times that the condition in step 4 is satisfied and the average of $T_1, T_2$, and $T_3$.
	\end{algorithmic} % End algorithmic block
\end{algorithm}


## Nonrank-based method

The nonrank-based method assumes that $\boldsymbol{\hat\theta} = \left(\hat\theta_1, \hat\theta_2, \dots, \hat\theta_K\right) \sim N \left( \boldsymbol{\theta}, \boldsymbol{\Sigma}\right)$. It accounts for potential correlation among items being ranked. For this case, an exchangeable correlation, $\boldsymbol{\rho}$ (See Equation \ref{eq:equicorrelation}.), is assumed and used in the calculation of the variance covariance matrix (See Equation \ref{eq:sigma_matrix}.).

\begin{equation}
  \boldsymbol{\rho} = \left( 1-\rho \right) \mathbf{I}_K + \rho \boldsymbol{1}_K \boldsymbol{1}'_K
  \label{eq:equicorrelation}
\end{equation}


\begin{equation}
  \boldsymbol{\Sigma} = \boldsymbol{\Delta}^{1/2} \boldsymbol{\rho} \boldsymbol{\Delta}^{1/2}
  \label{eq:sigma_matrix}
\end{equation}

where $\boldsymbol{\Delta} = \text{diag} \left\{ \sigma^2_1, \sigma^2_2, \dots, \sigma^2_K \right\}$, with known $\sigma_k$'s and $\rho$ is studied for $0.1, 0.5, 0.9$.

\begin{algorithm}[H]
	\caption{Computation of Joint Confidence Region via Nonrank-based Method} 
	\label{alg:nonrank_ci}
	Let the data consist of $\hat \theta_1, \dots, \hat \theta_K$ and suppose $\boldsymbol{\Sigma}$ is known
	\begin{algorithmic}[1]
		\For {$b = 1, 2, \dots, B$}
				\State Generate $\boldsymbol{\hat\theta}^*_b \sim N_K \left( \boldsymbol{\hat\theta}, \boldsymbol{\Sigma}\right)$ and write $\boldsymbol{\hat\theta}^*_b = \left( \hat\theta^*_{b1}, \hat\theta^*_{b2}, \dots, \hat\theta^*_{bK} \right)' $
				\State Compute $t^*_b = \underset{1 \leq j \leq K}{\max} \Bigg| \frac{\hat\theta^*_{bj} - \hat\theta^*_{j}}{\sigma_j} \Bigg|$
		\EndFor
		\State Compute the $\left(1-\alpha\right)$-sample quantile of $t^*_1, t^*_2, \dots, t^*_B$, call this $\hat{t}$.
		\State The joint confidence region of $\theta_1, \theta_2, \dots, \theta_K$ is given by 
		\Statex \begin{minipage}{\linewidth}
    \centering
$\mathfrak{R} = \left[ \hat\theta_1 \pm \hat t \times \sigma_1  \right] \times \left[ \hat\theta_2 \pm \hat t \times \sigma_2  \right] \times \dots \times \left[ \hat\theta_K \pm \hat t \times \sigma_K  \right]$
    \end{minipage}
	\end{algorithmic} 
\end{algorithm}



\begin{algorithm}[H]
	\caption{Computation of Coverage Probability for Nonrank-based Method} 
	\label{alg:nonrank_cov}
	For given values of $\theta_1, \theta_2, \dots, \theta_K$ and $\boldsymbol{\Sigma}$
	\begin{algorithmic}[1] % Start algorithmic block
			\For {$\text{replications} = 1, 2, \dots, 5000$}
    		\State Generate $\boldsymbol{\hat\theta} \sim N_K(\boldsymbol{\theta}, \boldsymbol{\Sigma})$
    		\State Compute the rectangular confidence region $\mathfrak{R}$ using Algorithm \ref{alg:nonrank_ci}.
    		\State Check if $\left( \theta_1, \theta_2, \dots, \theta_K\right) \in \mathfrak{R}$ and compute $T_1, T_2$, and $T_3$.
    	\EndFor
    \State Compute the proportion of times that the condition in step 4 is satisfied and the average of $T_1, T_2$, and $T_3$.
	\end{algorithmic} % End algorithmic block
\end{algorithm}

## Results





For the simulation studies, $\alpha$ is fixed at $0.1$, while the true standard deviations are varied ($sd = 2.0, 3.6, 6.0$) along with the number of items to be ranked ($K = 5, 10, 20, 30, 40, 51$). Table \@ref(tab:coverage) shows that when the correlation is zero, the nonrank-based, independent, and Bonferroni CI methodology exhibit similar coverage values regardless of $K$ and $sd$. In contrast, the parametric approach generally yields higher coverage for smaller $sd$ while showing comparable variability across different values of $K$.




\begin{longtable}[t]{rrrrrr}
\caption{(\#tab:coverage)Simulation results for coverage probabilities when correlation is zero.}\\
\toprule
\multicolumn{2}{c}{ } & \multicolumn{4}{c}{Coverage} \\
\cmidrule(l{3pt}r{3pt}){3-6}
K & sd & Parametric & Non-rankbased & Independent & Bonferroni\\
\midrule
\endfirsthead
\caption[]{(\#tab:coverage)Simulation results for coverage probabilities when correlation is zero. \textit{(continued)}}\\
\toprule
K & sd & Parametric & Non-rankbased & Independent & Bonferroni\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
 & 2.0 & 0.9198 & 0.8970 & 0.9000 & 0.9042\\
\nopagebreak
 & 3.6 & 0.8534 & 0.8970 & 0.9000 & 0.9042\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 5} & 6.0 & 0.8034 & 0.8970 & 0.9000 & 0.9042\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 0.8900 & 0.8962 & 0.8984 & 0.9034\\
\nopagebreak
 & 3.6 & 0.8830 & 0.8962 & 0.8984 & 0.9034\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 10} & 6.0 & 0.8212 & 0.8962 & 0.8984 & 0.9034\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 0.8842 & 0.9070 & 0.9096 & 0.9134\\
\nopagebreak
 & 3.6 & 0.8952 & 0.9070 & 0.9096 & 0.9134\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 20} & 6.0 & 0.8264 & 0.9070 & 0.9096 & 0.9134\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 0.8786 & 0.9026 & 0.9048 & 0.9088\\
\nopagebreak
 & 3.6 & 0.8594 & 0.9026 & 0.9048 & 0.9088\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 30} & 6.0 & 0.8458 & 0.9026 & 0.9048 & 0.9088\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 0.8820 & 0.8958 & 0.8972 & 0.9020\\
\nopagebreak
 & 3.6 & 0.8804 & 0.8958 & 0.8972 & 0.9020\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 40} & 6.0 & 0.8474 & 0.8958 & 0.8972 & 0.9020\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 0.9442 & 0.9010 & 0.9008 & 0.9054\\
\nopagebreak
 & 3.6 & 0.9128 & 0.9010 & 0.9008 & 0.9054\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 51} & 6.0 & 0.9124 & 0.9010 & 0.9008 & 0.9054\\*
\end{longtable}


The case is different in terms of $T_1$ (See Table \@ref(tab:T1).) as it increases with decreasing $sd$ and increasing $K$. The CIs are wider for the parametric approach compared to the remaining approaches whose $T_1$ only vary by a small margin, with nonrank-based method having the smallest $T_1$ and Bonferroni, the largest one. The same behavior is observed for $T_2$ and $T_3$.



\begin{longtable}[t]{rrrrrr}
\caption{(\#tab:T1)Simulation results for coverage probabilities.}\\
\toprule
\multicolumn{2}{c}{ } & \multicolumn{4}{c}{$T_1$} \\
\cmidrule(l{3pt}r{3pt}){3-6}
K & sd & Parametric & Non-rankbased & Independent & Bonferroni\\
\midrule
\endfirsthead
\caption[]{(\#tab:T1)Simulation results for coverage probabilities. \textit{(continued)}}\\
\toprule
K & sd & Parametric & Non-rankbased & Independent & Bonferroni\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
 & 2.0 & 2.212960 & 2.127360 & 2.128560 & 2.130800\\
\nopagebreak
 & 3.6 & 1.983280 & 1.834720 & 1.835600 & 1.842240\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 5} & 6.0 & 1.622000 & 1.462560 & 1.462560 & 1.465520\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 4.002280 & 3.243800 & 3.246320 & 3.259040\\
\nopagebreak
 & 3.6 & 2.710680 & 2.307480 & 2.308680 & 2.317680\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 10} & 6.0 & 1.924080 & 1.731200 & 1.733320 & 1.736840\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 7.022060 & 5.333380 & 5.336680 & 5.361700\\
\nopagebreak
 & 3.6 & 4.207100 & 3.448120 & 3.451680 & 3.465240\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 20} & 6.0 & 2.787040 & 2.453000 & 2.454920 & 2.462440\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 11.830987 & 9.538733 & 9.547507 & 9.591387\\
\nopagebreak
 & 3.6 & 7.653613 & 5.802093 & 5.806080 & 5.833933\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 30} & 6.0 & 4.764387 & 3.778320 & 3.781573 & 3.797987\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 15.153140 & 12.100160 & 12.108990 & 12.160730\\
\nopagebreak
 & 3.6 & 10.392030 & 7.201310 & 7.205910 & 7.241320\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 40} & 6.0 & 6.508180 & 4.483520 & 4.485920 & 4.506070\\
\cmidrule{1-6}\pagebreak[0]
 & 2.0 & 20.446533 & 15.607796 & 15.614745 & 15.685914\\
\nopagebreak
 & 3.6 & 13.206918 & 9.098996 & 9.103490 & 9.143977\\
\nopagebreak
\multirow{-3}{*}{\raggedleft\arraybackslash 51} & 6.0 & 8.598722 & 5.786298 & 5.789153 & 5.814400\\*
\end{longtable}



As the correlation increases, the coverage of both independent and Bonferroni CIs exceed the nominal value while that of nonrank-based method remains close to it. This holds regardless of $K$ and $sd$. See Table \@ref(tab:nonzerocorrel).


\begin{longtable}[t]{rrrrr}
\caption{(\#tab:nonzerocorrel)Simulation results for coverage probabilities when correlation is nonzero.}\\
\toprule
\multicolumn{2}{c}{ } & \multicolumn{3}{c}{Coverage} \\
\cmidrule(l{3pt}r{3pt}){3-5}
corr & K & Non-rankbased & Independent & Bonferroni\\
\midrule
\endfirsthead
\caption[]{(\#tab:nonzerocorrel)Simulation results for coverage probabilities when correlation is nonzero. \textit{(continued)}}\\
\toprule
corr & K & Non-rankbased & Independent & Bonferroni\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
 & 5 & 0.8984 & 0.9008 & 0.9046\\
\nopagebreak
 & 10 & 0.8996 & 0.9016 & 0.9060\\
\nopagebreak
 & 20 & 0.8988 & 0.9026 & 0.9082\\
\nopagebreak
 & 30 & 0.9000 & 0.9036 & 0.9088\\
\nopagebreak
 & 40 & 0.8916 & 0.8968 & 0.9012\\
\nopagebreak
\multirow{-6}{*}{\raggedleft\arraybackslash 0.1} & 51 & 0.8944 & 0.8994 & 0.9048\\
\cmidrule{1-5}\pagebreak[0]
 & 5 & 0.9042 & 0.9218 & 0.9260\\
\nopagebreak
 & 10 & 0.9038 & 0.9322 & 0.9346\\
\nopagebreak
 & 20 & 0.9032 & 0.9378 & 0.9408\\
\nopagebreak
 & 30 & 0.8910 & 0.9312 & 0.9338\\
\nopagebreak
 & 40 & 0.8920 & 0.9328 & 0.9358\\
\nopagebreak
\multirow{-6}{*}{\raggedleft\arraybackslash 0.5} & 51 & 0.9086 & 0.9492 & 0.9516\\
\cmidrule{1-5}\pagebreak[0]
 & 5 & 0.9032 & 0.9574 & 0.9586\\
\nopagebreak
 & 10 & 0.8962 & 0.9682 & 0.9690\\
\nopagebreak
 & 20 & 0.8980 & 0.9758 & 0.9766\\
\nopagebreak
 & 30 & 0.8960 & 0.9802 & 0.9806\\
\nopagebreak
 & 40 & 0.8996 & 0.9862 & 0.9870\\
\nopagebreak
\multirow{-6}{*}{\raggedleft\arraybackslash 0.9} & 51 & 0.8928 & 0.9866 & 0.9868\\*
\end{longtable}




# Introduction




## Background of the Study

## Statement of the Problem

## Objective of the Study

## Study Hypothesis

## Significance of the Study

## Scope and Limitation

## Definition of Terms


# Background


THIS IS @mariarizzo and @klein

# References {-}
<div id="refs"></div>

# Appendices {-}


## Codes for algorithm 1 {-}

```r
 get_independent_ci <- function(theta_hat,
                               S,
                               alpha){
  K <- length(theta_hat)
  gamma = 1-(1-alpha)^(1/K)
  z = qnorm(1-gamma/2)
  ci_lower <- theta_hat - z*S
  ci_upper <- theta_hat + z*S
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_bonferroni_ci <- function(theta_hat,
                              S,
                              alpha){
  K <- length(theta_hat)
  z = qnorm(1-(alpha/K)/2)
  ci_lower <- theta_hat - z*S
  ci_upper <- theta_hat + z*S
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_parametric_ci <- function(B,
                              theta_hat,
                              S,
                              alpha) {
  K <- length(theta_hat)
  # step 1a ====================================
  thetahat_star <- sapply(seq_len(K), function(i) {
    rnorm(B, mean = theta_hat[i], sd = S[i])
  })
  colnames(thetahat_star) <- paste0("thetahat_star", 
                                    sprintf("%02d", 1:K))
  sorted_thetahat_star <- t(apply(thetahat_star, 1, sort))
  colnames(sorted_thetahat_star) <- paste0("sorted_thetahat_star", 
                                           sprintf("%02d", 1:K))
  # step 1b ====================================
  variance_vector <- S^2
  minuend <- thetahat_star^2 + rep(
    variance_vector, each = nrow(thetahat_star))
  sigma_hat_star <- sqrt(
    t(apply(minuend, 1, sort)) - sorted_thetahat_star^2)
  # step 1c ====================================
  sorted_theta_hat <- sort(theta_hat)
  t_star <- apply(
    abs(
      (
        sorted_thetahat_star - rep(
          sorted_theta_hat,
          each = nrow(sorted_thetahat_star)
          )
        )/sigma_hat_star
      ),
    1, 
    max)
  # step 2 =====================================
  t_hat <- quantile(t_star, probs = 1 - alpha)
  # step 3 =====================================
  sigma_hat <- sqrt(
    sort(theta_hat^2 + variance_vector) - sorted_theta_hat^2)
  # step 6 =====================================
  ci_lower <- sorted_theta_hat - t_hat*sigma_hat
  ci_upper <- sorted_theta_hat + t_hat*sigma_hat
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

get_nonrankbased_ci <- function(B, 
                                theta_hat,
                                alpha, 
                                varcovar_matrix) {
  K <- length(theta_hat)
  # step 1a ===================================
  generate_data <- function(){MASS::mvrnorm(n = 1,
                                      mu = theta_hat,
                                      Sigma = varcovar_matrix)}
  thetahat_star <- t(replicate(B, generate_data()))
  # step 1b ===================================
  t_star <- apply(thetahat_star, 
                  1, 
                  function(x) max(abs((x - theta_hat) / sqrt(
                    diag(varcovar_matrix)))))  
  # step 2 ====================================
  t_hat <- quantile(t_star, probs = 1 - alpha)
  # step 3 ====================================
  ci_lower <- theta_hat - t_hat*sqrt(diag(varcovar_matrix))
  ci_upper <- theta_hat + t_hat*sqrt(diag(varcovar_matrix))
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
} 
```

## Codes for algorithm 2 {-}

```r
 source("../../R/compute_ci.R")
library("doRNG")

get_ranks <- function(k, tuple_list){
  Lambda_lk <- which(
    tuple_list[,2]<=tuple_list[k,1])
  Lambda_lk <- Lambda_lk[Lambda_lk != k]
  Lambda_Ok <- which(
    tuple_list[,2]>tuple_list[k,1] & tuple_list[k,2] > tuple_list[,1])
  Lambda_Ok <- Lambda_Ok[Lambda_Ok != k]
  ranks <- seq(
    length(unique(Lambda_lk)) + 1,
    length(unique(Lambda_lk)) + length(unique(Lambda_Ok)) + 1,
    1
  )
  return(list(
    ranks = ranks,
    Lambda_Ok = Lambda_Ok
  ))
}

get_t1 <- function(v) mean(v)

get_t2 <- function(v) prod(v)^(1/length(v))

get_t3 <- function(v) {
  1 - ((length(v)+sum(v))/(length(v)^2))
}

get_coverage <- function(ci_lower,
                         ci_upper,
                         true_theta) {
  return(all(ci_lower<=true_theta) & all(true_theta<=ci_upper))
}

algo2_nonrankbased <- function(
    true_theta,
    K, 
    reps = 5, # step 4
    B=100, 
    alpha= 0.10,
    varcovar_matrix){
  foreach(iter = 1:reps, 
          .combine = rbind,
          .packages = c("foreach", "arrow", "MASS"),
          .export = c("get_nonrankbased_ci", "get_independent_ci",
                      "get_bonferroni_ci", "get_ranks", "get_coverage",
                      "get_t1", "get_t2", "get_t3")
  ) %dorng% {
    
    # step 1 =======
    theta_hat <- mvrnorm(n = 1, 
                         mu = true_theta, 
                         Sigma = varcovar_matrix)
    
    # step 2 =======
    S <- sqrt(diag(varcovar_matrix))
    
    ci_methods <- list(
      nonrankbased = function() get_nonrankbased_ci(B, theta_hat, alpha, 
                                                    varcovar_matrix),
      independent  = function() get_independent_ci(theta_hat, S, alpha),
      bonferroni   = function() get_bonferroni_ci(theta_hat, S, alpha)
    )
    
    ci_results <- lapply(ci_methods, function(f) f())
    
    coverages <- lapply(ci_results, function(res) {
      get_coverage(
        ci_lower   = res$ci_lower,
        ci_upper   = res$ci_upper,
        true_theta = true_theta
      )
    })
    
    process_ci_result <- function(result, K) {
      tuple_list <- t(apply(
        data.frame(
          ci_lower = result$ci_lower,
          ci_upper = result$ci_upper
        ), 
        1, 
        function(row) as.numeric(row)
      ))
      
      rank_range_length <- sapply(1:K, function(x) 
        length(get_ranks(x, tuple_list)$ranks)
      )
      
      list(
        t1 = get_t1(rank_range_length),
        t2 = get_t2(rank_range_length),
        t3 = get_t3(rank_range_length)
      )
    }
    
    processed <- lapply(ci_results, process_ci_result, K = K)

    data.frame(
      t1_nonrankbased = processed$nonrankbased$t1,
      t2_nonrankbased = processed$nonrankbased$t2,
      t3_nonrankbased = processed$nonrankbased$t3,
      coverage_nonrankbased = coverages$nonrankbased,
      t1_independent = processed$independent$t1,
      t2_independent = processed$independent$t2,
      t3_independent = processed$independent$t3,
      coverage_independent = coverages$independent,
      t1_bonferroni = processed$bonferroni$t1,
      t2_bonferroni = processed$bonferroni$t2,
      t3_bonferroni = processed$bonferroni$t3,
      coverage_bonferroni = coverages$bonferroni
      )
  }
}


algo2_parametric <- function(
    true_theta,
    K, 
    reps = 5, # step 4
    B=100, 
    alpha= 0.10,
    S){
  foreach(iter = 1:reps, 
          .combine = rbind,
          .packages = c("foreach", "arrow", "MASS"),
          .export = c("get_parametric_ci","get_ranks", "get_coverage",
                      "get_t1", "get_t2", "get_t3")
  ) %dorng% {
    
    # step 1 =======
    theta_hat <- rnorm(
      n    = K,
      mean = true_theta,
      sd   = S
    )
    
    # step 2 =======
    result <- get_parametric_ci(B,
                                theta_hat,
                                S,
                                alpha)
    
    # step 3 =======
    sorted_true_theta <- sort(true_theta)
    coverage <- get_coverage(ci_lower = result$ci_lower,
                             ci_upper = result$ci_upper,
                             true_theta = sorted_true_theta)
    
    tuple_list <- t(apply(
      data.frame(ci_lower = result$ci_lower,
                 ci_upper = result$ci_upper), 1, function(row) as.numeric(row)))
    rank_range_length <- sapply(1:K, function(x) length(
      get_ranks(x, tuple_list)$ranks))
    t1 <- get_t1(rank_range_length)
    t2 <- get_t2(rank_range_length)
    t3 <- get_t3(rank_range_length)
    
    data.frame(
      t1_parametric = t1,
      t2_parametric = t2,
      t3_parametric = t3,
      coverage_parametric = coverage
    )
  }
}
 
```


## Codes for simulation {-}

```r
 #3:37PM
source("../../R/implement_algo2.R")

mean <- 23.8
df <- readRDS("../../data/mean_travel_time_ranking_2011.rds")
cl=parallel::makeCluster(15)
registerDoParallel(cl)

sds <- c(2, 3.6, 6)
Ks <- c(51, 40, 30, 20, 10, 5)
corrs <- c(0.1,0.5,0.9)
alphas <- c(0.1)#c(0.05, 0.1, 0.15, 0.2)

for (sd in sds) {
  for (K in Ks) {
    set.seed(123974)
    true_theta <- rnorm(K, mean, sd)
    true_sds <- df$S[1:K]
    
    for (alpha in alphas) {
      
      tic("Running parametric...")
      coverage_parametric_df <- algo2_parametric(true_theta,
                                                 K, 
                                                 reps = 5000,
                                                 B=500, 
                                                 alpha= alpha,
                                                 S=true_sds)
      toc()
      saveRDS(coverage_parametric_df,  paste0("output/coverage_parametric_",
                                              K,"_", sd, "_", alpha, ".rds"))
      for (corr in corrs) {
        corr_matrix <- (1 - corr) * diag(K) + corr * matrix(1, K, K)
        variance_vector <- true_sds^2
        delta <- diag(variance_vector)
        varcovar_matrix <- delta^(1/2) %*% corr_matrix %*% delta^(1/2)
        
        tic("Running nonrankbased...")
        coverage_output_df <- algo2_nonrankbased(
          true_theta,
          K, 
          reps = 5000, 
          B = 500, 
          alpha=alpha,
          varcovar_matrix = varcovar_matrix)
        toc()
        
        saveRDS(coverage_output_df,  paste0("output/coverage_probability_",
                                            K,"_", sd, "_", corr, "_", 
                                            alpha, ".rds"))
      }
    }
  }
}

stopCluster(cl)

param_grid <- expand.grid(K = Ks, sd = sds, corr = corrs, alpha = alphas)

results <- do.call(rbind, lapply(seq_len(nrow(param_grid)), function(i) {
  K <- param_grid$K[i]
  sd <- param_grid$sd[i]
  corr <- param_grid$corr[i]
  alpha <- param_grid$alpha[i]
  
  a <- readRDS(paste0("output/coverage_probability_", 
                      K, "_", sd, "_", corr, "_", alpha, ".rds"))
  
  data.frame(
    K = K,sd = sd,corr = corr,alpha = alpha,
    Cov_nonrankbased = mean(a$coverage_nonrankbased),
    Cov_independent = mean(a$coverage_independent),
    Cov_bonferroni = mean(a$coverage_bonferroni),
    T1_nonrankbased = mean(a$t1_nonrankbased),
    T1_independent = mean(a$t1_independent),
    T1_bonferroni = mean(a$t1_bonferroni),
    T2_nonrank = mean(a$t2_nonrankbased),
    T2_independent = mean(a$t2_independent),
    T2_bonferroni = mean(a$t2_bonferroni),
    T3_independent = mean(a$t3_independent),
    T3_nonrankbased = mean(a$t3_nonrankbased),
    T3_bonferroni = mean(a$t3_bonferroni)
  )
}))

param_grid <- expand.grid(K = Ks, sd = sds, alpha = alphas)

results1 <- do.call(rbind, lapply(seq_len(nrow(param_grid)), function(i) {
  K <- param_grid$K[i]
  sd <- param_grid$sd[i]
  alpha <- param_grid$alpha[i]
  
  a <- readRDS(paste0("output/coverage_parametric_", 
                      K, "_", sd, "_", alpha, ".rds"))
  
  data.frame(
    K = K,
    sd = sd,
    alpha = alpha,
    Cov_parametric = mean(a$coverage_parametric),
    T1_parametric = mean(a$t1_parametric),
    T2_parametric = mean(a$t2_parametric),
    T3_parametric = mean(a$t3_parametric)
  )
}))

save(results, results1, file = "simulation_results.RData") 
```


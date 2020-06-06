# Officer bias in stop and search is exacerbated by deployment decisions

This Github repo contains the code used to produce the analysis for our paper ``Officer bias in stop and search is exacerbated by deployment decisions'' (currently R&R at Nature Human Behaviour). 

The 'run.R' file runs all code in the correct order and reproduces the results.

This research is based on data resources provided by West Midlands Police. Data were originally collected as part of routine police record keeping. The data are not available publicly and were provided to us under an Information Sharing Agreement with West Midlands Police. 

Since the original data from West Midlands Police may not be shared publicly, we generate synthetic data to demonstrate our code in the file 'generate_synthetic_data.R' (called by 'run.R'). The distributions of the variables in the synthetic data match the sample distributions of the original data. 

Please let me know if you find any errors!

```R
sessionInfo()
```

```R
R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252    LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] xtable_1.8-4         egg_0.4.5            gridExtra_2.3        forcats_0.4.0        skimr_2.1.1          scales_1.1.0        
 [7] tictoc_1.0           magrittr_1.5         tidybayes_2.0.1      ggridges_0.5.2       bayesplot_1.7.1      rstan_2.19.3        
[13] ggplot2_3.3.0        StanHeaders_2.21.0-1 purrr_0.3.3          data.table_1.12.8   

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3         lattice_0.20-38    tidyr_1.0.2        prettyunits_1.1.1  ps_1.3.2           utf8_1.1.4         assertthat_0.2.1  
 [8] packrat_0.5.0      digest_0.6.25      R6_2.4.1           plyr_1.8.5         repr_1.1.0         stats4_3.6.3       coda_0.19-3       
[15] pillar_1.4.3       rlang_0.4.4        rstudioapi_0.11    praise_1.0.0       callr_3.4.2        labeling_0.3       stringr_1.4.0     
[22] loo_2.2.0          munsell_0.5.0      compiler_3.6.3     xfun_0.12          pkgconfig_2.0.3    base64enc_0.1-3    pkgbuild_1.0.6    
[29] htmltools_0.4.0    tidyselect_1.0.0   tibble_2.1.3       arrayhelpers_1.1-0 codetools_0.2-16   matrixStats_0.55.0 fansi_0.4.1       
[36] crayon_1.3.4       dplyr_0.8.4        withr_2.1.2        grid_3.6.3         jsonlite_1.6.1     gtable_0.3.0       lifecycle_0.1.0   
[43] cli_2.0.2          stringi_1.4.6      reshape2_1.4.3     farver_2.0.3       ellipsis_0.3.0     vctrs_0.2.3        tools_3.6.3       
[50] svUnit_0.7-12      glue_1.3.1         processx_3.4.2     parallel_3.6.3     inline_0.3.15      colorspace_1.4-1   knitr_1.28
```
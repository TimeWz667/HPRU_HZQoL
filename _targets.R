library(targets)

# library(tarchetypes) # Load other packages as needed.
library(rstan)
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmeresampler)

tar_option_set(
  packages = c("tibble", "tidyverse", "tidybayes", "rstan", "readxl", "glue", "lme4")
)

tar_source()

dir.create("pars/", showWarnings = F)
dir.create("posteriors/", showWarnings = F)


options(dplyr.summarise.inform = FALSE)
options(mc.cores = 8)
rstan_options(auto_write = TRUE)
theme_set(theme_bw())


# preprocessing with confidential data
# source("rev_convert.R")


list(
    tar_target(file_norm, here::here("data", "external", "qale_shortfall.csv"), format = "file"),
    
    tar_target(vset, c("uk", "orig")),
    tar_target(file_qol, here::here("data", "processed", paste0("eq5d_", vset, ".csv")), format = "file", pattern = map(vset)),

    ## extraction
    tar_target(data_raw, get_data_qol(file_qol, vset), pattern = map(file_qol, vset)),
    tar_target(data_norm, get_data_norm(file_norm)),

    tar_target(data_tte, format_tte(data_raw), pattern = slice(data_raw, 1)),
    tar_target(data_qol, format_qol(data_raw), pattern = map(data_raw)),
    ## split training sets

    # tar_target(subdata_tte, split_training_tte(data_tte, prop = 0.5, prop_training = 0.5, seed = 11667 + 100)),
    # tar_target(subdata_qol, split_training_qol(data_qol, prop = 0.5, prop_training = 0.5, seed = 11667 + 200)),

    # modelling, TTE
    tar_target(file_model_tte, here::here("models", "time2zero_surv_age.stan"), format = "file"),
    tar_target(model_tte, stan_model(file_model_tte)),
    tar_target(pars_tte, fit_tte(model_tte, data_tte)),
    tar_target(file_pars_tte, summarise_tte(pars_tte), format = "file"),
    tar_target(plot_tte, visualise_tte(data_tte, pars_tte)),

    ## modelling, QoL, Freq
    # tar_target(pars_qol_k, fit_qol_kmeans(data_qol), pattern = map(data_qol)),
    # tar_target(file_pars_qol_k, summarise_qol_kmeans(pars_qol_k, vset), pattern = map(pars_qol_k, vset), format = "file"),
    # tar_target(plot_qol, visualise_qol_k(data_qol, pars_qol_k, vset), pattern = map(data_qol, pars_qol_k, vset)),

    tar_target(pars_qol_f, fit_qol_freq(data_qol), pattern = map(data_qol)),
    tar_target(file_pars_qol_f, summarise_qol_freq(pars_qol_f, vset), pattern = map(pars_qol_f, vset), format = "file"),
    tar_target(plot_qol_f, visualise_qol_freq(data_qol, pars_qol_f, vset, drf = F), pattern = map(data_qol, pars_qol_f, vset)),
    tar_target(plot_qol_f_apx, visualise_qol_freq(data_qol, pars_qol_f, vset, drf = T), pattern = map(data_qol, pars_qol_f, vset)),

    ## modelling, QoL, Bayes
    tar_target(pars_qol_b, fit_qol_bayes(data_qol), pattern = map(data_qol)),
    tar_target(file_pars_qol_b, summarise_qol_bayes(pars_qol_b, vset), pattern = map(pars_qol_b, vset), format = "file"),
    
    ## simulation, Freq
    tar_target(pars_shortfall_f, boot_pars(file_pars_tte, file_pars_qol_f, n_sim = 1000), pattern = map(file_pars_qol_f)),
    # tar_target(js_shortfall, output_pars_shortfall(pars_shortfall, data_norm, vset), pattern = map(pars_shortfall, vset), format = "file"),  
    # tar_target(pars_shortfall_f, reload_pars(vset), pattern = map(vset)),
    tar_target(sim_shortfall_f, simulate_shortfall(pars_shortfall_f, data_norm, vset, mod = "f"), pattern = map(pars_shortfall_f, vset)),
    tar_target(tab_shortfall_f, summarise_shortfall(sim_shortfall_f, paste0("f_", vset)), pattern = map(sim_shortfall_f, vset)),
    tar_target(plot_shortfall_f, vis_shortfall(sim_shortfall_f, tab_shortfall_f, paste0("f_", vset)), pattern = map(sim_shortfall_f, tab_shortfall_f, vset)),
    
    ## simulation, Bayes
    tar_target(pars_shortfall_b, boot_pars_bayes(file_pars_tte, file_pars_qol_b, n_sim = 1000), pattern = map(file_pars_qol_b)),
    tar_target(sim_shortfall_b, simulate_shortfall(pars_shortfall_b, data_norm, vset, mod = "b"), pattern = map(pars_shortfall_b, vset)),
    tar_target(tab_shortfall_b, summarise_shortfall(sim_shortfall_b, vset), pattern = map(sim_shortfall_b, vset)),
    tar_target(plot_shortfall_b, vis_shortfall(sim_shortfall_b, tab_shortfall_b, vset), pattern = map(sim_shortfall_b, tab_shortfall_b, vset)),
    # tar_target(plot_qol_t, vis_qol_t(pars_shortfall, data_norm, vset, age = 80), pattern = map(pars_shortfall, vset)),    
    

    # tar_target(sim_shortfall, simulate_shortfall(pars_shortfall, data_norm, vset, mod = "f"), pattern = map(pars_shortfall, vset)),
    
    # Meta regression
    
    
    # 
    tar_target(stats, describe_basic(data_raw, data_tte, pars_qol_f, vset), pattern = map(data_raw, pars_qol_f, vset))

)


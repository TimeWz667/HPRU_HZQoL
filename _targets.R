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
  tar_target(vset, c("orig", "uk")),
  tar_target(file_qol, here::here("data", "processed", paste0("eq5d_", vset, ".csv")), format = "file", pattern = map(vset)),
  tar_target(file_qol_baseline, here::here("data", "processed", paste0("eq5d_baseline_", vset, ".csv")), format = "file", pattern = map(vset)),
  tar_target(file_norm, here::here("data", "processed", "population_norm_mapped.csv"), format = "file"),
  
  ## extraction
  tar_target(data_norm, read_csv(file_norm)),
  
  ### Perfect health baseline
  tar_target(data_raw, get_data_qol(file_qol, vset), pattern = map(file_qol, vset)),
  tar_target(data_tte, format_tte(data_raw), pattern = slice(data_raw, 1)),
  tar_target(data_qol, format_qol(data_raw), pattern = map(data_raw)),
 
  ### Population norm baseline
  tar_target(data_raw_pn, get_data_pn(data_raw, data_norm), pattern = map(data_raw)),
  tar_target(data_tte_pn, format_tte(data_raw_pn), pattern = slice(data_raw_pn, 1)),
  tar_target(data_qol_pn, format_qol(data_raw_pn), pattern = map(data_raw_pn)),
  
  ### Pre-HZ baseline
  tar_target(data_baseline, read_csv(file_qol_baseline), pattern = map(data_raw, file_qol_baseline)),
  tar_target(data_raw_0, get_data_qol_shift(data_raw, data_baseline), pattern = map(data_raw, data_baseline)),
  tar_target(data_tte_0, format_tte(data_raw_0), pattern = slice(data_raw_0, 1)),
  tar_target(data_qol_0, format_qol(data_raw_0), pattern = map(data_raw_0)),
  
  ## split training sets for exploratory analyses
  tar_target(subdata_tte, split_training_tte(data_tte, prop = 0.5, prop_training = 0.5, seed = 11667 + 100)),
  tar_target(subdata_qol, split_training_qol(data_qol, prop = 0.5, prop_training = 0.5, seed = 11667 + 200)),
  
  ## compile stan models

  # modelling, TTE
  tar_target(file_model_tte, here::here("models", "time2zero_surv_age.stan"), format = "file"),
  tar_target(model_tte, stan_model(file_model_tte)),
  
  tar_target(pars_tte, fit_tte(model_tte, data_tte)),
  tar_target(tabs_tte, restructure_stan(pars_tte)),
  tar_target(gs_tte, visualise_tte(data_tte, tabs_tte)),
  tar_target(file_posterior_tte, output_posterior(tabs_tte, "ph_tte"), format = "file"),
  tar_target(file_vis_tte, output_vis_tte(gs_tte, folder = here::here("docs", "figs"), "ph")),
  
  tar_target(pars_tte_pn, fit_tte(model_tte, data_tte_pn)),
  tar_target(tabs_tte_pn, restructure_stan(pars_tte_pn)),
  tar_target(gs_tte_pn, visualise_tte(data_tte_pn, tabs_tte_pn)),
  tar_target(file_posterior_tte_pn, output_posterior(tabs_tte_pn, "pn_tte"), format = "file"),
  tar_target(file_vis_tte_pn, output_vis_tte(gs_tte_pn, folder = here::here("docs", "figs"), "pn")),
  
  tar_target(pars_tte_0, fit_tte(model_tte, data_tte_0)),
  tar_target(tabs_tte_0, restructure_stan(pars_tte_0)),
  tar_target(gs_tte_0, visualise_tte(data_tte_0, tabs_tte_0)),
  tar_target(file_posterior_tte_0, output_posterior(tabs_tte_0, "base_tte"), format = "file"),
  tar_target(file_vis_tte_0, output_vis_tte(gs_tte_0, folder = here::here("docs", "figs"), "base")),
  tar_target(tabs_tte_compare, vis_tte_comparing(tabs_tte, tabs_tte_pn, tabs_tte_0, folder = here::here("docs", "figs"), ext = ".png", scale = 1))

)
# 
# list(
#     tar_target(file_norm, here::here("data", "external", "qale_shortfall.csv"), format = "file"),
#     
#     tar_target(vset, c("uk", "orig")),
#     tar_target(file_qol, here::here("data", "processed", paste0("eq5d_", vset, ".csv")), format = "file", pattern = map(vset)),
# 
#     ## extraction
#     tar_target(data_raw, get_data_qol(file_qol, vset), pattern = map(file_qol, vset)),
#     tar_target(data_norm, get_data_norm(file_norm)),
# 
#     tar_target(data_tte, format_tte(data_raw), pattern = slice(data_raw, 1)),
#     tar_target(data_qol, format_qol(data_raw), pattern = map(data_raw)),
#     ## split training sets
# 
#     # tar_target(subdata_tte, split_training_tte(data_tte, prop = 0.5, prop_training = 0.5, seed = 11667 + 100)),
#     # tar_target(subdata_qol, split_training_qol(data_qol, prop = 0.5, prop_training = 0.5, seed = 11667 + 200)),
# 
#     # modelling, TTE
#     tar_target(file_model_tte, here::here("models", "time2zero_surv_age.stan"), format = "file"),
#     tar_target(model_tte, stan_model(file_model_tte)),
#     tar_target(pars_tte, fit_tte(model_tte, data_tte)),
#     tar_target(file_pars_tte, summarise_tte(pars_tte), format = "file"),
#     tar_target(plot_tte, visualise_tte(data_tte, pars_tte)),
# 
#     ## modelling, QoL, Freq
#     tar_target(pars_qol_f, fit_qol_freq(data_qol), pattern = map(data_qol)),
#     tar_target(file_pars_qol_f, summarise_qol_freq(pars_qol_f, vset), pattern = map(pars_qol_f, vset), format = "file"),
#     tar_target(plot_qol_f, visualise_qol_freq(data_qol, pars_qol_f, vset, drf = F), pattern = map(data_qol, pars_qol_f, vset)),
#     tar_target(plot_qol_f_apx, visualise_qol_freq(data_qol, pars_qol_f, vset, drf = T), pattern = map(data_qol, pars_qol_f, vset)),
# 
#     ## simulation, Freq
#     # tar_target(pars_shortfall_f, boot_pars(file_pars_tte, file_pars_qol_f, n_sim = 1000), pattern = map(file_pars_qol_f)),
#     # tar_target(sim_shortfall_f, simulate_shortfall(pars_shortfall_f, data_norm, vset, mod = "f"), pattern = map(pars_shortfall_f, vset)),
#     # tar_target(tab_shortfall_f, summarise_shortfall(sim_shortfall_f, paste0("f_", vset)), pattern = map(sim_shortfall_f, vset)),
#     # tar_target(plot_shortfall_f, vis_shortfall(sim_shortfall_f, tab_shortfall_f, paste0("f_", vset)), pattern = map(sim_shortfall_f, tab_shortfall_f, vset)),
#     
#     
#     ## modelling, QoL, Bayes
#     tar_target(pars_qol_b, fit_qol_bayes(data_qol), pattern = map(data_qol)),
#     tar_target(file_pars_qol_b, summarise_qol_bayes(pars_qol_b, vset), pattern = map(pars_qol_b, vset), format = "file"),
#     tar_target(plot_qol_b, visualise_qol_bayes(data_qol, pars_qol_f, pars_qol_b, vset, drf = T), pattern = map(data_qol, pars_qol_b, pars_qol_f, vset)),
#     
#     ## simulation, Bayes
#     # tar_target(pars_shortfall_b, boot_pars_bayes(file_pars_tte, file_pars_qol_b, n_sim = 1000), pattern = map(file_pars_qol_b)),
#     # tar_target(sim_shortfall_b, simulate_shortfall(pars_shortfall_b, data_norm, vset, mod = "b"), pattern = map(pars_shortfall_b, vset)),
#     # tar_target(tab_shortfall_b, summarise_shortfall(sim_shortfall_b, vset), pattern = map(sim_shortfall_b, vset)),
#     # tar_target(plot_shortfall_b, vis_shortfall(sim_shortfall_b, tab_shortfall_b, vset), pattern = map(sim_shortfall_b, tab_shortfall_b, vset)),
#     # tar_target(plot_qol_t, vis_qol_t(pars_shortfall_b, data_norm, vset, age = 80), pattern = map(pars_shortfall_b, vset)),
#     # 
#     # 
#     # investigate response shift
#     tar_target(file_qol_baseline, here::here("data", "processed", paste0("eq5d_baseline_", vset, ".csv")), format = "file", pattern = map(vset)),
#     tar_target(data_raw0, get_data_qol_shift(data_raw, file_qol_baseline, vset), pattern = map(data_raw, file_qol_baseline, vset)),
#     tar_target(data_tte0, format_tte(data_raw0), pattern = slice(data_raw0, 1)),
#     tar_target(pars_tte0, fit_tte(model_tte, data_tte0)),
#     tar_target(plot_tte0, visualise_tte(data_tte0, pars_tte0, "_baseline")),
#     tar_target(file_pars_tte0, summarise_tte(pars_tte0), format = "file"),
#     
#     tar_target(data_qol0, format_qol(data_raw0), pattern = map(data_raw0)),
#     tar_target(pars_qol_f0, fit_qol_freq(data_qol0), pattern = map(data_qol0)),
#     tar_target(file_pars_qol_f0, summarise_qol_freq(pars_qol_f0, vset), pattern = map(pars_qol_f0, vset), format = "file"),
#     
#     tar_target(pars_shortfall_f0, boot_pars(file_pars_tte0, file_pars_qol_f0, n_sim = 1000), pattern = map(file_pars_qol_f0)),
#     tar_target(sim_shortfall_f0, simulate_shortfall(pars_shortfall_f0, data_norm, vset, mod = "f"), pattern = map(pars_shortfall_f0, vset)),
#     tar_target(tab_shortfall_f0, summarise_shortfall(sim_shortfall_f0, paste0("f0_", vset)), pattern = map(sim_shortfall_f0, vset)),
#     tar_target(plot_shortfall_f0, vis_shortfall(sim_shortfall_f0, tab_shortfall_f0, glue::as_glue("baseline_") + vset), pattern = map(sim_shortfall_f0, tab_shortfall_f0, vset)),
# 
#     tar_target(tab_diff_tte, desc_disuti(pars_tte, pars_tte0)),
#     
#     # 
#     tar_target(stats, describe_basic(data_raw, data_tte, pars_qol_f, vset), pattern = map(data_raw, pars_qol_f, vset))
# 
# )


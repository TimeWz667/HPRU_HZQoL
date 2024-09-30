library(targets)

# library(tarchetypes) # Load other packages as needed.
library(rstan)
library(ggplot2)

tar_option_set(
  packages = c("tibble", "tidyverse", "rstan", "readxl", "glue")
)

tar_source()

options(mc.cores = 4)
rstan_options(auto_write = TRUE)
theme_set(theme_bw())


list(
  tar_target(file_qol, here::here("data", "eq5d_uk.csv"), format = "file"),
  tar_target(file_norm, here::here("data", "qale_shortfall.csv"), format = "file"),
  
  ## extraction
  tar_target(data_raw, get_data_qol(file_qol, "uk")),
  tar_target(data_norm, get_data_norm(file_norm)),
  
  ## transformation
  tar_target(data_tte, format_tte(data_raw)),
  tar_target(data_qol, format_qol(data_raw)),
  
  ## modelling
  tar_target(file_model_tte, here::here("models", "time2zero_age.stan"), format = "file"),
  tar_target(model_tte, stan_model(file_model_tte)),
  tar_target(pars_tte, fit_tte(model_tte, data_tte)),

  # 
  tar_target(pars_qol, fit_qol(data_qol)),
  
  ## simulation
  # tar_target(sim_qol_t, simulate_qol_t(pars_tte, pars_qol)),
  # tar_target(sim_shortfall, calc_shortfall(sim_qol_t, data_norm)),
  
  ## presentation
  tar_target(tab_pars_tte, summarise_tte(pars_tte)),
  tar_target(tab_pars_qol, summarise_qol(pars_qol))
  # tar_target(plot_shortfall, vis_shortfall(sim_shortfall)),
  # tar_target(tab_shortfall, summarise_shortfall(sim_shortfall)),
  # 
  # tar_target(gof, eval_gof(data_norm, data_tte, sim_qol_t))
  

)

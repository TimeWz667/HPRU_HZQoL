library(targets)

# library(tarchetypes) # Load other packages as needed.
library(rstan)
library(ggplot2)

tar_option_set(
  packages = c("tibble", "tidyverse", "tidybayes", "rstan", "readxl", "glue")
)

tar_source()


options(dplyr.summarise.inform = FALSE)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
theme_set(theme_bw())


list(
  tar_target(file_norm, here::here("data", "qale_shortfall.csv"), format = "file"),
  
  tar_target(vset, c("uk", "orig")),
  tar_target(file_qol, here::here("data", paste0("eq5d_", vset, ".csv")), format = "file", pattern = map(vset)),

  ## extraction
  tar_target(data_raw, get_data_qol(file_qol, vset), pattern = map(file_qol, vset)),
  tar_target(data_norm, get_data_norm(file_norm)),

  ## modelling, TTE
  tar_target(data_tte, format_tte(data_raw), pattern = slice(data_raw, 1)),
  tar_target(file_model_tte, here::here("models", "time2zero_age.stan"), format = "file"),
  tar_target(model_tte, stan_model(file_model_tte)),
  tar_target(pars_tte, fit_tte(model_tte, data_tte)),
  tar_target(tab_pars_tte, summarise_tte(pars_tte)),
  tar_target(plot_tte, vis_tte(data_tte, pars_tte)),
 
  ## modelling, QoL
  tar_target(data_qol, format_qol(data_raw), pattern = map(data_raw)),
  tar_target(pars_qol, fit_qol(data_qol), map(data_qol)),
  tar_target(tab_pars_qol, summarise_qol(pars_qol, vset), pattern = map(pars_qol, vset)),
  
  tar_target(plot_qol, vis_qol(data_qol, pars_qol, vset), pattern = map(data_qol, pars_qol, vset)),
 
  ## simulation
  tar_target(pars_shortfall, boot_pars(pars_tte, pars_qol, n_sim = 1000), pattern = map(pars_qol)),
  tar_target(sim_shortfall, simulate_shortfall(pars_shortfall, data_norm, vset), pattern = map(pars_qol, vset)),
  tar_target(tab_shortfall, summarise_shortfall(sim_shortfall, vset), pattern = map(sim_shortfall, vset)),
  tar_target(plot_shortfall, vis_shortfall(sim_shortfall, tab_shortfall, vset), pattern = map(sim_shortfall, tab_shortfall, vset))
  
  ## presentation



  # 
  # tar_target(gof, eval_gof(data_norm, data_tte, sim_qol_t))
  

)

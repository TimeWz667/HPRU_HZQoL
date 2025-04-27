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


fig_path <- here::here("docs", "figs")
fig_ext <- ".png"
n_mc <- 1000


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
 
  #### split training sets for exploratory analyses
  tar_target(subdata_tte, split_training_tte(data_tte, prop = 0.5, prop_training = 0.5, seed = 11667 + 100)),
  tar_target(subdata_qol, split_training_qol(data_qol, prop = 0.5, prop_training = 0.5, seed = 11667 + 200)),
  
  ### Population norm baseline
  tar_target(data_raw_pn, get_data_pn(data_raw, data_norm), pattern = map(data_raw)),
  tar_target(data_tte_pn, format_tte(data_raw_pn), pattern = slice(data_raw_pn, 1)),
  tar_target(data_qol_pn, format_qol(data_raw_pn), pattern = map(data_raw_pn)),
  
  ### Pre-HZ baseline
  tar_target(data_baseline, read_csv(file_qol_baseline), pattern = map(data_raw, file_qol_baseline)),
  tar_target(data_raw_0, get_data_qol_shift(data_raw, data_baseline), pattern = map(data_raw, data_baseline)),
  tar_target(data_tte_0, format_tte(data_raw_0), pattern = slice(data_raw_0, 1)),
  tar_target(data_qol_0, format_qol(data_raw_0), pattern = map(data_raw_0)),
  

  # modelling, TTE
  tar_target(file_model_tte, here::here("models", "time2zero_surv_age.stan"), format = "file"),
  tar_target(model_tte, stan_model(file_model_tte)),
  
  tar_target(pars_tte, fit_tte(model_tte, data_tte)),
  tar_target(tabs_tte, restructure_stan(pars_tte)),
  tar_target(gs_tte, visualise_tte(data_tte, tabs_tte)),
  tar_target(file_posterior_tte, output_posterior(tabs_tte, "ph_tte"), format = "file"),
  tar_target(file_vis_tte, output_vis_tte(gs_tte, folder = fig_path, "ph", ext = fig_ext)),
  
  tar_target(pars_tte_pn, fit_tte(model_tte, data_tte_pn)),
  tar_target(tabs_tte_pn, restructure_stan(pars_tte_pn)),
  tar_target(gs_tte_pn, visualise_tte(data_tte_pn, tabs_tte_pn)),
  tar_target(file_posterior_tte_pn, output_posterior(tabs_tte_pn, "pn_tte"), format = "file"),
  tar_target(file_vis_tte_pn, output_vis_tte(gs_tte_pn, folder = fig_path, "pn", ext = fig_ext)),
  
  tar_target(pars_tte_0, fit_tte(model_tte, data_tte_0)),
  tar_target(tabs_tte_0, restructure_stan(pars_tte_0)),
  tar_target(gs_tte_0, visualise_tte(data_tte_0, tabs_tte_0)),
  tar_target(file_posterior_tte_0, output_posterior(tabs_tte_0, "base_tte"), format = "file"),
  tar_target(file_vis_tte_0, output_vis_tte(gs_tte_0, folder = fig_path, "base", ext = fig_ext)),
  tar_target(tabs_tte_compare, vis_tte_comparing(tabs_tte, tabs_tte_pn, tabs_tte_0, folder = fig_path, ext = fig_ext, scale = 1)),

  # modelling, QoL
  tar_target(pars_qol, fit_qol_bayes(data_qol), pattern = map(data_qol)),
  tar_target(file_pars_qol, summarise_qol_bayes(pars_qol, vset), pattern = map(pars_qol, vset), format = "file"),

  tar_target(pars_qol_f, fit_qol_freq(data_qol), pattern = map(data_qol)), # for k-means labels
  tar_target(gs_qol, visualise_qol(data_qol, pars_qol_f, pars_qol, drf = T), pattern = map(data_qol, pars_qol, pars_qol_f, vset)),
  tar_target(file_vis_qol, output_vis_qol(gs_qol, ext = "_" + glue::as_glue(vset) + ".png", drf = T), map(gs_qol, vset)),
  
  # simulate QALY loss
  tar_target(pars_qloss, boot_pars_bayes(file_posterior_tte, file_pars_qol, n_sim = n_mc), pattern = map(file_pars_qol)),
  tar_target(sim_qloss, simulate_ql(pars_qloss, age0 = 50, age1 = 99, age_until = 5, dt = 0.01), pattern = map(pars_qloss)),
  tar_target(tab_qloss, summarise_ql(pars_qloss, sim_qloss), pattern = map(pars_qloss, sim_qloss)),
  tar_target(out_gloss, save_tab(tab_qloss, key = paste0("summary_qloss_ph_", vset)), pattern = map(tab_qloss, vset)),
  
  tar_target(pars_qloss_pn, boot_pars_bayes(file_posterior_tte_pn, file_pars_qol, n_sim = n_mc), pattern = map(file_pars_qol)),
  tar_target(sim_qloss_pn, simulate_ql_pn(pars_qloss_pn, data_norm, age0 = 50, age1 = 99, age_until = 5, dt = 0.01), pattern = map(pars_qloss_pn)),
  tar_target(tab_qloss_pn_sub, summarise_ql_pn(sim_qloss_pn), pattern = map(sim_qloss_pn)),
  tar_target(out_gloss_pn_sub, save_tab(tab_qloss_pn_sub, key = paste0("summary_qloss_pn_sub_", vset)), pattern = map(tab_qloss_pn_sub, vset)),
  tar_target(tab_qloss_pn_sub_short, simplify_ql_pn_sub_tabs(tab_qloss_pn_sub), pattern = map(tab_qloss_pn_sub)),
  tar_target(out_gloss_pn_sub_short, save_tab(tab_qloss_pn_sub_short, key = paste0("summary_qloss_pn_sub_", vset)), pattern = map(tab_qloss_pn_sub_short, vset)),

  tar_target(data_norm_pooled, get_norm_wt(data_raw_pn, data_norm), pattern = map(data_raw_pn)),
  tar_target(sim_qloss_pn_pooled, simulate_ql_pn(pars_qloss_pn, data_norm_pooled, age0 = 50, age1 = 99, age_until = 5, dt = 0.01), pattern = map(pars_qloss_pn, data_norm_pooled)),
  tar_target(tab_qloss_pn, summarise_ql(pars_qloss_pn, sim_qloss_pn_pooled), pattern = map(pars_qloss_pn, sim_qloss_pn_pooled)),
  tar_target(out_gloss_pn, save_tab(tab_qloss_pn, key = paste0("summary_qloss_pn_", vset)), pattern = map(tab_qloss_pn, vset)),

  tar_target(pars_qloss_0, boot_pars_bayes(file_posterior_tte_0, file_pars_qol, n_sim = n_mc), pattern = map(file_pars_qol)),
  tar_target(pars_baseline, get_pars_baseline(data_baseline, n_sim = n_mc), pattern = map(data_baseline)),
  tar_target(sim_qloss_0, simulate_ql_0(pars_qloss_0, pars_baseline, age0 = 50, age1 = 99, age_until = 5, dt = 0.01), pattern = map(pars_qloss_0, pars_baseline)),
  tar_target(tab_qloss_0, summarise_ql(pars_qloss_0, sim_qloss_0), pattern = map(pars_qloss_0, sim_qloss_0)),
  tar_target(out_gloss_0, save_tab(tab_qloss_0, key = paste0("summary_qloss_0_", vset)), pattern = map(tab_qloss_0, vset)),
  
  tar_target(tab_qloss_bind, bind_ql_tabs(tab_qloss_ph = tab_qloss, tab_qloss_pn, tab_qloss_0, ages = seq(50, 90, 10)), pattern = map(tab_qloss, tab_qloss_pn, tab_qloss_0)),
  tar_target(out_gloss_bind, save_tab(tab_qloss_bind, key = paste0("summary_qloss_bind_", vset)), pattern = map(tab_qloss_bind, vset)),
  tar_target(stats, describe_basic(data_raw, data_tte, pars_qol_f, vset), pattern = map(data_raw, pars_qol_f, vset))
)

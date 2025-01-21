

split_training_tte <- function(dat, prop = 0.5, prop_training = 0.5, seed = 11667) {
  set.seed(seed)
  
  sub <- dat %>% 
    slice_sample(prop = prop, by = SID)
  
  training <- sub %>% 
    slice_sample(prop = prop_training, by = SID)
  
  test <- sub %>% 
    filter(!PID %in% c(training %>% pull(PID)))
  
  return(list(
    meta = c(
      Prop = prop,
      Prop_training = prop_training,
      Prop_test = 1 - prop_training,
      N_src = nrow(dat),
      N_trainig = nrow(training),
      N_test = nrow(test)
    ),
    training = training,
    test = test
  ))
}


split_training_qol <- function(dat, prop = 0.5, prop_training = 0.5, seed = 11667) {
  set.seed(seed)
  
  subjects <- dat %>% select(SID, PID) %>% distinct()
    
  sub <- subjects %>% 
    slice_sample(prop = prop)
  
  training <- sub %>% 
    slice_sample(prop = prop_training, by = SID) %>% 
    left_join(dat)
  
  test <- sub %>% 
    filter(!PID %in% c(training %>% pull(PID))) %>% 
    left_join(dat)
  
  return(list(
    meta = c(
      Prop = prop,
      Prop_training = prop_training,
      Prop_test = 1 - prop_training,
      N_src = nrow(dat),
      N_trainig = nrow(training),
      N_test = nrow(test)
    ),
    training = training,
    test = test
  ))
}

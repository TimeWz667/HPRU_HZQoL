library(tidyverse)
library(targets)


theme_set(theme_bw())


# dat_qol <- tar_read(data_qol, 1)
# 
# 
# ds_nz <- dat_qol %>% mutate(
#     Gs = as.numeric(as.factor(SID))
#   ) %>% filter(!is.na(Age)) %>% 
#   filter(EQ5D < 1)
# 
# 
# map_gp <- ds_nz %>% select(SID, Gs) %>% distinct() %>% 
#   mutate(
#     Gname = case_when(
#       startsWith(SID, "Drolet") ~ glue::as_glue(SID) + " Canada",
#       startsWith(SID, "Scott") ~ glue::as_glue(SID) + " England",
#       startsWith(SID, "van") ~ glue::as_glue(SID) + "van Wijck et al. 2016 Netherlands",
#       T ~ SID
#     )
#   ) %>% 
#   tidyr::extract(Gname, c("Author", "Year", "Country"), "(\\S+) et al. (\\d+) (\\S+)") %>% 
#   mutate(
#     Gname = sprintf("%s %s %s", Author, Country, Year)
#   ) %>%
#   arrange(Gs)
#   
# 
# write_csv(map_gp, here::here("docs", "map_studies.csv"))

map_gp <- read_csv(here::here("docs", "map_studies.csv"))
map_gp



pars_base <- read_csv(here::here("docs", "tabs", "stats_qol_b_orig.csv"))
pars_gp <- read_csv(here::here("docs", "tabs", "stats_qol_b_bg_orig.csv"))

heter_beta <- pars_gp %>% 
  tidyr::extract(Var, "Gs", "bg\\[(\\d+)\\]", convert = T) %>% 
  left_join(map_gp) %>% 
  arrange(Model, Gs) %>% 
  mutate(
    SID = sprintf("%s %s\n%s", Author, Year, Country),
    SID = ifelse(is.na(Year), "Pooled", SID), 
    SID = factor(SID)
  )
  
  
labs_models <- c(
  C1 = "Model 1: mild discomfort", 
  C2 = "Model 2: severe discomfort",
  PZ = "Model 3: Pr(temporally well)", 
  PC1 = "Model 4: Pr(mild discomfort|not temporally well)"
)
  
g_heter_beta <- heter_beta %>% 
  select(Model, SID, M = mean, L = `X2.5.`, U = `X97.5.`) %>%
  mutate(Model = factor(Model, levels = names(labs_models))) %>% 
  ggplot(aes(y = SID)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(aes(x = M)) +
  geom_errorbar(aes(xmin = L, xmax = U), width = 0.3) +
  facet_wrap(.~Model, labeller = labeller(Model = labs_models)) +
  scale_x_continuous("Random-effect term") +
  scale_y_discrete("", limits = rev) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(g_heter_beta, filename = here::here("docs", "figs", paste0("g_qol_heter_beta.png")), width = 7, height = 8)



res <- bind_rows(
  rstan::extract(post_c1, pars = c("b0", "bg")) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  mutate(bg.0 = 0) %>% 
  pivot_longer(-b0) %>% 
  mutate(
    pred = value + b0,
    sub = "C1"
  ),
  rstan::extract(post_c2, pars = c("b0", "bg")) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  mutate(bg.0 = 0) %>% 
  pivot_longer(-b0) %>% 
  mutate(
    pred = value + b0,
    sub = "C2"
  )
) %>% 
  group_by(name, sub)%>% 
  summarise(
    M = median(pred),
    L = quantile(pred, 0.05),
    U = quantile(pred, 0.95)
  ) %>% 
  tidyr::extract(name, "Gs", "bg.(\\d+)", convert = T) %>% 
  left_join(map_gp) %>% 
  arrange(sub, Gs)

write_csv(res, here::here("docs", "tabs", "tab_hetero.csv"))




dat <- read_csv(here::here("docs", "tabs", "tab_hetero.csv")) %>% 
  mutate(
    SID = sprintf("%s %s\n%s", Author, Year, Country),
    SID = ifelse(is.na(Year), "Pooled", SID), 
    SID = factor(SID)
  )


g_heter <- dat %>% 
  filter(SID != "Pooled") %>% 
  ggplot(aes(y = SID)) +
  geom_point(aes(x = M)) +
  geom_errorbar(aes(xmin = L, xmax = U), width = 0.3) +
  geom_pointrange(data = dat %>% filter(SID == "Pooled"), aes(y = SID, x = M, xmin = L, xmax = U)) +
  facet_grid(.~sub, labeller = labeller(sub = c(C1 = "mild discomfort", C2 = "severe discomfort"))) +
  scale_x_continuous("EQ-5D score") +
  scale_y_discrete("", limits = rev) +
  expand_limits(x = 1) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


ggsave(g_heter, filename = here::here("docs", "figs", paste0("g_qol_heter.png")), width = 7, height = 4)


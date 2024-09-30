library(data.table)
library(ggdist)

ours = fread(here::here("posteriors", "QALY_loss_sims_uk.csv"))
theirs = fread(here::here("experimental", "2019_fig6.csv"))

plot = ggplot() +
  stat_interval(data = ours[Age < 99], aes(x = Age, y = QLH35)) +
  geom_ribbon(data = theirs, aes(x = Age, ymin = lower, ymax = upper), fill = "#ec3", alpha = 0.5) +
  geom_line(data = theirs, aes(x = Age, y = middle), colour = "#440") +
  scale_colour_brewer() +
  ylim(0, NA) +
  theme(legend.position = "none") +
  labs(x = "Age at rash onset", y = "QALY loss")

ggsave(plot, filename = here::here("experimental", "qaly_loss.png"), width = 4, height = 3)

  
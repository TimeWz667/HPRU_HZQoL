library(data.table)
library(ggplot2)

# Load data
data = fread("./data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv")
data = data[order(study, Patient.ID, time_points)]

# This extends the method in example_area.R with additional areas (%%%):
#           *                                   
#          #|##                                 
#        ###|#####*                    *        
#    %%*####|#####|#####          #####|%%%%   
# %%%%%|####|#####|##########*#########|%%%%%%%
#
# The area on the left is the QALY loss that prompted the visit to health care.
# The area on the right, only for those patients for whom calculation of QALY
# over time was censored.

# Do some processing on data to identify censored patients etc
### LEFT OFF HERE
data[, last := c(rep(FALSE, .N - 1)

# Calculate QALY loss for each patient
area = function(t, x)
{
    widths = diff(t)
    heights = (head(x, -1) + tail(x, -1)) / 2
    sum(widths * heights)
}
losses = data[, .(loss_min = area(time_points, 1 - EQ5D) / 365.25), 
    by = .(study, Patient.ID, age)]

# Look at trend in QALY loss with age
losses[, author := sub(" et al.*$", "", study)]
ggplot(losses[age >= 50], aes(x = age, y = loss_min)) +
    geom_point(aes(colour = author)) +
    geom_smooth() +
    geom_vline(aes(xintercept = 80)) + 
    coord_cartesian(ylim = c(0, 0.2)) +
    scale_y_continuous(breaks = seq(0, 0.2, by = 0.02))
ggsave("./experimental/example_area_age.pdf", width = 8, height = 4)

# Look at individual QALY loss per patient
data = merge(data, losses, by = c("study", "Patient.ID", "age"), all = TRUE)
data[, patient_number := .GRP, by = .(study, Patient.ID)]
data[, panel_id := paste(patient_number, round(loss_min, 3))]

# Save as a multi-page figure
pdf("./experimental/example_area_individual.pdf", width = 10, height = 6)

patients = data[, unique(Patient.ID)]
for (p in seq(1, length(patients), by = 40)) {
    cat(".")
    pat = patients[p:(p + 39)]
    pat = pat[!is.na(pat)]
    plot = ggplot(data[Patient.ID %in% pat]) +
        geom_area(aes(x = time_points, y = 1 - EQ5D)) +
        geom_point(aes(x = time_points, y = 1 - EQ5D), colour = "red", size = 0.5) +
        facet_wrap(~panel_id, nrow = 5, ncol = 8) +
        cowplot::theme_cowplot(font_size = 7) +
        labs(x = "Time (days)", y = "QALY loss")
    print(plot)
}

dev.off()

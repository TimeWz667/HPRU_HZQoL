library(data.table)
library(ggplot2)
library(mgcv)

# Load data
data = fread("./data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv")

ggplot(data[age >= 50]) +
  geom_point(aes(x = time_points, y = EQ5D, colour = "Data"), alpha = 0.1) +
  geom_smooth(aes(x = time_points, y = EQ5D), method = "loess") +
  facet_wrap(~age)

# Fit model with spline surface on time and age
model1 = gam(EQ5D ~ s(time_points, age, k = 4), data = data)
# plot(model1)
newdata = CJ(age = 50:99, time_points = 0:365)
newdata$eq1 = predict(model1, newdata = newdata)

# Another model, use sqrt of time as there is less data further out, make age and time additive
data[, sqrt_time := sqrt(time_points)]
newdata[, sqrt_time := sqrt(time_points)]
model2 = gam(EQ5D ~ s(sqrt_time, k = 3) + s(age, k = 3), data = data)
# plot(model2)
newdata$eq2 = predict(model2, newdata = newdata)

# Plot relationship
ggplot() +
    geom_point(data = data[age >= 50], aes(x = time_points, y = EQ5D, colour = "Data"), alpha = 0.1) +
    geom_line(data = newdata, aes(x = time_points, y = eq1, colour = "Model 1")) +
    geom_line(data = newdata, aes(x = time_points, y = eq2, colour = "Model 2")) +
    facet_wrap(~age)

# Make sure no predictions out of bounds
newdata[, range(eq1)]
newdata[, range(eq2)]

# Get QALY loss out to first year
summ = newdata[, .(qaly_loss_1 = mean(1 - eq1), qaly_loss_2 = mean(1 - eq2)), by = age]
ggplot(summ[age <= 90]) +
  geom_line(aes(x = age, y = qaly_loss_1, colour = "Model 1")) + 
  geom_line(aes(x = age, y = qaly_loss_2, colour = "Model 2")) +
  scale_y_continuous(breaks = seq(0.08, 0.16, by = 0.02), limits = c(0.07, 0.16))


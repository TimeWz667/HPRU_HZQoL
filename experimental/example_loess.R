library(data.table)
library(ggplot2)
library(mgcv)

# Load data
data = fread("./data/27-06-2018 EQ5D_IL.df.mini EQ5D individual level data.csv")

# Restrict data to 50-90, amalgamate by decade
data = data[age >= 50]
data[, age_decade := floor(age / 10) * 10]

# Do loess regression for each decade and predict
decades = c(50, 60, 70, 80, 90)
newdata = CJ(age_decade = decades, time_points = 0:365)
for (a in decades) {
  d2 = data[age_decade == a]
  lom = loess(EQ5D ~ time_points, data = d2)
  newdata[age_decade == a, eq1 := predict(lom, newdata = .SD)]
}

# Cap at 1
newdata[, eq1 := pmin(eq1, 1.0)]

# Fix NAs
newdata[, eq1 := zoo::na.fill(eq1, fill = "extend"), by = "age_decade"]

# Check
ggplot(newdata) +
  geom_point(aes(x = time_points, y = eq1, colour = age_decade, group = age_decade)) +
  ylim(c(NA, 1))
# A bit wonky, but ok...



# Calculate losses...
ggplot(newdata[, .(qaly_loss_1 = mean(1 - eq1)), by = age_decade]) +
  geom_line(aes(x = age_decade, y = qaly_loss_1)) +
  scale_y_continuous(breaks = seq(0.08, 0.16, by = 0.02), limits = c(0.07, 0.16))

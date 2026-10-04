ship <- read.csv(file.choose())

names(ship)
str(ship)
colSums(is.na(ship))

ship2 <- ship[!is.na(ship$exposure), ]

nrow(ship2)

ship2$any_accident <- ifelse(ship2$accidents > 0, 1, 0)

table(ship2$any_accident)
prop.table(table(ship2$any_accident))

summary(ship2$exposure)
sd(ship2$exposure)

ship2$construction_era <- ifelse(
  ship2$construction1 == 1, "Era 1",
  ifelse(
    ship2$construction2 == 1, "Era 2",
    ifelse(
      ship2$construction3 == 1, "Era 3",
      "Reference Era"
    )
  )
)

table(ship2$construction_era)

baseline <- lm(
  accidents ~ exposure + construction1 + construction2 + construction3,
  data = ship2
)

summary(baseline)

par(mfrow = c(2, 2))
plot(baseline)
par(mfrow = c(1, 1))

library(lmtest)

bptest(baseline)
shapiro.test(residuals(baseline))

log_model <- lm(
  log(accidents + 1) ~ exposure + construction1 + construction2 + construction3,
  data = ship2
)

summary(log_model)

par(mfrow = c(2, 2))
plot(log_model)
par(mfrow = c(1, 1))

bptest(log_model)
shapiro.test(residuals(log_model))

logistic_model <- glm(
  any_accident ~ exposure + construction1 + construction2 + construction3,
  data = ship2,
  family = binomial
)

summary(logistic_model)

revised_logistic <- glm(
  any_accident ~ exposure,
  data = ship2,
  family = binomial
)

summary(revised_logistic)

exp(coef(revised_logistic))
exp(confint(revised_logistic))
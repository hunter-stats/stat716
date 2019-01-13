# if you don't have the packages installed you can uncomment the line 
# below to install them. Uncomment the line by removing the # at the front.
# install.packages(c("ISLR", "dplyr", "ggplot2"))

library(ISLR)
library(dplyr)
library(ggplot2)

# education

# where they live
# wage

# correlation
# vs. health

# prediction of wage

# distributions of wage in the data set
# prediction of having health insurance

Wage %>% 
  ggplot(aes(x = age, y = wage)) + 
  geom_point() + 
  geom_smooth()

Smarket %>% 
  glimpse

Smarket %>% 
  ggplot(aes(x = Lag1, y = Today)) +
  geom_point() +
  geom_smooth()

data.frame(
  Z1 = NCI60$data[, 1],
  Z2 = NCI60$data[, 2],
  label = NCI60$labs) %>% 
  ggplot(aes(x = Z1, y = Z2, color = label)) + geom_point()

Wage %>% 
  ggplot(aes(x = age, y = wage)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(method = "lm",
              formula =  y ~ x + I(x^2),
              color = "red")

library(splines)

mod1 <- lm(wage ~ age, data = Wage)
mod2 <- lm(wage ~ ns(age, df = 5), data = Wage)
mod3 <- lm(wage ~ ns(age, df = 30), data = Wage)

wage_pred <- Wage %>% 
  select(age, wage)

wage_pred$mod1 <- predict(mod1, wage_pred)

wage_pred %>% glimpse

wage_pred$mod2 <- predict(mod2, wage_pred)
wage_pred$mod3 <- predict(mod3, wage_pred)

wage_pred %>% glimpse

wage_pred %>% 
  ggplot(aes(x = age, y = wage)) +
  geom_point(alpha = .2) +
  # geom_smooth(method = "lm")+
  geom_line(aes( y = mod1), color = "blue", size = 2) + 
  geom_line(aes(y =  mod2), color = "red", size = 2) + 
  geom_line(aes(y = mod3), color = "green", size = 2)


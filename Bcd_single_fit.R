library(readxl)
library(tidyverse)
library(gridExtra)

data <- read_excel("/Users/shaheenkabir/Bcd_analysis/BR8/BR8_25/BR8_25_4/BR8_25_4_Bcd_profile_abs.xlsx")
colnames(data) <- c("Length", "intensity")
#data$norm_length <- (data$Length/max(data$Length))*100
data$norm_length <- data$Length
#max(data$Length) * .25
plot1 <- ggplot(data, aes(x = norm_length, y = intensity))+
  geom_line()+
  labs(x = "Position (%)", y = "Bicoid concentration")
plot2 <- ggplot(data, aes(x = norm_length, y = intensity))+
  geom_point()+
  labs(x = "Position (%)", y = "Bicoid concentration")

#data <- data %>% dplyr::filter(norm_length <= 50)
fit <- nls(intensity ~ A * exp(-norm_length / lambda) + B, data = data, start = list(A = max(data$intensity), lambda = 30, B = min(data$intensity)))
summary(fit)
coef(fit)["lambda"]
coef(fit)["B"]
data$fit <- predict(fit, newdata = data)

plot3 <- ggplot(data, aes(norm_length, intensity)) +
  geom_line() +
  geom_line(aes(y = fit), color = "red") +
  labs(x = "Position (%)", y = "Bicoid concentration",
       title = "Exponential fit of Bicoid gradient")
plot4 <- ggplot(data, aes(norm_length, intensity)) +
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(x = "Position (%)", y = "Bicoid concentration",
       title = "Exponential fit of Bicoid gradient")
grid.arrange(plot1, plot2, plot3,plot4, ncol = 2)








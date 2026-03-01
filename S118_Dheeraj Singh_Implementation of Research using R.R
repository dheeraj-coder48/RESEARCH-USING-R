install.packages("ggplot2")
install.packages("dplyr")

# Load libraries
library(ggplot2)
library(dplyr)

cybercrime_yearly <- data.frame(
  Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 
           2018, 2019, 2020, 2021, 2022, 2023),
  Cases = c(1322, 2213, 3477, 5693, 9622, 11592, 12317, 21796, 
            27248, 44735, 50035, 52974, 65893, 86420)
)

print(cybercrime_yearly)

cybercrime_yearly <- cybercrime_yearly %>%
  mutate(Growth_Rate = (Cases - lag(Cases)) / lag(Cases) * 100)

print(cybercrime_yearly)

ggplot(cybercrime_yearly, aes(x = Year, y = Cases)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Cybercrime Cases in India (2010–2023)",
       x = "Year",
       y = "Reported Cybercrime Cases") +
  theme_minimal()
















summary(cybercrime_yearly$Cases)

mean_cases <- mean(cybercrime_yearly$Cases)
median_cases <- median(cybercrime_yearly$Cases)
sd_cases <- sd(cybercrime_yearly$Cases)

cat("Mean:", mean_cases, "\n")
cat("Median:", median_cases, "\n")
cat("Standard Deviation:", sd_cases, "\n")


correlation <- cor(cybercrime_yearly$Year, cybercrime_yearly$Cases)

cat("Correlation between Year and Cybercrime Cases:", correlation)
















model <- lm(Cases ~ Year, data = cybercrime_yearly)

summary(model)

ggplot(cybercrime_yearly, aes(x = Year, y = Cases)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression Trend (2010–2023)",
       x = "Year",
       y = "Cybercrime Cases") +
  theme_minimal()







future_data <- data.frame(Year = 2024)

predicted_2024 <- predict(model, future_data)

cat("Predicted Cybercrime Cases for 2024:", predicted_2024)


cybercrime_states_2024 <- data.frame(
  State = c("Karnataka", "Telangana", "Uttar Pradesh",
            "Maharashtra", "Tamil Nadu", "Gujarat"),
  Cases = c(575000, 510000, 340000,
            295000, 210000, 180000)
)

print(cybercrime_states_2024)


ggplot(cybercrime_states_2024,
       aes(x = reorder(State, -Cases), y = Cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "State-wise Cybercrime Complaints (2024 Provisional)",
       x = "State",
       y = "Number of Complaints") +
  theme_minimal()
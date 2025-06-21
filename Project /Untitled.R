setwd('/Users/tagishimashego/Desktop/Economics Masters/Data Science /Potential project /Potential Project ')

library(tidyverse)
library(lubridate)




Macro%>% filter(countryname ==  "Aruba ") %>%  ggplot(aes(x = year, y = rGDP, color = countryname)) +  geom_line() + labs(title = "SA GDP ")


countra <- c("Belgium", "Switzerland", "Canada", "Denmark", "Sweden", "Norway")


Macro%>% filter(year >= 1875) %>% filter(countryname %in% countra) %>%  ggplot(aes(x = year, y = ltrate, color = countryname)) + geom_line() + labs(title = " LONG Interest ")

MzansiMAcro <- Macro %>% filter(countryname == "South Africa")

MzansiMAcro %>% filter(year>= 1970) %>% ggplot(aes(x = year, y = ltrate, color = countryname)) + geom_line() + labs(title = " LONG Interest ")


MzansiMAcro %>% filter(year>= 1970) %>% ggplot(aes(x = year, y = strate, color = countryname)) + geom_line() + labs(title = " SHORT Interest ")

MzansiMAcro %>% filter(year>=2010 & year<= 2017 ) %>%  mutate(avg_inflation = mean(infl, na.rm = TRUE))

MzansiMAcro %>% filter(year >= 2010 & year <= 2017) %>% summarise(avg_inflation = mean(infl, na.rm = TRUE))
## This you can have in your paper , the MPC was established in 2017 



Couuu <-MzansiMAcro %>% filter(year>= 2000) %>% filter( infl< 3 | infl > 6) %>% select(year , infl)%>% arrange(year)


Couuu


UsaMAcro <- Macro %>% filter(countryname == "United States")

UsaMAcro %>% filter(year>= 1970) %>% ggplot(aes(x = year, y = ltrate, color = countryname)) + geom_line() + labs(title = " LONG Interest ")


byy <- c("United States", "South Africa")

Macro%>% filter(year >= 1970) %>% filter(countryname %in% byy) %>%  ggplot(aes(x = year, y = ltrate, color = countryname)) + geom_line() + labs(title = " LONG Interest ")

Macro%>% filter(year >= 1970) %>% filter(countryname %in% byy) %>%  ggplot(aes(x = year, y = strate, color = countryname)) + geom_line() + labs(title = " Short Interest ")

Macro%>% filter(year >= 1970) %>% filter(countryname %in% byy) %>%  ggplot(aes(x = year, y = cbrate, color = countryname)) + geom_line() + labs(title = "  Central Interest ")





sa_rates_long <- MzansiMAcro %>% filter(year >= 1970) %>% pivot_longer(cols = c(cbrate, strate),names_to = "rate_type", values_to = "rate")

ggplot(sa_rates_long, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) +
  labs(title = "South Africa: Central Bank Rate vs. Short-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 



sa_rates_long1 <- MzansiMAcro %>% filter(year >= 1970) %>% pivot_longer(cols = c(cbrate, ltrate),names_to = "rate_type", values_to = "rate")

ggplot(sa_rates_long1, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) +
  labs(title = "South Africa: Central Bank Rate vs. Long-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 

sa_rates_long2 <- MzansiMAcro %>% filter(year >= 1970) %>% pivot_longer(cols = c(strate, ltrate),names_to = "rate_type", values_to = "rate")

ggplot(sa_rates_long2, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) +
  labs(title = "South Africa: Short-Term Market Rate vs. Long-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 


sa_rates_long4 <- MzansiMAcro %>% filter(year >= 1970 & year <= 2024)%>% pivot_longer(cols = c(strate, ltrate, infl),names_to = "rate_type", values_to = "rate")

ggplot(sa_rates_long4, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) + scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  labs(title = "South Africa: Short-Term Market Rate vs. Long-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 





Zimbo <- Macro %>% filter(countryname == "Zimbabwe")

ZIM_rates_long1 <- Zimbo  %>% filter(year >= 1970) %>% pivot_longer(cols = c(strate, ltrate),names_to = "rate_type", values_to = "rate")

ggplot(ZIM_rates_long1, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) +
  labs(title = "ZIM: Short-Term Market Rate vs. Long-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 

ZIM_rates_long2 <- Zimbo  %>% filter(year >= 1970) %>% pivot_longer(cols = c(strate, cbrate),names_to = "rate_type", values_to = "rate")

ggplot(ZIM_rates_long2, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) +
  labs(title = "ZIM: Short-Term Market Rate vs. Central Bank Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 







USA_rates_long <- UsaMAcro %>% filter(year >= 1970) %>% pivot_longer(cols = c(cbrate, strate),names_to = "rate_type", values_to = "rate")

ggplot(USA_rates_long, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) +
  labs(title = "USA: Central Bank  Rate vs. Short-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 


USA_rates_long1 <- UsaMAcro %>% filter(year >= 1970) %>% pivot_longer(cols = c(cbrate, ltrate),names_to = "rate_type", values_to = "rate")

ggplot(USA_rates_long1, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) +
  labs(title = "USA: Central Bank  Rate vs. Long-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 


USA_rates_long2 <- UsaMAcro %>% filter(year >= 1970) %>% pivot_longer(cols = c(strate, ltrate ),names_to = "rate_type", values_to = "rate")

ggplot(USA_rates_long2, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) +
  labs(title = "USA: Short-Term Market Rates vs. Long-Term Market Rate",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 


USA_rates_long3 <- UsaMAcro %>% filter(year >= 1970) %>% pivot_longer(cols = c(strate, ltrate,infl ),names_to = "rate_type", values_to = "rate")

ggplot(USA_rates_long3, aes(x = year, y = rate, color = rate_type)) +
  geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) +
  labs(title = "USA: Short-Term Market Rates vs. Long-Term Market Rate and INFl",
       x = "Year", y = "Interest Rate (%)",
       color = "Rate Type") +
  theme_minimal() 



MzansiMAcro %>% filter(year >=2000 & year<=2024) %>% mutate(outside_target = infl < 3 | infl > 6) %>% ggplot(aes(x = year, y = infl)) +
  geom_line(color = "grey60") +
  geom_point(aes(color = outside_target), size = 3) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 6, linetype = "dashed", color = "blue") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
  labs(
    title = "Years with Inflation Outside SARB Target Range (3–6%)",
    x = "Year",
    y = "Inflation Rate (%)"
  ) +
  theme_minimal()

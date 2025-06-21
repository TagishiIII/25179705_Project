MzansiMAcro <-Macro %>% filter(countryname == "South Africa")




Mzansi_plot <- g1 <- MzansiMAcro(data) %>% filter(year >=2000 & year<=2024) %>% mutate(outside_target = infl < 3 | infl > 6)

       g2 <- g1 %>%
       ggplot(aes(x = year, y = infl)) +
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


    g2





rates_plot <- function(data){



    g3 <- MzansiMAcro(data)  %>% filter(year >= 2000 & year <= 2024)%>% pivot_longer(cols = c(strate, ltrate, infl),names_to = "rate_type", values_to = "rate")

g4 <- g3 %>% ggplot(aes(x = year, y = rate, color = rate_type)) +
    geom_line(linewidth = 1.2) + scale_y_continuous(limits = c(-5, 25)) + scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
    labs(title = "Inflation and Interest Rate Dynamics in South Africa ",
         x = "Year", y = "Rate (%)",
         color = "Rate Type") +
    theme_minimal()


g4

}




df <- MzansiMAcro %>%
    filter(year >= 2008 & year <= 2024) %>%
    arrange(year) %>%
    mutate(infl_lag1 = lag(infl, 1))

model <- lm(infl ~ infl_lag1, data = df)
fyf<- summary(model)

hibo<-xtable(fyf , caption = "Inflation Persistence in South Africa (2008-2024) \\label{tab4}" )


print.xtable(hibo,
             # tabular.environment = "longtable",
             floating = TRUE,
             table.placement = 'H',
             # scalebox = 0.3,
             comment = FALSE,
             caption.placement = 'bottom'
)


clean_df <- MzansiMAcro %>%
    filter(year >= 2000 & year <= 2024) %>%
    dplyr ::select(infl, short_rate = strate, long_rate = ltrate) %>%
    na.omit()

ts_data <- ts(clean_df, start = 2000, frequency = 1)

var_model <- VAR(ts_data, p = 2, type = "const")


short_rate_output <- summary(var_model$varresult$short_rate)$coefficients
short_rate_table <- xtable(short_rate_output,
                           caption = "VAR Equation for Short-Term Interest Rate \\label{tab34}")
print(short_rate_table,
      include.rownames = TRUE,
      caption.placement = "bottom",  comment = FALSE)





Persist <- function(data) {
    df <- MzansiMAcro(data) %>%
        filter(year >= 2008 & year <= 2024) %>%
        arrange(year) %>%
        mutate(infl_lag1 = lag(infl, 1))

    model <- lm(infl ~ infl_lag1, data = df)
    tidy(model)  # return tidy model output
}

model_output <- Persist(data = Macro)
model_table <- xtable(model_output, caption = "Inflation Persistence in South Africa (2008–2024) \\label{tab4}")


print.xtable(model_table,
             floating = TRUE,
             table.placement = 'H',
             comment = FALSE,
             caption.placement = 'bottom')


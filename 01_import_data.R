library(haven)
data <- read_sav("data/ESS Country data - No.sav")
names(data)
str(data)
grep("edu", names (data), value = TRUE)
grep("life| happy| depress| health", names(data), value = TRUE)
grep("^edu", names(data), value = TRUE)
summary(data$eduyrs)
summary(data$stflife)
hist(data$eduyrs)
cor(data$eduyrs, data$stflife, use = "complete.obs")
cor.test(data$eduyrs, data$stflife, use = "complete.obs")
c("fltdpr", "fltanx", "flteeff", "fltlnl")
names(data)[grep("flteeff|fltlnl", names(data))]
attributes(data$fltlnl)
attributes(data$fltlnla)
attributes(data$fltdpr)
attributes(data$fltanx)
attributes(data$flteeff)
attributes(data$eduyrs)
attributes(data$eduyrs)
table(data$eduyrs)
library(dplyr)
data <- data %>%
  mutate(
    fltdpr = ifelse(fltdpr >= 5, NA, fltdpr) ,
    fltanx = ifelse(fltanx >= 5, NA, fltanx) ,
    flteeff = ifelse(flteeff >= 5, NA, flteeff) ,
    fltlnl = ifelse(fltlnl >= 5, NA, fltlnl) 
  )
table(data$fltdpr, useNA = "always")
sum(is.na(data$fltdpr) | is.na(data$fltanx) | is.na(data$flteeff) | is.na(data$fltlnl))
data <- data %>%
  mutate(mental_health_index = rowMeans(
    select(., fltdpr, fltanx, flteeff, fltlnl) ,
    na.rm = TRUE
  ))
summary(data$mental_health_index)
sum(!is.na(data$mental_health_index))
sapply(list(fltdpr = data$fltdpr, 
            fltanx = data$fltanx, 
            flteeff = data$flteeff, 
            fltlnl = data$fltlnl), 
       function(x) sum(is.na(x)))
library(ggplot2)
  
  summary_stats <- data.frame(
  Statistikk = c("Minimum", "Q1", "Median", "Gjennomsnitt", "Q3", "Maksimum") ,
  Verdi = c(1.000, 1.000, 1.250, 1.289, 1.500, 4.000)
)

summary_stats$Statistikk <- factor(summary_stats$Statistikk, 
                                   levels = c("Minimum", "Q1", "Median", "Gjennomsnitt", "Q3", "Maksimum"))
ggplot(summary_stats, aes(x = Statistikk, y = Verdi, fill = Statistikk)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Summary statistikk - Psykisk helseindeks",
    x = "",
    y = "Verdi",
    caption = "Data: ESS Round 11, Norge"
  ) +
  theme(legend.position = "none")
ggplot(data, aes(x = eduyrs)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Fordeling av utdanningsar - Norge",
    x = "Utdanningsar",
    y = "Frekvens",
    caption = "Data: ESS Round 11, Norge"
  )
sum(is.na(data$eduyrs))
cor.test(data$eduyrs, data$mental_health_index, use = "complete.obs")
ggplot(data, aes(x = eduyrs, y = stflife)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Sammenheng mellom utdanningsar og livstilfredshet",
    x = "Utdanningsar",
    y = "Livstilfredshet (stflife)",
    caption = "Data: ESS Round 11, Norge | r = -0.02, p = 0.02, N = 17402"
  )
names(data)[grep("stl", names(data))]
names(data)[grep("life|satis|lsat", names(data), ignore.case = TRUE)]
attributes(data$stflife)
data <- data %>%
  mutate(stflife = ifelse(stflife >= 77, NA, stflife))
cor.test(data$eduyrs, data$stflife, use = "complete.obs")
ggplot(data, aes(x = eduyrs, y = stflife)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Sammenheng mellom utdanningsar og livstilfredshet",
    x = "Utdanningsar",
    y = "Livstilfredshet (0-10)",
    caption = "Data: ESS Round 11, Norge | r = -0.017, p = 0.022, N = 17306"
  )
ggplot(data, aes(x = eduyrs, y = mental_health_index)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Sammenheng mellom utdanningsar og psykisk helse",
    x = "Utdanningsar",
    y = "Psykisk helseindeks (1-4)",
    caption = "Data: ESS Round 11, Norge | r = -0.075, p < 0.001, N = 6126"
  )
# library(psych)
# install.packages("psych")
library(psych)
alpha(data[ c("fltdpr", "fltanx", "flteeff", "fltlnl")])
library(ggplot2)
ggplot(data, aes(x = mental_health_index)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Fordeling av psykisk helseindeks - Norge" ,
    x = "Psykisk helseindeks (1-4)" ,
    y = "Frekvens" , 
    caption = "Data: ESS round 11, Norge"
  )

           
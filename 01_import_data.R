library(haven)



data <- read_sav("../data/ESS Country data - No.sav")


#Inspeksjon av variabler

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


#Fjerner manglende verdier p?? indeksvariablene jeg ??nsker.

data <- data %>%
  mutate(
    fltdpr = ifelse(fltdpr >= 5, NA, fltdpr) ,
    fltanx = ifelse(fltanx >= 5, NA, fltanx) ,
    flteeff = ifelse(flteeff >= 5, NA, flteeff) ,
    fltlnl = ifelse(fltlnl >= 5, NA, fltlnl) 
  )


#Sjekker for manglende verdier i indeksvariabler. Stort frafall.  

table(data$fltdpr, useNA = "always")
sum(is.na(data$fltdpr) | is.na(data$fltanx) | is.na(data$flteeff) | is.na(data$fltlnl))


#Konstruksjon av psykisk helseindeks.

data <- data %>%
  mutate(mental_health_index = rowMeans(
    select(., fltdpr, fltanx, flteeff, fltlnl) ,
    na.rm = TRUE
  ))


#Sjekker data for konstruert indeks.

summary(data$mental_health_index)
sum(!is.na(data$mental_health_index))
sapply(list(fltdpr = data$fltdpr, 
            fltanx = data$fltanx, 
            flteeff = data$flteeff, 
            fltlnl = data$fltlnl), 
       function(x) sum(is.na(x)))


#Cronbach's alfa p?? psykisk helseindeks

# library(psych)
# install.packages("psych")
library(psych)
alpha(data[ c("fltdpr", "fltanx", "flteeff", "fltlnl")])


#Deskriptiv statistikk


library(ggplot2)
  
#Histogram av fordelingen av respondenter p?? antall utdannings??r- histogram

ggplot(data, aes(x = eduyrs)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Fordeling av utdanningsar - Norge",
    x = "Utdanningsar",
    y = "Frekvens",
    caption = "Data: ESS Round 11, Norge"
  )


#Historgram av fordelingen av respondenter p?? psykisk helseindeks-frekvens 

ggplot(data, aes(x = mental_health_index)) +
  geom_histogram(binwidth = 0.25, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Fordeling av psykisk helseindeks - Norge" ,
    x = "Psykisk helseindeks (1-4)" ,
    y = "Frekvens" , 
    caption = "Data: ESS round 11, Norge"
  )


#Fjerner manglende verdier p?? livsstilsfredshet

data <- data %>%
  mutate(stflife = ifelse(stflife >= 77, NA, stflife))

#Korrelasjonstest: utdannings??r -> livsstilsfredshet

cor.test(data$eduyrs, data$stflife, use = "complete.obs")

#OLS regresjon- figur. Korrelasjon: utdannings??r -> livsstilsfredshet  

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

#OLS regresjon- figur. Korrelasjon: utdannings??r-> psykisk helseindeks 

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

           
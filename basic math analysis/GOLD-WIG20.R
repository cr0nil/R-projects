require(dplyr)
require(ggplot2)# wykresy
require(qrmtools)
require(moments)# kurtoza i skośność

#csv from stooq
url_gold <- "https://stooq.pl/q/d/l/?s=gold.us&d1=20090404&d2=20240403&i=d"
url_wig20 <- "https://stooq.pl/q/d/l/?s=wig20&d1=20090404&d2=20240404&i=d"

dataset_gold <- read.csv(url_gold)
dataset_wig20 <- read.csv(url_wig20)

# dane muszą być równe dla korelacji(przycięcię do 3750 rekordów)
log_returns_gold <- diff(log(dataset_gold$Zamkniecie[0:3750]), lag=1)
log_returns_wig20 <- diff(log(dataset_wig20$Zamkniecie[0:3750]), lag=1)

print("Złoto logarytmiczne stopy zwrotu")
head(log_returns_gold)
print("WIG20 logarytmiczne stopy zwrotu")
head(log_returns_wig20)

# Statystyki opisowe
summary_stats_gold <- summary(log_returns_gold)
summary_stats_wig20 <- summary(log_returns_wig20)
#odchylenie std
sd_gold <- sd(log_returns_gold)
sd_wig20 <- sd(log_returns_wig20)

print("Złoto statystyki")
print(summary_stats_gold)
print("WIG20 statystyki")
print(summary_stats_wig20)
print("Złoto odchylenie standardowe")
print(sd_gold)
print("WIG20 odchylenie standardowe")
print(sd_wig20)


# Dane szeregu czasowego logarytmicznych stóp zwrotu
dates_gold_data <- dataset_gold$Data[0:3750]
dates_gold <- as.Date(dates_gold_data[-1]) # Pomijamy pierwszą datę, ponieważ brakuje dla pierwszej różnicy
returns_data_gold <- data.frame(Date = dates_gold, Log_Return = log_returns_gold)

dataset_wig20_data <- dataset_wig20$Data[0:3750]
dates_wig20 <- as.Date(dataset_wig20_data[-1]) # Pomijamy pierwszą datę, ponieważ brakuje dla pierwszej różnicy
returns_data_wig20 <- data.frame(Date = dates_wig20, Log_Return = log_returns_wig20)

# Wykres szeregu czasowego logarytmicznych stóp zwrotu
ggplot(returns_data_gold, aes(x = Date, y = Log_Return)) +
  geom_line() +
  labs(x = "Data", y = "Logarytmiczna stopa zwrotu", title = "Szereg czasowy logarytmicznych stóp zwrotu dla złota")

ggplot(returns_data_wig20, aes(x = Date, y = Log_Return)) +
  geom_line() +
  labs(x = "Data", y = "Logarytmiczna stopa zwrotu", title = "Szereg czasowy logarytmicznych stóp zwrotu dla wig20")

#korelacja -> tutaj musi byc każdy z każdym AB AC BC
correlationGOLD_WIG20 <- cor(log_returns_gold, log_returns_wig20)
print(correlationGOLD_WIG20)

# Obliczenie skośności i kurtozy
skewness_log_returns_gold <- skewness(log_returns_gold)
kurtosis_log_returns_gold <- kurtosis(log_returns_gold)

skewness_log_returns_wig20 <- skewness(log_returns_wig20)
kurtosis_log_returns_wig20 <- kurtosis(log_returns_wig20)

# Wyświetlenie wyników
print("Skośność logarytmicznych stóp zwrotu złota:")
print(skewness_log_returns_gold)
print("Skośność logarytmicznych stóp zwrotu wig20:")
print(skewness_log_returns_wig20)

print("Kurtoza logarytmicznych stóp zwrotu złota:")
print(kurtosis_log_returns_gold)
print("Kurtoza logarytmicznych stóp zwrotu wig20:")
print(kurtosis_log_returns_wig20)

# Tworzenie histogramu
hist(log_returns_gold, main = "Histogram logarytmicznych stóp zwrotu dla złota", xlab = "Logarytmiczna stopa zwrotu", col = "lightblue", border = "black")
hist(log_returns_wig20, main = "Histogram logarytmicznych stóp zwrotu dla wig20", xlab = "Logarytmiczna stopa zwrotu", col = "lightblue", border = "black")

# Wykonanie testu Shapiro-Wilka
shapiro_test_gold <- shapiro.test(log_returns_gold)
shapiro_test_wig20 <- shapiro.test(log_returns_wig20)

# Wyświetlenie wyników testu
print("Test Shapiro-Wilka złoto:")
print(shapiro_test_gold)
print("Test Shapiro-Wilka wig20:")
print(shapiro_test_wig20)

# Sprawdzenie wartości p-wartości
print("rozkład normalny Złoto")
if (shapiro_test_gold$p.value > 0.05) {
  print("Nie ma wystarczających dowodów na odrzucenie hipotezy o normalności rozkładu.")
} else {
  print("Hipoteza o normalności rozkładu zostaje odrzucona.")
}

print("rozkład normalny Wig20")
if (shapiro_test_wig20$p.value > 0.05) {
  print("Nie ma wystarczających dowodów na odrzucenie hipotezy o normalności rozkładu.")
} else {
  print("Hipoteza o normalności rozkładu zostaje odrzucona.")
}

# Wizualizacja danych
qqnorm(log_returns_gold)
qqline(log_returns_gold)

qqnorm(log_returns_wig20)
qqline(log_returns_wig20)

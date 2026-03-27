
library(bsts)
library(readr)
library(dplyr)
library(lubridate)

path <- getwd()
gold <- read.csv(paste0(path,"/data/monthly.csv"))

gold <- gold %>%
  mutate(
    Date = paste0(Date, "-01"),  # transforme 2025-04 → 2025-04-01
    Date = as.Date(Date)
  )
  arrange(Date)

# Extract double vector
y <- gold$Price %>% as.double()
# Rendements log
r <- diff(log(y))

par(mar=c(4,4,0,0),bty='n')
plot(r, lwd=2, ylab="Unemployment claims (thousand)")

#Modelisation
ss <- AddLocalLinearTrend(list(), r)	

ss <- AddSeasonal(ss, r, nseasons = 12)
model1 <- bsts(r,state.specification = ss,niter = 1000)
pred_r <- predict(model1, horizon = 12)

# point de départ = dernier prix observé
last_price <- tail(y, 1)

# reconstruction du niveau
forecast_mean <- last_price * exp(cumsum(pred_r$mean))
forecast_lower  <- last_price * exp(cumsum(pred_r$interval[1, ]))
forecast_upper  <- last_price * exp(cumsum(pred_r$interval[2, ]))

last_date <- max(gold$Date)
future_dates <- seq(last_date %m+% months(1), by = "month", length.out = length(forecast_mean))

plot(gold$Date, y, type = "l", col = "black", lwd = 2,
     main = "Prévision du prix de l'or (BSTS sur rendements)",
     xlab = "Date", ylab = "Prix de l'or")

# Prévision
lines(future_dates, forecast_mean, col = "blue", lwd = 2)

# Intervalles
lines(future_dates, forecast_lower, col = "red", lty = 2)
lines(future_dates, forecast_upper, col = "red", lty = 2)

# Optionnel : zone d'incertitude
polygon(
  c(future_dates, rev(future_dates)),
  c(forecast_lower, rev(forecast_upper)),
  col = rgb(1, 0, 0, 0.15), border = NA
)

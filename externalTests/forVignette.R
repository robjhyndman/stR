require(graphics)
m <- decompose(co2)
plot(m)

require(forecast)
fit <- tbats(USAccDeaths)
plot(fit)
plot(forecast(fit))

taylor.fit <- tbats(taylor)
plot(taylor.fit)
plot(forecast(taylor.fit))

co2.fit <- tbats(co2)
plot(co2.fit)

require(seasonal)
co2.fit <- seas(co2)
plot(co2.fit, trend = T)
residplot(co2.fit)

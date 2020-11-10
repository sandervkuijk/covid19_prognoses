f_trend <- function(x, time = 7, span = 0.25) {

dag <- 1:length(x)
time_pred <- seq(length(x) + 1, length.out = time)

# loess
loess <- loess(x ~ dag, control = loess.control(surface = "direct"), span = span)
pred_loess <- predict(loess, data.frame(dag = time_pred), se = TRUE)[[1]]
pred_loess_se  <- predict(loess, data.frame(dag = time_pred), se = TRUE)[[2]]
pred_loess_lo <- pred_loess - pred_loess_se * qnorm(.975)
pred_loess_up <- pred_loess + pred_loess_se * qnorm(.975)

# arima
arima <- auto.arima(x)
# autoplot(forecast(arima)) 
# checkresiduals(arima)
sink("nul") 
pred_arima <- summary(forecast(arima, h = time))[[1]]
pred_arima_lo <- summary(forecast(arima, h = time))[[4]]
pred_arima_up <- summary(forecast(arima, h = time))[[5]]
sink() 

# combine upper and lower
pred_lo <- pmax(pmin(pred_loess_lo, pred_arima_lo), 0)
pred_up <- pmax(pred_loess_up, pred_arima_up)

pred <- data.frame(time = time_pred,
                   loess = pmax(0, pred_loess),
                   loess_lo = pmax(0, pred_loess_lo),
                   loess_up = pmax(0, pred_loess_up),
                   arima = pmax(0, pred_arima),
                   arima_lo = pmax(0, pred_arima_lo),
                   arima_up = pmax(0, pred_arima_up),
                   lo = pred_lo,
                   up = pred_up
)

# visualise fit
plot(x ~ dag, xlim = c(0, length(dag) + time),  ylim = c(0, max(pred[, -1], x)))
lines(loess$x, loess$fitted, col = "red")
lines(time_pred, pred_loess, col = "red", lty= 3)
lines(loess$x, arima$fitted, col = "blue")
lines(time_pred, pred_arima, col = "blue", lty= 3)
polygon(c(tail(dag, n=1), time_pred, rev(time_pred), tail(dag, n=1)), 
        c(tail(x, n=1), pred_lo, rev(pred_up), tail(x, n=1)), 
        col = adjustcolor("black", alpha.f = 0.3), border = NA)

p <- recordPlot()

return(list(plot = p, pred = pred))

}

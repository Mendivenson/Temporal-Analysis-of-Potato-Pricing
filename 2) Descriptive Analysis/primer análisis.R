# In this section the main features of the serie "Precio mensual de la papa superior en Corabastos"
# are reviewed. Further discussions about these features in the project document.

colores = RColorBrewer::brewer.pal(n = 3, 'Paired')

# ===> Setting a directory: 
dir = '~/UN/4) Series de tiempo univariadas (STU)/2) Workplace/'
setwd(dir)

# ===> Load the serie: 
load('1) Data/TimeSeries.RData')

# ===> The serie's variance does not appear to be the same for each time period:
cat('Para la serie de papa superior, el lambda que maximiza la verosimilitud es:', forecast::BoxCox.lambda(TimeSeries))
par(mfrow = c(2,1)); plot(TimeSeries); plot(log(TimeSeries))
TimeSeriesT = log(TimeSeries)

# ===> A small trend can be seen in the serie:
plot(TimeSeriesT); plot(diff(TimeSeriesT))
TimeSeriesT = diff(TimeSeriesT)

# A comparation between the original data and the transformed data
par(mfrow = c(2,1), mar = c(3, 4.1, 4, 2.1))
plot(TimeSeries, xlim = c(2014, 2024),
     main = 'Potato price (Variedad Superior)',ylab = NA, xlab = NA, col = colores[2], type = 'l',lwd = 2)
mtext(bquote(bold('Monthly average price per kilogram')), side = 3,
      col = 'gray', line = 0.35)
abline(v = seq(2013, 2024, by = 0.5), col = 'gray', lty = 'dashed')

plot(TimeSeriesT, xlim = c(2014, 2024),
     main = 'Potato price (Variedad Superior)',ylab = NA, xlab = NA, col = colores[2], type = 'l',lwd = 2)
mtext(bquote(bold('Serie transformada con log y primera diferencia regular')), side = 3,
      col = 'gray', line = 0.35)
abline(v = seq(2013, 2024, by = 0.5), col = 'gray', lty = 'dashed')

# ===> Now it's time to look at the ACF and PACF of the serie.
par(mfrow = c(2,2))
acf(TimeSeries, main = 'ACF', ylab = NA); pacf(TimeSeries, main = 'PACF', ylab = NA)
acf(TimeSeries, main = 'ACF (Transformed data)', ylab = NA); pacf(TimeSeries, main = 'PACF (Transformed data)', ylab = NA)

# ===> Let's look at the behavior of the serie each month: 
data = cbind('price' = as.vector(TimeSeriesT), 'month' = rep(month.abb, c(9, rep(10, 11))))
data = as.data.frame(data)
data$price = as.numeric(data$price)
# png('Plots/Boxplot mensual.png', width = 10, height = 7, res = 180, units = 'in')
par(mar = c(2.5,2.5,4,2), mfrow = c(1,1))
boxplot(data$price~ data$month, col = colores[1], border = colores[2], axes = F, ylab = NA,  xlab = NA,
        main = 'Boxplot mensual serie precio de la papa superior', cex.main = 2, lwd = 2, lty = 'solid')
box(bty = 'l', lwd = 2)
abline(v = 1:12, lwd = 2, col = adjustcolor('gray',0.5), lty = 'dashed')
boxplot(data$price~ data$month, col = colores[1], border = colores[2], add = T, axes = F, lty = 'solid')

axis(1, lwd = 2, labels = month.abb, at = 1:12, font = 2)
axis(2, lwd = 2, font = 2)
# dev.off()

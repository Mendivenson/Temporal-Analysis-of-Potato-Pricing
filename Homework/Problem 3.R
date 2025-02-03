s1 = arima.sim(list(ma = c(0.9), 
                    ar = c(-0.9)), 
               n = 500)

s2 = arima.sim(n = 500,
               list(ar = c(0.2, 0.55)),
               rand.gen = function(n, ...) rnorm(n = n, sd = sqrt(2.25)))
# plot(s2); acf(s2); pacf(s2)


s3 = arima.sim(n = 500,
               list(ma = c(0.9, -0.8, -0.8)),
               rand.gen = function(n, ...) rnorm(n = n, sd = 3))
plot(s3); acf(s3); pacf(s3)

par(mfrow = c(3,3),
    mar = c(5.6,4,4,2))
name = c('Serie ARMA(1,1)',
         'Serie AR(2)',
         'Serie MA(3)')
for (i in 1:3){
  s = get(paste0('s', i))
  plot(s, main = '', xlab = 'Time', ylab = 'Serie')
  mtext(bquote(bold('Serie\'s plot')), side = 1, cex = 0.85,
        line = 4.5)
  acf(s, main = '')
  title(main = name[i], cex.main = 2)
  mtext(bquote(bold('Autocorrelation Function')), side = 1, cex = 0.85,
        line = 4.5)
  pacf(s, main = '')
  mtext(bquote(bold('Parcial Autocorrelation Function')), side = 1, cex = 0.85,
        line = 4.5)
}

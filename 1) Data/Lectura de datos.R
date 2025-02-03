# The data was obtained from the open data website of DANE and have been collected
# from 2013 to 2023 under the framework of SIPSA: the Price and Supply Information 
# System for the Food and Agricultural Sector. They represent the average price per
# kilogram reported by wholesalers at the main collection points across the country, 
# and can be accessed at the following link: https://microdatos.dane.gov.co/index.php/catalog/776/get-microdata.

# Each dataset contains information on 5 variables:
#   - Fecha: Date, The month corresponding to the information
#   - Grupo: Group, General category of the product
#   - Producto: Product name
#   - Fuente: Which wholesale center provided the information?
#   - Precio: Monthly average price per kilogram of the product

library(dplyr)

# =====> Setting a fixed directory
dir = '~/UN/4) Series de tiempo univariadas (STU)/2) Workplace/Data'
setwd(dir)

# =====> Reading the data
# The datasets are encoded in Latin1 format for unknown reasons and have some 
# extra ;;;; at the end of each line (That's why only columns 1 to 5 are selected)

data13 = read.csv2(file = 'Files/Mensual_2013_2017.csv', header = TRUE, sep = ';', 
                   fileEncoding = 'latin1', blank.lines.skip = TRUE)[,1:5]
data18 = read.csv2(file = 'Files/Mensual_2018.csv', header = TRUE, sep = ';', 
                   fileEncoding = 'latin1', blank.lines.skip = TRUE)[,1:5]
data19 = read.csv2(file = 'Files/Mensual_2019.csv', header = TRUE, sep = ';', 
                   fileEncoding = 'latin1', blank.lines.skip = TRUE)[,1:5]
data20 = read.csv2(file = 'Files/Mensual_2020.csv', header = TRUE, sep = ';', 
                   fileEncoding = 'latin1', blank.lines.skip = TRUE)[,1:5]
data21 = read.csv2(file = 'Files/Mensual_2021.csv', header = TRUE, sep = ';', 
                   fileEncoding = 'latin1', blank.lines.skip = TRUE)[,1:5]
data22 = read.csv2(file = 'Files/mensual 22.csv', header = TRUE, sep = ';', 
                   fileEncoding = 'latin1', blank.lines.skip = TRUE)[,1:5]
data23 = read.csv2(file = 'Files/mensual 23.csv', header = TRUE, sep = ';', 
                   fileEncoding = 'latin1', blank.lines.skip = TRUE)[,1:5]


# =====> Rowbind all the datasets 
# Since each dataset has the same columns, it’s straightforward to row-bind 
# them all to have the data in a single dataframe.
dataset = list(data13, data18, data19, data20, data21, data22, data23)
data = NA
for (i in dataset){
  colnames(i) = colnames(data13)       # Unfortunately, not all datasets have the same column names
  data = rbind(data, i)
}
rm(list = setdiff(ls(), 'data'))

# =====>  Cleaning the data
# The price is formatted with a period as the thousand separator, so we need to remove
# these periods before converting the values to numeric; our main interest is in potato prices.
library(dplyr)
data = data %>% 
  filter(stringr::str_detect(Producto, "Papa ") & Fuente == "Bogotá, D.C., Corabastos") %>% 
  mutate(Precio = as.integer(gsub(pattern = '\\.', x = Precio, replacement = ''))) %>% 
  select(-Grupo) # The Group variable in no longer needed

# =====> Saving the time series
# Since there are multiple kinds of potatoes, all time series will be
# stored in a multivariate ts object for easier plotting
TimeSeries <- data |> 
  filter(Producto == unique(Producto)[1]) |> 
  select(Fecha, Precio) 
colnames(TimeSeries) <- c('Fecha', as.character(unique(data$Producto)[1]))

for (i in unique(data$Producto)[-1]) {
  # Filtra el producto actual y renombra la columna de precios
  product_data <- data |> 
    filter(Producto == i) |> 
    select(Fecha, Precio)
  
  colnames(product_data) <- c('Fecha', as.character(i))
  
  # Une los datos con TimeSeries usando full_join para mantener todas las fechas
  TimeSeries <- TimeSeries |> 
    full_join(product_data, by = 'Fecha')
}
TimeSeries = ts(TimeSeries[,-1], start = 2013, freq = 12)

# str(TimeSeries)
# After filtering to include only information from Corabastos, some series are left
# with less data than expected. These series are: Papa R-12 negra, Papa R-12 roja,
# Papa suprema, Papa rubí, and Papa Betina (this one has only a single observation).


# ===> PLotting all the series
# png('Plots/allSeries.png', width = 15, height = 7.5, units = 'in', res = 250)
par(mar = c(3, 4.1, 4.1, 2.1))
colores <- RColorBrewer::brewer.pal(n = 11, name = 'Paired')
plot(TimeSeries, col = colores, plot.type = 'single', ylab = 'Colombian pesos', xlab = NA, type = "n",
     bty = 'n', axes = F, font.lab = 2)
title(main = 'Potato price', cex.main = 3, line = 2)
axis(1, lwd = 2, font = 2); axis(2, lwd = 2, font = 2); box(bty = 'l', lwd = 2)
abline(v = 2013:2024, col = 'gray', lty = 'dashed')                       # Reference lines
abline(h = seq(0, 5000, by = 500), col = 'gray', lty = 'dashed')

for (i in 1:ncol(TimeSeries)) {
  lines(TimeSeries[, i], col = colores[i], lwd = 1.5)                     # Plotting all the series
}

legend(x = 'topleft', x.intersp = 0.5, y.intersp = 1,
       # Position, space between legend items in x and y
       bty = 'n', col = colores, lty = 'solid', text.font = 2,
       # Remove legend box, set colors and line type
       ncol = 4, cex = 0.8, legend = colnames(TimeSeries))

mtext(bquote(bold('Monthly average price per kilogram')), side = 3,
      col = 'gray', line = 0.35)
mtext('Prices reported at Corabastos (Bogotá D.C.)', side = 1,
      line = -1.1, cex = 0.8, adj = 0.99, col = 'darkgray')
# dev.off()

# =====> Selected serie
# The project is going to be about Papa Superior because it is mainly produced in Cundinamarca.

TimeSeries = TimeSeries[,c('Papa superior')]
TimeSeries = TimeSeries[!is.na(TimeSeries)]
TimeSeries = ts(TimeSeries, start = 2014, freq = 12)
par(mar = c(3, 4.1, 4.1, 2.1))
plot(x = NA, y = NA, xlim = c(2014, 2024), 
     ylim = c(min(sapply(TimeSeries, min, na.rm = TRUE)) - 50, 
              max(sapply(TimeSeries, max, na.rm = TRUE)) + 50),
     main = 'Potato Price (Variedad superior)', xlab = NA, ylab = 'Colombian pesos')
mtext(bquote(bold('Monthly average price per kilogram')), side = 3, col = 'gray', line = 0.35)
mtext('Prices reported at Corabastos (Bogotá D.C.)', side = 1, line = -1.1, cex = 0.8, adj = 0.99, col = 'azure4')
abline(v = seq(2014, 2024, by = 0.5), col = adjustcolor('gray',0.5), lty = 'dashed')
abline(h = seq(0, 4500, by = 250), col = adjustcolor('gray',0.5) , lty = 'dashed')

colors = RColorBrewer::brewer.pal(n = 3, name = 'Dark2')
lines(TimeSeries, col = colors[1])

# ===> Save the selected time serie.
save(TimeSeries, file = 'TimeSeries.RData')
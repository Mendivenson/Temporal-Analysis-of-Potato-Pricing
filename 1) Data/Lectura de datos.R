# The data were obtained from the open data website of DANE and have been collected
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


# =====> Setting a fixed directory
dir = 'UN/4) Series de tiempo univariadas (STU)/2) Workplace/Data'
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
  select(-Grupo, -Fecha) # The Group and Date variables are no longer needed

# =====> Saving the time series
# Since there are multiple kinds of potatoes, all time series will be
# stored in a list for easier plotting.
TimeSeries = list()
Products = unique(data$Producto)
for (i in 1:length(Products)){
  Product = Products[i]
  TimeSeries[[Product]] = ts(data$Precio[data$Producto == Product], start = c(2013,1), frequency = 12)
}

# str(TimeSeries)
# After filtering to include only information from Corabastos, some series are left
# with less data than expected. These series are: Papa R-12 negra, Papa R-12 roja,
# Papa suprema, Papa rubí, and Papa Betina (this one has only a single observation).


# =====> Plotting all the series

# A blank plot
par(mar = c(3, 4.1, 4.1, 2.1))
plot(x = NA, y = NA, xlim = c(2013, 2024),
     ylim = c(min(sapply(TimeSeries, min, na.rm = TRUE)) - 50, 
              max(sapply(TimeSeries, max, na.rm = TRUE)) + 50),
     main = 'Potato Prices', xlab = NA, ylab = 'Colombian Pesos')

# Additional information
mtext(bquote(bold('Monthly average price per kilogram')), side = 3,
      col = 'gray', line = 0.35)
mtext('Prices reported at Corabastos (Bogotá D.C.)', side = 1,
      line = -1.1, cex = 0.8, adj = 0.99, col = 'darkgray')

# Reference lines
abline(v = seq(2013, 2024, by = 0.5), col = 'gray', lty = 'dashed')
abline(h = seq(0, 5000, by = 500), col = 'gray', lty = 'dashed')

# In total there are 11 different series
colors <- RColorBrewer::brewer.pal(n = 11, name = 'Paired')

# Plot each time series
for (i in 1:length(Products)) {
  Product = Products[i]
  lines(x = TimeSeries[[Product]], col = colors[i], type = 'l')
}

# Legend showing which line represents each potato type
legend(x = 'topleft', x.intersp = 0.5, y.intersp = 1,
       # Position, space between legend items in x and y
       bty = 'n', col = colors, lty = 'solid',
       # Remove legend box, set colors and line type
       ncol = 4, cex = 0.8, legend = Products)
       # Number of columns in the legend, legend labels
# This plot is saved as a pdf called All series

# =====> Selected Series
# Currently, I'm planning to analyze three series:
#   - Papa única
#   - Papa sabanera
#   - Papa parda pastusa
# Mainly because these are the series with complete data.

Products = c('Papa única', 'Papa sabanera', 'Papa parda pastusa')
TimeSeries = TimeSeries[Products]

par(mar = c(3, 4.1, 4.1, 2.1))
plot(x = NA, y = NA, xlim = c(2013, 2024), 
     ylim = c(min(sapply(TimeSeries, min, na.rm = TRUE)) - 50, 
              max(sapply(TimeSeries, max, na.rm = TRUE)) + 50),
     main = 'Potato Prices', xlab = NA, ylab = 'Colombian pesos')
mtext(bquote(bold('Monthly average price per kilogram')), side = 3, col = 'gray', line = 0.35)
mtext('Prices reported at Corabastos (Bogotá D.C.)', side = 1, line = -1.1, cex = 0.8, adj = 0.99, col = 'azure4')
abline(v = seq(2013, 2024, by = 0.5), col = 'gray', lty = 'dashed')
abline(h = seq(0, 4500, by = 250), col = 'gray', lty = 'dashed')

colors = RColorBrewer::brewer.pal(n = 3, name = 'Dark2')
for (i in 1:length(Products)) {
  Product = Products[i]
  lines(x = TimeSeries[[Product]], col = colors[i], type = 'l')
}
legend(x = 'topleft', x.intersp = 0.5, y.intersp = 1,
       col = colors, lty = 'solid', cex = 0.9, legend = Products,
       bg = rgb(142,229,255, alpha = 100, maxColorValue = 255),
       box.col = 'white', inset = 0.03)
# This plot is saved as a pdf called Selected series

save(TimeSeries, file = 'TimeSeries.RData')

# Potato Price Time Series Analysis

This repository contains the analysis of potato prices in Colombia over time. The primary focus is on tracking the monthly average prices of different types of potatoes, sourced from wholesalers at the Corabastos market in Bogotá, D.C. The data spans from 2013 to 2023 and is part of the SIPSA (Sistema de Información de Precios y Abastecimiento del Sector Agropecuario de Alimentos) project.

## Dataset

The data used in this project was obtained from the [DANE (Departamento Administrativo Nacional de Estadística)](https://microdatos.dane.gov.co/index.php/catalog/776/get-microdata) open data portal. The datasets contain price information collected monthly from the main wholesale centers in Colombia, specifically Corabastos in Bogotá, D.C.

The data includes the following variables:
- **Date**: The month to which the information corresponds.
- **Group**: The general category of the product.
- **Product**: The specific type of potato.
- **Source**: The wholesale center providing the data.
- **Price**: The average price per kilogram for the given product.

![Potato Price Trend](Plots/All%20series.pdf)

> In the open data portal, the data is divided by year, but it has been consolidated into a single dataset to create the time series. Some filtering was done too to focus on the Corabastos info and just in the potato prices. 


## Roadmap
- [x] Get the data and select the main time series to work with.
- [ ] Perform descriptive analysis (Currently in progress).

## Authors
- [Michel Mendivenson Barragán Zabala](mailto:mbarraganz@unal.edu.co)


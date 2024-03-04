## Authors
[Alessio Chiodin](https://www.linkedin.com/in/alessio-chiodin/)\
[Paolo Maranzano, Phd](https://www.paolomaranzano.net/home)

## Motivation
The motivation behind this shiny app is to provide and display monthly fuel sales trends in Italy at the provincial level, along with a set of socioeconomic variables that may help explain fuel usage. The goal of such tool is to help researchers and policy makers gain insight on local transport and mobility, as well as suggest solutions to sustainability issues. 

## Data
Data for the fuel usage application is sourced from 
- the oil bulletin of the Italian [Ministry of Environment and Energy Security](https://dgsaie.mise.gov.it/bollettino_petrolifero.php);
- ISTAT's database [IstatData](https://esploradati.istat.it/databrowser/#/it/dw/categories)
- Bank of Italy's [Statistical Database](https://www.bancaditalia.it/statistiche/basi-dati/bds/index.html?com.dotmarketing.htmlpage.language=1)
- Eurostat's [database](https://ec.europa.eu/eurostat/web/main/data/database)

Detailed links to the original data are available in the 'Metadata' section under 'Data Explorer'. 
The processed dataset and the metadata are available for download on the shiny app and my [github](https://github.com/ale-ch).

##### Spatial aggregation
The regions were aggregated according to Eurostat's NUTS 2021 classification (Nomenclature of territorial units for statistics). Eurostat defines three levels of aggregation:
- NUTS 1: major socio-economic regions
- NUTS 2: basic regions for the application of regional policies
- NUTS 3: small regions for specific diagnoses

## Limitations
The main limitation of this application is data availability. The explanatory variables were collected at different levels of granularity over space and time, some of them were discontinued or not collected at certain periods (such as during the covid lockdown), or at certain locations. 

If any of the plots show errors, please refer to the 'Metadata' section and change the desidered time range or location accordingly. 

A 2017 [regional reform in Sardinia](https://web.archive.org/web/20160509123345/http://www.regione.sardegna.it/j/v/25?s=306981&v=2&c=348&t=1) forced a remapping of pre-2017 provinces, making the data less reliable in the 2015-2017 period. 

## Updates
Fuel consumption data will be updated periodically, whereas the other variables will be discontinued.

## Link
The app can be found [here](https://ale-ch.shinyapps.io/it-fuel-dashboard/)

## Contacts
[alessiochiodin@hotmail.com](mailto:alessiochiodin@hotmail.com)\
[paolo.maranzano@unimib.it](mailto:paolo.maranzano@unimib.it)

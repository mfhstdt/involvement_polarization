# Involvement as a polarizing factor? 

This repository contains code to reproduce all analyses reported in "Involvement as a polarizing factor? – A comprehensive multi-method analysis across representative datasets"

### Data 

This folder contains processed datasets (`Data/processed_data/`) and scripts to prepare those datasets from raw data (`Data/data_preparation/`). Raw datasets can be found at: 

- *LISS Panel*: https://www.dataarchive.lissdata.nl/study-units/view/1257
- *American National Election Study*: https://electionstudies.org/data-center/2020-time-series-study/
- *European Value Study & World Value Survey*: https://europeanvaluesstudy.eu/methodology-data-documentation/survey-2017/joint-evs-wvs/
- *Eurobarometer*: https://doi.org/10.4232/1.14101
- *Dataset on attitudes towards COVID-19 vaccines*: https://osf.io/357h4/

### Helper Functions

Functions for computations used in the main analyses

### Analyses

Contains subfolder on longitundinal analyses (the panel data regression model) and on cross-sectional analyses which includes fitting data to the Cusp model, measures of spread and dispersion and on modality detection (applying our method to the datasets). 

Additioanlly, the subfolder `Appendix_Analyses` contains robustness checks for the LISS panel, code for confirmatory factor analyses of all scales, and an investigation into the heterogenity of results when analyzing the EVS/WVS data per country. 

The `Additional_checks` subfolder contains exploratory checks reported in the main manuscript, namely results for different involvement items in the COVID-19 dataset and an investigation into party identification and general political orientation in the LISS panel.

### Simulation on Detection Methods

This folder contains code to replicate our simulation study (see Appendix B) which assesses the performance of various mode detection methods on simulated distributions. In `modality_simulation1` distributions are simulated, the `modality_simulation2` script defines functions to apply the modality detection methods and in `modality_simulation3` we apply the methods to the simulated distributions and assess their performance. 

### References

Centerdata. (2021). Longitudinal internet studies for the social sciences, core panel [dataset and documentation]. www.lissdata.nl/

American National Election Studies. (2021). Anes 2020 time series study full release [dataset and documentation]. www.electionstudies.org

European Commission. (2022). Eurobarometer 97.1 [dataset and documentation.GESIS, Cologne. ZA7886 Data file Version 2.0.0]. https://doi.org/10.4232/1.14101

European Value Systems Study Group & World Values Survey Association. (2021). European values study and world values survey: Joint evs/wvs 2017-2022 dataset [dataset and documentation]. https://europeanvaluesstudy.eu/methodology-data-documentation/survey-2017/joint-evs-wvs/

Chambon, M., Kammeraad, W. G., van Harreveld, F., Dalege, J., Elberse, J. E., & van der Maas, H. L. J. (2022). Understanding change in COVID-19 vaccination intention with  network analysis of longitudinal data from dutch adults. npj Vaccines, 7 (1), 114

# Lake-Angre
The code for the article 'What kind of the environmental heterogeneity could support the parallelism in sympatric diversification?'
DOI would be added asap

- The Lake-Angre1.R file contains the code for models 1.1 - 1.6
- The Lake-Angre2.R file contains the code for models 2.1 - 2.6
- Files modify_date.R and modify_date_up.R contain custom function which are used in the main code

**Model 1 description** 
From LARC, using the [pynasapower](https://github.com/alekfal/pynasapower) package for Python, we downloaded the following data for at 55.5151 157.6796:
1) PS - Surface Pressure (kPa)
2) TS - Earth Skin Temperature (C)
3) T2M - Temperature at 2 Meters (C)
4) QV2M - Specific Humidity at 2 Meters (g/kg)
5) RH2M - Relative Humidity at 2 Meters (%)
6) WD2M - Wind Direction at 2 Meters (Degrees)
7) WS2M - Wind Speed at 2 Meters (m/s)
8) WD10M - Wind Direction at 10 Meters (Degrees)
9) WS10M - Wind Speed at 10 Meters (m/s)
10) GWETTOP - Surface Soil Wetness (1)
11) GWETROOT - Root Zone Soil Wetness (1)
12) CLOUD_AMT - Cloud Amount (%)
13) PRECTOTCORR - Precipitation Corrected (mm/day)
14) PRECTOTCORR_SUM - Precipitation Corrected Sum (mm)
15) ALLSKY_SFC_LW_DWN - All Sky Surface Longwave Downward Irradiance (W/m2)
16) ALLSKY_SFC_SW_DWN - All Sky Surface Shortwave Downward Irradiance (MJ/m2/day)

The model also included a modified date. To do so we calculated the time difference in days from 1989.01.24 (first lake area measurement date) for each data point (n). Modified date n' was calculated as

n'=|sin(365.25n)|
 
**Model 1.1**
so, there were initially 17 predictors. To account for the possible time lag, another 16 predictors were added, which represented LARC data with a 1-month back shift. Larger time shifts were not taken into account in the model, since with the addition of a larger number of parameters, the task of comparing models using AIC becomes too computationally complicated. Variables with a time shift are designated by the suffix _shift1.
We also added precipitation data for each month from November to May as predictors, since the amount of snow in winter can affect the amount of water in the lake. 
Thus, there were 40 predictors in total. Next, we estimated the correlation coefficients of the predictors with the lake area and selected those for which the coefficient was greater than 0.35. The remaining parameters were:
1) RH
2) TS
3) T2M
4) QV2M
5) WD2M
6) GWETTOP
7) GWETROOT
8) PRECTOTCORR
9) PRECTOTCORR_SUM
10) ALLSKY_SFC_LW_DWN
11) PS_shift1
12) TS_shift1
13) T2M_shift1
14) QV2M_shift1
15) RH2M_shift1
16) WD2M_shift1
17) WS2M_shift1
18) WD10M_shift1
19) WS10M_shift1
20) PRECTOTCORR_shift1
21) PRECTOTCORR_SUM_shift1
22) ALLSKY_SFC_LW_DWN_shift1
23) n'
24) dec_prec (precipitation in December)
We then found predictors that were correlated with each other such that |R| was larger than 0.95, and retained only one predictor from each group of correlated variables (**fig. S1**).

![image](https://github.com/user-attachments/assets/f5395971-d4d9-4b5b-8458-7490306f0164)
_Fig. S1. The correlation matrix between predictors, presented as a heat map. The numbers on the axes correspond to the numbers in the list above._

As a result, the following set of parameters remained:
1) P.S.
2) TS (correlated with T2M, QV2M and ALLSKY_SFC_LW_DWN)
3) WD2M
4) GWETTOP (correlated with GWETROOT)
5) PRECTOTCORR
6) PRECTOTCORR_SUM
7) PS_shift1
8) TS_shift1(correlated with T2M_shift1, QV2M_shift1 and ALLSKY_SFC_LW_DWN_shift1)
9) RH2M_shift1
10) WD2m_shift1
11) WS2M_shift1 (correlated with WS10M_shift1)
12) WD10M_shift1
13) PRECTOTCORR_shift1
14) PRECTOTCORR_SUM_shift1
15) n'
16) dec_prec

All these predictors were included in the model without interactions. The resultant model structure was:
**area = β + ɑ1⋅n' + ɑ2⋅TS + ɑ3⋅GWETTOP + ɑ4⋅PS_shift1 + ɑ5⋅TS_shift1 + ɑ6⋅RH2M_shift1 + ɑ7⋅WS2M_shift1 + ɑ8⋅PRECTOTCORR_SUM_shift1**
The effects of the parameters TS, GWETTOP and PRECTOTCORR_SUM_shift1 were significant. The adjusted R2 of the model was 0.54, AIC = 1887 (**fig. S2**).

![image](https://github.com/user-attachments/assets/76a70b1c-3022-407a-b0d4-f60a9574f53b)
_Fig. S2. Prediction of the temporal dynamics of the area using model 1.1. Red – real data, blue – model prediction_

Using the dredge function of the [MuMin](https://cran.r-project.org/web/packages/MuMIn/index.html) package, we estimated the AIC for models in which all possible combinations of predictors from model 1.1 were used as predictors. The model with the lowest AIC was called model 1.2.

**Model 1.2**
Model 1.2 had the next form:
**area = β + ɑ1⋅dec_prec + ɑ2⋅GWETTOP + ɑ3⋅PRECTOTCORR + ɑ4⋅PRECTOTCORR_SUM_shift1 + ɑ5⋅TS + ɑ6⋅TS_shift1**
For this model, we obtained adjusted R2 = 0.58, AIC = 1870. The effects were significant for  GWETTOP, PRECTOTCORR_SUM_shift1, TS and  dec_prec (**fig. S3**)

![image](https://github.com/user-attachments/assets/5cfca6e5-2398-4f9f-9131-379af2cf0b94)
_Fig. S3. Prediction of the temporal dynamics of the lake area using model 1.2. Red – real data, blue – model prediction_

**Model 1.3**
In model 1.3, we included all possible interactions of the predictors from model 2. The resulting model had an adjusted R2 = 0.61 and an AIC = 1877. The effects of dec_prec ⋅ PRECTOTCORR, dec_prec ⋅ PRECTOTCORR_SUM_shift1, dec_prec ⋅ TS, dec_prec ⋅ TS_shift1, and intercept were significant in this model (**fig. S4**).
Next, using the dredge function of the MuMin package, we selected a combination of predictors, taking into account their interactions, that yielded the lowest AIC of the model. The selected model was assigned model 1.4.

![image](https://github.com/user-attachments/assets/8698a8bb-e8a6-4aa9-a743-a8142301fe56)
_Fig. S4. Prediction of the temporal dynamics of the lake area using model 1.3. Red – real data, blue – model prediction_

**Model 1.4** 
Model 1.4 structure was as follows: 
**area = β + ɑ1⋅dec_prec + ɑ2⋅GWETTOP + ɑ3⋅PRECTOTCORR + ɑ4⋅PRECTOTCORR_SUM_shift1 + ɑ5⋅TS + ɑ6⋅dec_prec:PRECTOTCORR + ɑ7⋅PRECTOTCORR:TS**

The adjusted R2 for this model was 0.61, AIC = 1867 (**fig. S5**), with significant effects of GWETTOP, PRECTOTCORR_SUM_shift1, TS and intercept. The effects of dec_prec:PRECTOTCORR and PRECTOTCORR:TS were close to significance (p = 0.08 in both cases).

![image](https://github.com/user-attachments/assets/bfee3fcd-314a-4af1-8f29-66ab305c4d0a)
_Fig. S5. Prediction of the temporal dynamics of the lake area using model 1.4. Red – real data, blue – model prediction_

**Model 1.5**
Model 1.5 included only the significant effects from Model 1.4. Thus, Model 1.5 had the form
**area = β + ɑ1⋅GWETTOP + ɑ2⋅PRECTOTCORR_SUM_shift1 + ɑ3⋅TS + ɑ4⋅dec_prec:PRECTOTCORR + ɑ5⋅PRECTOTCORR:TS.**
For this model we obtained adjusted R2 = 0.61, AIC = 1863 (**fig. S6**).

![image](https://github.com/user-attachments/assets/a5d59bd1-3780-40d9-86f4-e8dc0619a432)
_Fig. S6. Prediction of the temporal dynamics of the lake area using model 1.5. Red – real data, blue – model prediction_

**Model 1.6**
Finally, we tested a model including only the significant effects of model 1.2. This model was in form:
**area = β + ɑ1 ⋅ GWETTOP + ɑ2 ⋅ PRECTOTCORR_SUM_shift1 + ɑ 3 ⋅ TS + ɑ4 ⋅ dec_prec.** 
For this model, we obtained adjusted R2 = 0.58 and AIC = 1869 (fig. S7). Thus, the best of the tested models is model 1.5.

![image](https://github.com/user-attachments/assets/25e70c53-fb3e-4db6-afa9-845454aa4114)
_Fig. S7. Prediction of the temporal dynamics of the lake area using model 1.5. Red – real data, blue – model prediction_

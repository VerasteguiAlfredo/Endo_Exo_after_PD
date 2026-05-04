### Figure 1. Figure 1. HbA1c trajectory following pancreatectomy stratified by new-onset diabetes status.

=== DM group counts ===

New-Onset DM        No DM 
         154          587 


==============================================================================
Table 6. HbA1c Trajectory by New-Onset Diabetes Status
============================================================================== 


|Time Point    |  N| New-Onset DM
Median HbA1c, % (IQR) |   N| No DM
Median HbA1c, % (IQR) | P-Value¹ |
|:-------------|--:|:---------------------------------:|---:|:--------------------------:|:--------:|
|Pre-operative | 60|           5.8 (5.4–6.2)           | 162|       5.5 (5.2–5.9)        |  <0.001  |
|Year 1        | 61|           6.3 (6.0–6.9)           |  98|       5.5 (5.2–5.9)        |  <0.001  |
|Year 2        | 41|           6.8 (6.0–8.0)           |  46|       5.6 (5.4–6.0)        |  <0.001  |
|Year 3        | 45|           6.7 (6.1–7.6)           |  40|       5.6 (5.5–5.9)        |  <0.001  |
|Year 4        | 45|           6.9 (6.2–7.6)           |  35|       5.7 (5.4–5.9)        |  <0.001  |
|Year 5        | 41|           6.7 (6.2–7.6)           |  37|       5.6 (5.2–5.9)        |  <0.001  |
|Year 6        | 38|           6.7 (6.1–8.0)           |  24|       5.8 (5.4–5.9)        |  <0.001  |
|Year 7        | 30|           7.0 (6.2–7.6)           |  19|       5.7 (5.6–5.9)        |  <0.001  |
|Year 8        | 25|           7.0 (6.1–9.0)           |  20|       5.6 (5.2–5.8)        |  <0.001  |
|Year 9        | 22|           7.2 (6.4–8.1)           |  17|       5.9 (5.6–6.1)        |  <0.001  |
|Year 10       | 19|           7.3 (6.0–8.6)           |  14|       5.8 (5.7–6.2)        |  0.005   |

✓ Table 6 exported to:
   C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Tables/Table6_HbA1c_Trajectory.html 

=== HbA1c trajectory summary ===
Time Point       N(DM) DM Median (IQR)        N(No DM) No DM Median (IQR)        P-Value
------------------------------------------------------------------------------------- 
Pre-operative       60 5.8 (5.4–6.2)           162 5.5 (5.2–5.9)            <0.001
Year 1              61 6.3 (6.0–6.9)            98 5.5 (5.2–5.9)            <0.001
Year 2              41 6.8 (6.0–8.0)            46 5.6 (5.4–6.0)            <0.001
Year 3              45 6.7 (6.1–7.6)            40 5.6 (5.5–5.9)            <0.001
Year 4              45 6.9 (6.2–7.6)            35 5.7 (5.4–5.9)            <0.001
Year 5              41 6.7 (6.2–7.6)            37 5.6 (5.2–5.9)            <0.001
Year 6              38 6.7 (6.1–8.0)            24 5.8 (5.4–5.9)            <0.001
Year 7              30 7.0 (6.2–7.6)            19 5.7 (5.6–5.9)            <0.001
Year 8              25 7.0 (6.1–9.0)            20 5.6 (5.2–5.8)            <0.001
Year 9              22 7.2 (6.4–8.1)            17 5.9 (5.6–6.1)            <0.001
Year 10             19 7.3 (6.0–8.6)            14 5.8 (5.7–6.2)             0.005


### Figure 2. Estimated HbA1c Trajectories After Pancreatectomy From Linear Mixed‑Effects Models

=== Long format ===
Observations: 700 
Patients: 306 

=== Model 1: Unadjusted, random intercept + slope ===
N obs: 700 | N pts: 306 
Linear mixed model fit by REML. t-tests use Satterthwaite's method [
lmerModLmerTest]
Formula: hba1c ~ time_c * dm_hba1c_group + (1 + time_c | mayo_id)
   Data: hba1c_long
Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))

REML criterion at convergence: 2940.2

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.5311 -0.2042 -0.0197  0.1027  5.8345 

Random effects:
 Groups   Name        Variance Std.Dev. Corr  
 mayo_id  (Intercept) 26.0329  5.1022         
          time_c       0.8667  0.9310   -0.98 
 Residual              0.8288  0.9104         
Number of obs: 700, groups:  mayo_id, 306

Fixed effects:
                                           Estimate Std. Error       df t value
(Intercept)                                  7.3207     0.7679 290.0014   9.534
time_c                                      -0.2892     0.1541 116.9398  -1.876
dm_hba1c_groupNo DM — Prediabetes           -1.4106     1.3801 300.6382  -1.022
dm_hba1c_groupNo DM — HbA1c unknown         -1.7682     0.9125 301.1461  -1.938
dm_hba1c_groupNew-Onset DM                  -0.6599     0.9213 294.4130  -0.716
time_c:dm_hba1c_groupNo DM — Prediabetes     0.2735     0.2688 123.4017   1.017
time_c:dm_hba1c_groupNo DM — HbA1c unknown   0.3000     0.1818 117.6525   1.650
time_c:dm_hba1c_groupNew-Onset DM            0.4076     0.1812 114.6018   2.250
                                           Pr(>|t|)    
(Intercept)                                  <2e-16 ***
time_c                                       0.0631 .  
dm_hba1c_groupNo DM — Prediabetes            0.3076    
dm_hba1c_groupNo DM — HbA1c unknown          0.0536 .  
dm_hba1c_groupNew-Onset DM                   0.4744    
time_c:dm_hba1c_groupNo DM — Prediabetes     0.3110    
time_c:dm_hba1c_groupNo DM — HbA1c unknown   0.1016    
time_c:dm_hba1c_groupNew-Onset DM            0.0264 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
            (Intr) time_c d_1D—P d_D—Hu d_1_ND t_:D—P t_D—Hu
time_c      -0.914                                          
dm_h1_NDM—P -0.556  0.509                                   
d_1_NDM—HAu -0.841  0.769  0.468                            
dm_h1_N-ODM -0.833  0.762  0.464  0.701                     
t_:_1_NDM—P  0.524 -0.573 -0.924 -0.441 -0.437              
t_:_1_ND—Hu  0.775 -0.848 -0.431 -0.919 -0.646  0.486       
t_:_1_N-ODM  0.778 -0.851 -0.433 -0.654 -0.927  0.488  0.721

=== Model 2A: Adjusted full cohort (no pre-op HbA1c) ===
N obs: 696 | N pts: 305 
Linear mixed model fit by REML. t-tests use Satterthwaite's method [
lmerModLmerTest]
Formula: hba1c ~ time_c * dm_hba1c_group + age_at_surgery + bmi_cat +  
    distal_resection + nat_binary + adenocarcinoma + tobacco_ever +  
    (1 | mayo_id)
   Data: hba1c_long
Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))

REML criterion at convergence: 3135.2

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.9857 -0.1703 -0.0181  0.1051  5.1446 

Random effects:
 Groups   Name        Variance Std.Dev.
 mayo_id  (Intercept) 22.748   4.770   
 Residual              1.098   1.048   
Number of obs: 696, groups:  mayo_id, 305

Fixed effects:
                                             Estimate Std. Error         df
(Intercept)                                 6.072e+00  1.582e+00  2.866e+02
time_c                                     -9.003e-04  5.989e-02  3.941e+02
dm_hba1c_groupNo DM — Prediabetes          -1.774e+00  1.302e+00  3.096e+02
dm_hba1c_groupNo DM — HbA1c unknown        -1.609e+00  8.675e-01  3.052e+02
dm_hba1c_groupNew-Onset DM                 -7.405e-01  8.906e-01  2.936e+02
age_at_surgery                              1.046e-02  2.291e-02  2.847e+02
bmi_catUnderweight                          5.638e-01  3.527e+00  2.932e+02
bmi_catOverweight                           3.173e-01  6.544e-01  2.855e+02
bmi_catObese                                1.245e+00  7.548e-01  2.846e+02
distal_resection                            1.346e-01  6.329e-01  2.850e+02
nat_binary                                 -4.416e-01  8.076e-01  2.866e+02
adenocarcinoma                              4.883e-01  7.177e-01  2.852e+02
tobacco_ever                               -2.878e-01  5.748e-01  2.852e+02
time_c:dm_hba1c_groupNo DM — Prediabetes    2.157e-02  1.377e-01  4.341e+02
time_c:dm_hba1c_groupNo DM — HbA1c unknown  2.216e-02  7.805e-02  4.167e+02
time_c:dm_hba1c_groupNew-Onset DM           1.318e-01  6.641e-02  3.943e+02
                                           t value Pr(>|t|)    
(Intercept)                                  3.837 0.000153 ***
time_c                                      -0.015 0.988014    
dm_hba1c_groupNo DM — Prediabetes           -1.362 0.174081    
dm_hba1c_groupNo DM — HbA1c unknown         -1.855 0.064541 .  
dm_hba1c_groupNew-Onset DM                  -0.831 0.406404    
age_at_surgery                               0.457 0.648348    
bmi_catUnderweight                           0.160 0.873106    
bmi_catOverweight                            0.485 0.628095    
bmi_catObese                                 1.649 0.100237    
distal_resection                             0.213 0.831756    
nat_binary                                  -0.547 0.584936    
adenocarcinoma                               0.680 0.496870    
tobacco_ever                                -0.501 0.616922    
time_c:dm_hba1c_groupNo DM — Prediabetes     0.157 0.875623    
time_c:dm_hba1c_groupNo DM — HbA1c unknown   0.284 0.776651    
time_c:dm_hba1c_groupNew-Onset DM            1.985 0.047821 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation matrix not shown by default, as p = 16 > 12.
Use print(summary(m2a), correlation=TRUE)  or
    vcov(summary(m2a))        if you need it


=== Model 2B: Adjusted HbA1c subcohort (with pre-op HbA1c) ===
N obs: 303 | N pts: 109 
Linear mixed model fit by REML. t-tests use Satterthwaite's method [
lmerModLmerTest]
Formula: hba1c ~ time_c * dm_hba1c_group + hb_a1c_pre_op + age_at_surgery +  
    bmi_cat + distal_resection + nat_binary + adenocarcinoma +  
    tobacco_ever + (1 | mayo_id)
   Data: hba1c_long %>% filter(!is.na(hb_a1c_pre_op))
Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))

REML criterion at convergence: 1408.8

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.7625 -0.2021 -0.0197  0.1368  4.7625 

Random effects:
 Groups   Name        Variance Std.Dev.
 mayo_id  (Intercept) 67.140   8.194   
 Residual              1.227   1.108   
Number of obs: 303, groups:  mayo_id, 109

Fixed effects:
                                           Estimate Std. Error         df
(Intercept)                               -3.518449  12.695600  96.386804
time_c                                     0.008107   0.063666 192.486793
dm_hba1c_groupNo DM — Prediabetes         -3.028059   2.690313  99.359709
dm_hba1c_groupNew-Onset DM                -0.757285   2.295138  97.473263
hb_a1c_pre_op                              1.511608   2.443109  96.347507
age_at_surgery                             0.032859   0.073683  96.746208
bmi_catOverweight                          0.003897   2.086180  96.682017
bmi_catObese                               2.315345   1.996842  96.752746
distal_resection                          -0.067897   1.849252  96.687632
nat_binary                                -2.137039   2.714779  96.822543
adenocarcinoma                             1.443209   2.119877  96.675828
tobacco_ever                              -1.136671   1.701863  96.559156
time_c:dm_hba1c_groupNo DM — Prediabetes   0.015305   0.148690 200.007706
time_c:dm_hba1c_groupNew-Onset DM          0.161071   0.076849 192.199750
                                         t value Pr(>|t|)  
(Intercept)                               -0.277   0.7823  
time_c                                     0.127   0.8988  
dm_hba1c_groupNo DM — Prediabetes         -1.126   0.2631  
dm_hba1c_groupNew-Onset DM                -0.330   0.7421  
hb_a1c_pre_op                              0.619   0.5376  
age_at_surgery                             0.446   0.6566  
bmi_catOverweight                          0.002   0.9985  
bmi_catObese                               1.160   0.2491  
distal_resection                          -0.037   0.9708  
nat_binary                                -0.787   0.4331  
adenocarcinoma                             0.681   0.4976  
tobacco_ever                              -0.668   0.5058  
time_c:dm_hba1c_groupNo DM — Prediabetes   0.103   0.9181  
time_c:dm_hba1c_groupNew-Onset DM          2.096   0.0374 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation matrix not shown by default, as p = 14 > 12.
Use print(summary(m2b), correlation=TRUE)  or
    vcov(summary(m2b))        if you need it


=== AIC comparison ===
Model 1  AIC: 2964.157 
Model 2A AIC: 3171.178 
Model 2B AIC: 1440.751 


=== TABLE 7: LMM Fixed Effects ===


|Variable                                                             |      β|     SE|     95% CI     | P-Value  |
|:--------------------------------------------------------------------|------:|------:|:--------------:|:--------:|
|Model 1 — Unadjusted (N obs = 700; N patients = 306)                 |       |       |                |          |
|Group and Time Effects                                               |       |       |                |          |
|Intercept                                                            |  7.321|  0.768|  5.816–8.826   | <0.001 * |
|Time (per year after Y1)                                             | -0.289|  0.154|  -0.591–0.013  | 0.063 †  |
|No DM — Prediabetes vs Normal HbA1c                                  | -1.411|  1.380|  -4.116–1.294  |  0.308   |
|No DM — HbA1c unknown vs Normal HbA1c                                | -1.768|  0.913|  -3.557–0.020  | 0.054 †  |
|New-Onset DM vs Normal HbA1c                                         | -0.660|  0.921|  -2.466–1.146  |  0.474   |
|Time × Group Interactions                                            |       |       |                |          |
|Time × No DM — Prediabetes                                           |  0.274|  0.269|  -0.253–0.800  |  0.311   |
|Time × No DM — HbA1c unknown                                         |  0.300|  0.182|  -0.056–0.656  |  0.102   |
|Time × New-Onset DM                                                  |  0.408|  0.181|  0.053–0.763   | 0.026 *  |
|                                                                     |       |       |                |          |
|Model 2A — Adjusted, full cohort (N obs = 696; N patients = 305)     |       |       |                |          |
|Group and Time Effects                                               |       |       |                |          |
|Intercept                                                            |  6.072|  1.582|  2.970–9.173   | <0.001 * |
|Time (per year after Y1)                                             | -0.001|  0.060|  -0.118–0.116  |  0.988   |
|No DM — Prediabetes vs Normal HbA1c                                  | -1.774|  1.302|  -4.327–0.778  |  0.174   |
|No DM — HbA1c unknown vs Normal HbA1c                                | -1.609|  0.867|  -3.310–0.091  | 0.065 †  |
|New-Onset DM vs Normal HbA1c                                         | -0.740|  0.891|  -2.486–1.005  |  0.406   |
|Time × Group Interactions                                            |       |       |                |          |
|Time × No DM — Prediabetes                                           |  0.022|  0.138|  -0.248–0.291  |  0.876   |
|Time × No DM — HbA1c unknown                                         |  0.022|  0.078|  -0.131–0.175  |  0.777   |
|Time × New-Onset DM                                                  |  0.132|  0.066|  0.002–0.262   | 0.048 *  |
|Baseline Covariates                                                  |       |       |                |          |
|Pre-op HbA1c (per 1%)                                                |      —|      —|       —        |    —     |
|Age at surgery (per year)                                            |  0.010|  0.023|  -0.034–0.055  |  0.648   |
|BMI: Underweight vs Normal                                           |  0.564|  3.527|  -6.350–7.477  |  0.873   |
|BMI: Overweight vs Normal                                            |  0.317|  0.654|  -0.965–1.600  |  0.628   |
|BMI: Obese vs Normal                                                 |  1.245|  0.755|  -0.235–2.724  |  0.100   |
|Distal pancreatectomy                                                |  0.135|  0.633|  -1.106–1.375  |  0.832   |
|Neoadjuvant therapy                                                  | -0.442|  0.808|  -2.025–1.141  |  0.585   |
|Adenocarcinoma diagnosis                                             |  0.488|  0.718|  -0.919–1.895  |  0.497   |
|Tobacco use (ever)                                                   | -0.288|  0.575|  -1.414–0.839  |  0.617   |
|                                                                     |       |       |                |          |
|Model 2B — Adjusted, HbA1c subcohort (N obs = 303; N patients = 109) |       |       |                |          |
|Group and Time Effects                                               |       |       |                |          |
|Intercept                                                            | -3.518| 12.696| -28.402–21.365 |  0.782   |
|Time (per year after Y1)                                             |  0.008|  0.064|  -0.117–0.133  |  0.899   |
|No DM — Prediabetes vs Normal HbA1c                                  | -3.028|  2.690|  -8.301–2.245  |  0.263   |
|No DM — HbA1c unknown vs Normal HbA1c                                |      —|      —|       —        |    —     |
|New-Onset DM vs Normal HbA1c                                         | -0.757|  2.295|  -5.256–3.741  |  0.742   |
|Time × Group Interactions                                            |       |       |                |          |
|Time × No DM — Prediabetes                                           |  0.015|  0.149|  -0.276–0.307  |  0.918   |
|Time × No DM — HbA1c unknown                                         |      —|      —|       —        |    —     |
|Time × New-Onset DM                                                  |  0.161|  0.077|  0.010–0.312   | 0.037 *  |
|Baseline Covariates                                                  |       |       |                |          |
|Pre-op HbA1c (per 1%)                                                |  1.512|  2.443|  -3.277–6.300  |  0.538   |
|Age at surgery (per year)                                            |  0.033|  0.074|  -0.112–0.177  |  0.657   |
|BMI: Underweight vs Normal                                           |      —|      —|       —        |    —     |
|BMI: Overweight vs Normal                                            |  0.004|  2.086|  -4.085–4.093  |  0.999   |
|BMI: Obese vs Normal                                                 |  2.315|  1.997|  -1.598–6.229  |  0.249   |
|Distal pancreatectomy                                                | -0.068|  1.849|  -3.692–3.557  |  0.971   |
|Neoadjuvant therapy                                                  | -2.137|  2.715|  -7.458–3.184  |  0.433   |
|Adenocarcinoma diagnosis                                             |  1.443|  2.120|  -2.712–5.598  |  0.498   |
|Tobacco use (ever)                                                   | -1.137|  1.702|  -4.472–2.199  |  0.506   |

=== KEY RESULTS SUMMARY ===

--- Model 1 — Unadjusted ---
  Time (No DM Normal ref):           -0.289% per year
  Time x New-Onset DM interaction:   0.408% per year (p=0.026)
  Time x No DM Prediabetes:          0.274% per year
  New-Onset DM baseline offset:      -0.660%

--- Model 2A — Adjusted full cohort ---
  Time (No DM Normal ref):           -0.001% per year
  Time x New-Onset DM interaction:   0.132% per year (p=0.048)
  Time x No DM Prediabetes:          0.022% per year
  New-Onset DM baseline offset:      -0.740%

--- Model 2B — Adjusted HbA1c subcohort ---
  Time (No DM Normal ref):           0.008% per year
  Time x New-Onset DM interaction:   0.161% per year (p=0.037)
  Time x No DM Prediabetes:          0.015% per year
  New-Onset DM baseline offset:      -0.757%

=== EMM summary (Model 2A, Prediabetes group suppressed) ===
          dm_hba1c_group time   emmean lower.CL upper.CL
1   No DM — Normal HbA1c    1 7.206573 4.924502 9.488644
2  No DM — HbA1c unknown    1 5.597263 3.668946 7.525580
3           New-Onset DM    1 6.466116 4.515740 8.416492
4   No DM — Normal HbA1c    2 7.205673 4.927978 9.483367
5  No DM — HbA1c unknown    2 5.618518 3.701844 7.535192
6           New-Onset DM    2 6.597045 4.650273 8.543816
7   No DM — Normal HbA1c    3 7.204772 4.925459 9.484085
8  No DM — HbA1c unknown    3 5.639773 3.729799 7.549746
9           New-Onset DM    3 6.727974 4.783201 8.672746
10  No DM — Normal HbA1c    4 7.203872 4.916958 9.490786
11 No DM — HbA1c unknown    4 5.661028 3.752763 7.569292
12          New-Onset DM    4 6.858903 4.914519 8.803286
13  No DM — Normal HbA1c    5 7.202972 4.902529 9.503414
14 No DM — HbA1c unknown    5 5.682282 3.770722 7.593842
15          New-Onset DM    5 6.989831 5.044227 8.935436
16  No DM — Normal HbA1c    6 7.202071 4.882272 9.521871
17 No DM — HbA1c unknown    6 5.703537 3.783702 7.623373
18          New-Onset DM    6 7.120760 5.172327 9.069194
19  No DM — Normal HbA1c    7 7.201171 4.856322 9.546020
20 No DM — HbA1c unknown    7 5.724792 3.791762 7.657822
21          New-Onset DM    7 7.251689 5.298826 9.204553
22  No DM — Normal HbA1c    8 7.200271 4.824850 9.575691
23 No DM — HbA1c unknown    8 5.746047 3.794998 7.697096
24          New-Onset DM    8 7.382618 5.423733 9.341502
25  No DM — Normal HbA1c    9 7.199370 4.788057 9.610683
26 No DM — HbA1c unknown    9 5.767302 3.793536 7.741067
27          New-Onset DM    9 7.513547 5.547064 9.480030
28  No DM — Normal HbA1c   10 7.198470 4.746167 9.650773
29 No DM — HbA1c unknown   10 5.788556 3.787529 7.789583
30          New-Onset DM   10 7.644476 5.668835 9.620117


### Figure 3. Figure 3. Cumulative incidence of new-onset diabetes mellitus following pancreatectomy accounting for the competing risk of death.

=== New-onset DM TTE dataset ===
Patients: 729 

Event type distribution:

        Censored     New-Onset DM Death without DM 
             331              147              251 

Median follow-up (months): 29 
Max follow-up (months): 188.4 

5 cases omitted due to missing values
=== Overall CIF at key time points ===
# A tibble: 5 × 4
   time estimate conf.low conf.high
  <dbl>    <dbl>    <dbl>     <dbl>
1    12   0.0776   0.0594    0.0989
2    24   0.0988   0.0781    0.122 
3    36   0.145    0.119     0.173 
4    60   0.184    0.154     0.216 
5   120   0.255    0.216     0.297 

=== CIF by resection type ===
# A tibble: 10 × 5
    time strata                estimate conf.low conf.high
   <dbl> <fct>                    <dbl>    <dbl>     <dbl>
 1    12 Non-Distal (PD/Total)   0.0650   0.0452    0.0896
 2    24 Non-Distal (PD/Total)   0.0827   0.0600    0.110 
 3    36 Non-Distal (PD/Total)   0.127    0.0979    0.160 
 4    60 Non-Distal (PD/Total)   0.145    0.114     0.181 
 5   120 Non-Distal (PD/Total)   0.210    0.165     0.258 
 6    12 Distal                  0.104    0.0687    0.148 
 7    24 Distal                  0.133    0.0922    0.181 
 8    36 Distal                  0.183    0.134     0.239 
 9    60 Distal                  0.265    0.203     0.331 
10   120 Distal                  0.352    0.274     0.430 
Gray's test:
      stat           pv df
1 11.90675 5.593097e-04  1
2 28.87442 7.722599e-08  1

=== CIF by adenocarcinoma ===
# A tibble: 10 × 5
    time strata             estimate conf.low conf.high
   <dbl> <fct>                 <dbl>    <dbl>     <dbl>
 1    12 Non-Adenocarcinoma   0.0850   0.0544    0.124 
 2    24 Non-Adenocarcinoma   0.113    0.0764    0.157 
 3    36 Non-Adenocarcinoma   0.166    0.120     0.218 
 4    60 Non-Adenocarcinoma   0.244    0.185     0.307 
 5   120 Non-Adenocarcinoma   0.341    0.265     0.418 
 6    12 Adenocarcinoma       0.0727   0.0512    0.0990
 7    24 Adenocarcinoma       0.0886   0.0646    0.117 
 8    36 Adenocarcinoma       0.132    0.102     0.165 
 9    60 Adenocarcinoma       0.151    0.118     0.187 
10   120 Adenocarcinoma       0.211    0.166     0.261 
Gray's test:
       stat          pv df
1  10.02944 0.001540576  1
2 118.95026 0.000000000  1

=== CIF by NAT ===
# A tibble: 10 × 5
    time strata                 estimate conf.low conf.high
   <dbl> <fct>                     <dbl>    <dbl>     <dbl>
 1    12 No Neoadjuvant Therapy   0.0840   0.0627    0.109 
 2    24 No Neoadjuvant Therapy   0.106    0.0816    0.134 
 3    36 No Neoadjuvant Therapy   0.156    0.125     0.189 
 4    60 No Neoadjuvant Therapy   0.199    0.163     0.237 
 5   120 No Neoadjuvant Therapy   0.271    0.226     0.319 
 6    12 Neoadjuvant Therapy      0.0560   0.0275    0.0992
 7    24 Neoadjuvant Therapy      0.0751   0.0408    0.123 
 8    36 Neoadjuvant Therapy      0.108    0.0656    0.163 
 9    60 Neoadjuvant Therapy      0.134    0.0846    0.195 
10   120 Neoadjuvant Therapy      0.235    0.116     0.377 
Gray's test:
       stat           pv df
1  2.281961 1.308863e-01  1
2 54.871883 1.286748e-13  1

=== Fine-Gray Model: New-Onset DM ===
Fine-Gray model summary:
Competing Risks Regression

Call:
cmprsk::crr(ftime = fg_data$time_to_event_months, fstatus = as.integer(fg_data$event_type) - 
    1, cov1 = cov_matrix, failcode = 1, cencode = 0)

                  coef exp(coef) se(coef)      z p-value
bmi_overweight   0.532     1.702    0.205  2.590 9.6e-03
bmi_obese        0.902     2.465    0.212  4.250 2.1e-05
bmi_underweight -0.523     0.593    1.039 -0.503 6.1e-01
distal           0.489     1.630    0.184  2.662 7.8e-03
adenocarcinoma  -0.232     0.793    0.201 -1.153 2.5e-01
nat_binary      -0.127     0.881    0.247 -0.512 6.1e-01
family_hx_dm     0.169     1.184    0.132  1.277 2.0e-01

                exp(coef) exp(-coef)   2.5% 97.5%
bmi_overweight      1.702      0.588 1.1381  2.54
bmi_obese           2.465      0.406 1.6262  3.74
bmi_underweight     0.593      1.687 0.0774  4.54
distal              1.630      0.613 1.1375  2.34
adenocarcinoma      0.793      1.261 0.5344  1.18
nat_binary          0.881      1.135 0.5429  1.43
family_hx_dm        1.184      0.844 0.9136  1.53

Num. cases = 724
Pseudo Log-likelihood = -861 
Pseudo likelihood ratio test = 38.5  on 7 df,

=== Fine-Gray results table ===


|Variable                   |   N| SHR  |  95% CI   | P-Value  |
|:--------------------------|---:|:----:|:---------:|:--------:|
|BMI: Overweight vs Normal  | 724| 1.70 | 1.14–2.54 | 0.010 *  |
|BMI: Obese vs Normal       |    | 2.47 | 1.63–3.74 | <0.001 * |
|BMI: Underweight vs Normal |    | 0.59 | 0.08–4.54 |  0.610   |
|Distal pancreatectomy      |    | 1.63 | 1.14–2.34 | 0.008 *  |
|Adenocarcinoma diagnosis   |    | 0.79 | 0.53–1.18 |  0.250   |
|Neoadjuvant therapy        |    | 0.88 | 0.54–1.43 |  0.610   |
|Family history of DM       |    | 1.18 | 0.91–1.53 |  0.200   |

### Figure 4. Figure 4. HbA1c trajectory following pancreatectomy.
=== Panel A summary ===
# A tibble: 10 × 5
    year median   q25   q75     n
   <dbl>  <dbl> <dbl> <dbl> <int>
 1     1   5.8   5.4   6.3    154
 2     2   5.95  5.6   6.65    82
 3     3   6     5.6   6.7     81
 4     4   6     5.6   7       79
 5     5   6     5.5   6.7     77
 6     6   6.1   5.72  7.07    62
 7     7   6.1   5.7   7.1     49
 8     8   6     5.7   7.1     45
 9     9   6.2   5.75  7.3     39
10    10   6.2   5.78  7.72    32

=== Panel B summary ===
# A tibble: 20 × 6
   dm_group      year median   q25   q75     n
   <fct>        <dbl>  <dbl> <dbl> <dbl> <int>
 1 No DM            1   5.5   5.2   5.9     97
 2 No DM            2   5.65  5.4   6       44
 3 No DM            3   5.7   5.5   5.95    39
 4 No DM            4   5.7   5.45  5.9     35
 5 No DM            5   5.6   5.2   5.9     37
 6 No DM            6   5.75  5.4   5.9     24
 7 No DM            7   5.7   5.65  5.9     19
 8 No DM            8   5.6   5.2   5.8     20
 9 No DM            9   5.9   5.6   6.1     17
10 No DM           10   5.8   5.7   6.18    14
11 New-Onset DM     1   6.3   5.9   6.9     57
12 New-Onset DM     2   6.75  6.02  8.15    38
13 New-Onset DM     3   6.65  6.1   7.75    42
14 New-Onset DM     4   6.9   6.25  7.6     44
15 New-Onset DM     5   6.7   6.28  7.6     40
16 New-Onset DM     6   6.7   6.1   7.95    38
17 New-Onset DM     7   7     6.22  7.57    30
18 New-Onset DM     8   7     6.1   9       25
19 New-Onset DM     9   7.15  6.43  8.07    22
20 New-Onset DM    10   7.3   6.13  8.93    18

### Figure 5. Cumulative incidence of post-operative exocrine insufficiency following pancreatectomy accounting for the competing risk of death.

=== Exocrine insufficiency TTE dataset ===
Patients: 606 

Event type distribution:

                Censored   Exocrine Insufficiency Death without Exo Insuff 
                     229                      248                      129 

Median follow-up (months): 15.1 
Max follow-up (months): 188.4 

4 cases omitted due to missing values
=== Overall CIF at key time points ===
# A tibble: 5 × 4
   time estimate conf.low conf.high
  <dbl>    <dbl>    <dbl>     <dbl>
1    12    0.307    0.270     0.344
2    24    0.362    0.323     0.401
3    36    0.397    0.356     0.437
4    60    0.421    0.379     0.462
5   120    0.467    0.419     0.513

=== CIF by resection type ===
# A tibble: 10 × 5
    time strata                estimate conf.low conf.high
   <dbl> <fct>                    <dbl>    <dbl>     <dbl>
 1    12 Non-Distal (PD/Total)    0.376    0.328     0.425
 2    24 Non-Distal (PD/Total)    0.447    0.396     0.496
 3    36 Non-Distal (PD/Total)    0.481    0.430     0.531
 4    60 Non-Distal (PD/Total)    0.503    0.451     0.554
 5   120 Non-Distal (PD/Total)    0.550    0.491     0.606
 6    12 Distal                   0.170    0.121     0.225
 7    24 Distal                   0.192    0.140     0.250
 8    36 Distal                   0.229    0.172     0.291
 9    60 Distal                   0.257    0.195     0.322
10   120 Distal                   0.298    0.224     0.376
Gray's test:
       stat           pv df
1 32.496756 1.193917e-08  1
2  3.577828 5.855567e-02  1

=== CIF by adenocarcinoma ===
# A tibble: 10 × 5
    time strata             estimate conf.low conf.high
   <dbl> <fct>                 <dbl>    <dbl>     <dbl>
 1    12 Non-Adenocarcinoma    0.170    0.122     0.225
 2    24 Non-Adenocarcinoma    0.213    0.159     0.272
 3    36 Non-Adenocarcinoma    0.249    0.190     0.311
 4    60 Non-Adenocarcinoma    0.270    0.208     0.335
 5   120 Non-Adenocarcinoma    0.339    0.256     0.423
 6    12 Adenocarcinoma        0.385    0.335     0.434
 7    24 Adenocarcinoma        0.443    0.392     0.493
 8    36 Adenocarcinoma        0.478    0.426     0.529
 9    60 Adenocarcinoma        0.504    0.451     0.555
10   120 Adenocarcinoma        0.538    0.479     0.593
Gray's test:
      stat           pv df
1 28.22525 1.079868e-07  1
2 52.42123 4.478640e-13  1

=== CIF by NAT ===
# A tibble: 10 × 5
    time strata                 estimate conf.low conf.high
   <dbl> <fct>                     <dbl>    <dbl>     <dbl>
 1    12 No Neoadjuvant Therapy    0.261    0.222     0.301
 2    24 No Neoadjuvant Therapy    0.318    0.276     0.361
 3    36 No Neoadjuvant Therapy    0.352    0.308     0.396
 4    60 No Neoadjuvant Therapy    0.379    0.333     0.424
 5   120 No Neoadjuvant Therapy    0.427    0.374     0.478
 6    12 Neoadjuvant Therapy       0.500    0.404     0.589
 7    24 Neoadjuvant Therapy       0.546    0.448     0.634
 8    36 Neoadjuvant Therapy       0.586    0.487     0.672
 9    60 Neoadjuvant Therapy       0.597    0.497     0.683
10   120 Neoadjuvant Therapy       0.659    0.489     0.784
Gray's test:
       stat           pv df
1 23.094377 1.542411e-06  1
2  3.045794 8.094652e-02  1

=== Fine-Gray Model: Exocrine Insufficiency ===
Fine-Gray model summary:
Competing Risks Regression

Call:
cmprsk::crr(ftime = fg_exo_data$time_to_event_months, fstatus = as.integer(fg_exo_data$event_type) - 
    1, cov1 = cov_exo, failcode = 1, cencode = 0)

                    coef exp(coef) se(coef)     z p-value
distal            -0.701     0.496    0.182 -3.85 1.2e-04
adenocarcinoma     0.453     1.573    0.176  2.57 1.0e-02
nat_binary         0.470     1.600    0.154  3.05 2.3e-03
new_onset_dm_flag  0.683     1.981    0.143  4.78 1.8e-06

                  exp(coef) exp(-coef)  2.5% 97.5%
distal                0.496      2.017 0.347 0.709
adenocarcinoma        1.573      0.636 1.113 2.222
nat_binary            1.600      0.625 1.183 2.162
new_onset_dm_flag     1.981      0.505 1.496 2.622

Num. cases = 601
Pseudo Log-likelihood = -1455 
Pseudo likelihood ratio test = 70  on 4 df,

=== Fine-Gray results table ===


|Variable                    |   N| SHR  |  95% CI   | P-Value  |
|:---------------------------|---:|:----:|:---------:|:--------:|
|Distal pancreatectomy       | 601| 0.50 | 0.35–0.71 | <0.001 * |
|Adenocarcinoma diagnosis    |    | 1.57 | 1.11–2.22 | 0.010 *  |
|Neoadjuvant therapy         |    | 1.60 | 1.18–2.16 | 0.002 *  |
|New-onset diabetes mellitus |    | 1.98 | 1.50–2.62 | <0.001 * |


### Figure 6. Forest plot of multivariable predictors of new-onset diabetes mellitus following pancreatectomy.

| Variable                                           | N   | OR   | 95% CI        | P-Value   |
|----------------------------------------------------|-----|------|---------------|-----------|
| **Model A — Full Cohort (no HbA1c)**               | 735 |      |               |           |
| Age at surgery (per year)                          |     | 1.01 | 0.99–1.02     | 0.401     |
| BMI: Underweight vs Normal                         |     | 0.57 | 0.06–2.58     | 0.510     |
| BMI: Overweight vs Normal                          |     | 1.90 | 1.22–3.00     | 0.005 *   |
| BMI: Obese vs Normal                               |     | 2.92 | 1.81–4.73     | <0.001 *  |
| Family history of DM                               |     | 1.28 | 0.94–1.73     | 0.122     |
| Clinically relevant POPF                           |     | 1.21 | 0.64–2.21     | 0.542     |
| Neoadjuvant therapy                                |     | 0.87 | 0.50–1.47     | 0.595     |
| Adenocarcinoma diagnosis                           |     | 0.66 | 0.42–1.02     | 0.061     |
| History of pancreatitis                            |     | 1.20 | 0.75–1.89     | 0.451     |
| Pre-op exocrine insufficiency                      |     | 1.49 | 0.89–2.48     | 0.131     |
| Tobacco use (ever)                                 |     | 1.49 | 1.03–2.17     | 0.036 *   |
| **Model B — HbA1c Subcohort**                      | 206 |      |               |           |
| Age at surgery (per year)                          |     | 1.00 | 0.97–1.03     | 0.985     |
| BMI: Underweight vs Normal                         |     | 0.84 | 0.01–9.23     | 0.909     |
| BMI: Overweight vs Normal                          |     | 1.61 | 0.68–3.84     | 0.278     |
| BMI: Obese vs Normal                               |     | 2.48 | 1.11–5.73     | 0.027 *   |
| HbA1c: Prediabetes vs Normal (<5.7%)               |     | 3.07 | 1.58–6.11     | <0.001 *  |
| Family history of DM                               |     | 0.86 | 0.49–1.48     | 0.597     |
| Clinically relevant POPF                           |     | 0.75 | 0.19–2.57     | 0.661     |
| Neoadjuvant therapy                                |     | 1.83 | 0.66–5.03     | 0.242     |
| Adenocarcinoma diagnosis                           |     | 0.28 | 0.11–0.66     | 0.003 *   |
| History of pancreatitis                            |     | 1.01 | 0.46–2.14     | 0.987     |
| Pre-op exocrine insufficiency                      |     | 1.60 | 0.68–3.68     | 0.273     |

### Figure 7. Forest plot of multivariable predictors of post-operative exocrine insufficiency following pancreatectomy.

| Variable                                           | N   | OR   | 95% CI        | P-Value   |
|----------------------------------------------------|-----|------|---------------|-----------|
| **Model A — Full Cohort**                          | 613 |      |               |           |
| Age at surgery (per year)                          |     | 0.99 | 0.97–1.00     | 0.107     |
| BMI: Underweight vs Normal                         |     | 0.29 | 0.05–1.20     | 0.089     |
| BMI: Overweight vs Normal                          |     | 0.80 | 0.53–1.18     | 0.257     |
| BMI: Obese vs Normal                               |     | 0.78 | 0.48–1.23     | 0.284     |
| Family history of DM                               |     | 1.21 | 0.89–1.65     | 0.228     |
| Clinically relevant POPF                           |     | 1.28 | 0.71–2.32     | 0.405     |
| Neoadjuvant therapy                                |     | 1.80 | 1.13–2.89     | 0.013 *   |
| Adenocarcinoma diagnosis                           |     | 2.06 | 1.32–3.25     | 0.001 *   |
| History of pancreatitis                            |     | 1.03 | 0.65–1.62     | 0.890     |
| Tobacco use (ever)                                 |     | 0.91 | 0.64–1.29     | 0.597     |
| Distal pancreatectomy                              |     | 0.42 | 0.28–0.63     | <0.001 *  |
| New-onset diabetes mellitus                        |     | 2.67 | 1.72–4.20     | <0.001 *  |
| **Model B — HbA1c Subcohort**                      | 164 |      |               |           |
| Age at surgery (per year)                          |     | 0.98 | 0.95–1.01     | 0.130     |
| BMI: Underweight vs Normal                         |     | 0.23 | 0.02–2.17     | 0.196     |
| BMI: Overweight vs Normal                          |     | 0.84 | 0.36–1.92     | 0.675     |
| BMI: Obese vs Normal                               |     | 0.33 | 0.14–0.77     | 0.010 *   |
| HbA1c: Prediabetes vs Normal (<5.7%)               |     | 0.83 | 0.41–1.68     | 0.607     |
| Family history of DM                               |     | 0.99 | 0.58–1.68     | 0.962     |
| Clinically relevant POPF                           |     | 2.49 | 0.67–9.55     | 0.170     |
| Neoadjuvant therapy                                |     | 3.74 | 1.35–11.88    | 0.010 *   |
| Adenocarcinoma diagnosis                           |     | 3.24 | 1.45–7.67     | 0.004 *   |
| History of pancreatitis                            |     | 1.48 | 0.66–3.33     | 0.339     |
| New-onset diabetes mellitus                        |     | 1.90 | 0.82–4.57     | 0.136     |


### Figure 8. Cumulative incidence of pancreatic endocrine and exocrine dysfunction following pancreatectomy stratified by BMI

#### Diabetes (DM) Cumulative Incidence by BMI
| Time (months) | BMI Group   | Estimate | 95% CI      |
| ------------- | ----------- | -------- | ----------- |
| 12            | Normal      | 0.051    | 0.029–0.082 |
| 24            | Normal      | 0.059    | 0.035–0.092 |
| 36            | Normal      | 0.087    | 0.056–0.126 |
| 60            | Normal      | 0.139    | 0.096–0.189 |
| 120           | Normal      | 0.186    | 0.130–0.250 |
| 12            | Underweight | 0.071    | 0.004–0.286 |
| 24            | Underweight | 0.071    | 0.004–0.286 |
| 36            | Underweight | 0.071    | 0.004–0.286 |
| 60            | Underweight | 0.071    | 0.004–0.286 |
| 120           | Underweight | 0.071    | 0.004–0.286 |
| 12            | Overweight  | 0.099    | 0.067–0.139 |
| 24            | Overweight  | 0.111    | 0.076–0.152 |
| 36            | Overweight  | 0.155    | 0.113–0.203 |
| 60            | Overweight  | 0.177    | 0.132–0.229 |
| 120           | Overweight  | 0.270    | 0.206–0.338 |
| 12            | Obese       | 0.089    | 0.051–0.140 |
| 24            | Obese       | 0.151    | 0.100–0.213 |
| 36            | Obese       | 0.238    | 0.171–0.311 |
| 60            | Obese       | 0.285    | 0.210–0.365 |
| 120           | Obese       | 0.365    | 0.270–0.460 |

Gray’s test (DM ~ BMI):
- Statistic = 19.94, p = 0.00017
- Statistic = 11.29, p = 0.010

#### Exocrine Insufficiency (Exo) Cumulative Incidence by BMI
| Time (months) | BMI Group   | Estimate | 95% CI      |
| ------------- | ----------- | -------- | ----------- |
| 12            | Normal      | 0.342    | 0.279–0.406 |
| 24            | Normal      | 0.392    | 0.326–0.458 |
| 36            | Normal      | 0.426    | 0.358–0.493 |
| 60            | Normal      | 0.439    | 0.369–0.506 |
| 120           | Normal      | 0.453    | 0.379–0.523 |
| 12            | Underweight | 0.250    | 0.030–0.579 |
| 24            | Underweight | 0.250    | 0.030–0.579 |
| 36            | Underweight | 0.250    | 0.030–0.579 |
| 60            | Underweight | 0.250    | 0.030–0.579 |
| 120           | Underweight | 0.250    | 0.030–0.579 |
| 12            | Overweight  | 0.317    | 0.258–0.378 |
| 24            | Overweight  | 0.357    | 0.296–0.420 |
| 36            | Overweight  | 0.387    | 0.324–0.451 |
| 60            | Overweight  | 0.419    | 0.352–0.484 |
| 120           | Overweight  | 0.463    | 0.386–0.536 |
| 12            | Obese       | 0.237    | 0.169–0.312 |
| 24            | Obese       | 0.330    | 0.250–0.413 |
| 36            | Obese       | 0.378    | 0.293–0.464 |
| 60            | Obese       | 0.411    | 0.321–0.498 |
| 120           | Obese       | 0.519    | 0.398–0.626 |

Gray’s test (Exo ~ BMI):
- Statistic = 0.73, p = 0.865
- Statistic = 11.53, p = 0.009
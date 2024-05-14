### Codes et images à retenir

- code_ROC
- code_ROXI
- code_transf

- ROC1
- ROC2
- ROC3

---

### TRe

**Logistic Regression** (Model selection by likelihood test)

|                   | N = 383    | OR [95%CI]    |  p-value     |
| :------------     | :--     |  :--          | :--          |
| **Terme**         |         |               | 0.0015       |
| < 37              | 49 (12.8)  | *Reference*   | |  
| > 37              | 334 (87.2 | 0.29 [0.14 ; 0.63] | |
| **Age**           |          |              | < 0.001          |
| <1 mois     | 80 (20.9) | *Reference* | | 
| 1-3 mois      | 175 (45.7) | 0.31 [0.16 ; 0.58] | | 
| > 3 mois  | 128 (33.4)  | 0.09 [0.04 ; 0.20]   | | 
| **Fréquence** |   |                         | 0.16 |
| Normocarde | 204 (53.3) | *Reference* | |
| >160 bpm | 139 (36.3) | 1.67 [0.91 ; 3.06] | |
| >180 bpm | 40 (10.4) | 1.94 [0.75 ; 4.80] | |
| **SpO2** |        |        |    <0.001 |
| <= 92  | 43 (11.2) | *Reference* | |
| > 92   | 340 (88.8) | 0.24 [0.11 ; 0.51] | |

----

**Collinearity**

Low Correlation

|    Term | VIF   [95% CI] | Increased SE |Tolerance [95% CI] |
| :-      | :-              | :-          | :-              |
| Terme.F | 1.07 [1.02, 1.34]  |  1.04     | 0.93     [0.75, 0.98] |
|   Age.F | 1.12 [1.04, 1.33]     |    1.06    |  0.89     [0.75, 0.96] |
|    FC.F | 1.07 [1.02, 1.34]      |   1.04    |  0.93     [0.75, 0.98] |
|  Spo2.F | 1.07 [1.02, 1.34]      |   1.04    |  0.93     [0.75, 0.98] |


**Hosmer-Lemeshow goodness-of-fit test**

| | value |
| -: | :- |
| Statistic | 6.88921 |
| degrees of freedom | 7 |
| p-value | 0.44051 |


**McFadden's Pseudo-R2**

_mod.null = glm(TRe01 ~ 1, data = rp, family = binomial)_ \
_1-logLik(mod1)/logLik(mod.null)_ \
**'log Lik.' 0.1686141 (df=7)**


**Accuracy of Model Predictions (Bootstrap)**

Accuracy (95% CI): 77.44% [71.38%, 83.14%] \
Method: Area under Curve

---

**Machine learning evaluation of the model**

*Missclassification error on train data*

| **Actual** | | |
| :- | :--: | :--: |
| **Predicted** |  _0_  | _1_ |
| _0_ | 236 | 44 |
| _1_  | 3  |12 |

_1-sum(diag(tab1))/sum(tab1)_ \
**0.159322 -> 15.9% missclassification error rate on train data**

*Misclassification error on test data*

| **Actual** | | |
| :- | :--: | :--: |
| **Predicted** |  _0_ | _1_ |
| _0_ | 66 | 17 |
| _1_ | 2 | 3 |

**17+2 = 19 missclassifications**

**Goodness-of-fit of the model by machine learning** \
_Null deviance: 381.64  on 381  degrees of freedom_ \
_Residual deviance: 317.29  on 375  degrees of freedom_ \
\
_pvalue = 1-pchisq(381.64-317.29, df=(382-375))_ \
**pvalue =  2.031297e-11**

---

### VNI

**Logistic Regression** (Model selection by likelihood test)

|                   | N = 383    | OR [95%CI]    |  p-value     |
| :------------     | :--     |  :--          | :--          |
| **Wood**            |           |               |  0.06             |
| 1                 | 247 (65.0)  | *Reference*    | |
| 2                 | 133 (35.0)    | 1.89 [0.98 ; 3.67]    | |
| **Apnee**       |                 |             |  0.04             |
| Non           | 36 (9.4)       | *Reference* | |
| Oui             | 347 (90.6)    | 0.38 [0.15 ; 0.96] |    |
| **Malaise**       |           |                       | 0.71 |
| Non               | 35 (9.1) | *Reference*   | |
| Oui               | 348 (90.9) | 0.31 [0.14 ; 0.71] | | 
| **ROXI** |    8.33 [6.7 - 10.4] | 0.93 [0.84 ; 1.01] | 0.11 | 

---

**Collinearity**

Low Correlation

| Term  | VIF  [95%CI] | Increased SE | Tolerance [95%CI] |
| :-    | :-            | :-          | :-                |
| Wood  | 1.16 [1.06, 1.40]   |      1.08    |  0.86     [0.72, 0.94] |
|   apnee | 1.16 [1.06, 1.40]  |       1.07   |   0.87     [0.72, 0.94] |
| malaise | 1.07 [1.01, 1.42]   |      1.04   |   0.93     [0.70, 0.99] |
|    ROXI | 1.23 [1.11, 1.46]   |      1.11   |   0.81     [0.68, 0.90] |

**Hosmer-Lemeshow goodness-of-fit test**

| | value |
| -: | :- |
| Statistic | 7.7259 |
| degrees of freedom | 8 |
| p-value | 0.46069  |

**McFadden's Pseudo-R2**

_mod.null = glm(VNI ~ 1, data = re2s, family = binomial)_ \
_1-logLik(modv1)/logLik(mod.null)_ \
**'log Lik.' 0.2963833 (df=5)**

**Accuracy of Model Predictions (Bootstrap)**

Accuracy (95% CI): 71.95% [64.36%, 79.55%]
Method: Area under Curve

---

**Machine learning evaluation of the model**

*Missclassification error on train data*

| **Actual** | | |
| :- | :--: | :--: |
| **Predicted** | _0_ | _1_ | 
| _0_ | 185| 36 | 
| _1_ | 5 | 6 | 

_1-sum(diag(tab1))/sum(tab1)_ \
**0.1767241 -> 17.6% misclassification error rate on train data**

*Misclassification error on test data*

| **Actual** | | |
| :- | :--: | :--: |
| **Predicted** |  _0_ | _1_ |
| _0_ | 50 | 11 |
| _1_ | 1 | 3 | 

**1+11 = 13 misclassifications**

**Goodness-of-fit of the model by machine learning** \
_Null deviance: 58.661  on 295  degrees of freedom_ \
_Residual deviance: 47.843  on 375  degrees of freedom_ \
\
_pvalue = 1-pchisq(58.661-47.843, df=(295-290))_ \
**pvalue =  4.938667e-05**

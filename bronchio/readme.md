**Logistic Regression**

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
**0.159322 -> 15.9 missclassification error rate on train data**

*Misclassification error on test data*

| **Actual** | | |
| :- | :--: | :--: |
| **Predicted** |  _0_ | _1_ |
| _0_ | 66 | 17 |
| _1_ | 2 | 3 |

**17+2 = 19 missclassifications**

*Goodness-of-fit of the model by machine learning* \
_Null deviance: 381.64  on 381  degrees of freedom_ \
_Residual deviance: 317.29  on 375  degrees of freedom_ \
\
_pvalue = 1-pchisq(381.64-317.29, df=(382-317.29))_ \
**pvalue = 0.4892427**

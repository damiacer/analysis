### codes

- reaped_models.R
- ROC.R

### Tranfert en réanimation 
  
**Logistic Regression** 

|                   | (N = 383)  | Events (n = 76)  | OR [95%CI]    |  _p-value_     |
| :---------        | :-------        |  :-------    |:---------            | -----:          |
| **Terme**         |            |          |               | _<0.001_     | 
| > 37              | 334 (87.2) | 59       | _Reference_   |              | 
| ≤ 37              | 49 (12.8)  | 17       | 4.25 [1.84 ; 9.95]    |      |
| **Age**           |            |          |               | _<0.001_     | 
| > 3 mois          | 128 (33.4) | 10       | _Reference_   |              |
| [1-3] mois        | 175 (45.7) | 34       | 3.88 [1.60 ; 10.64]    |     |           
| < 1 mois          | 80 (20.9)  | 32       | 14.9 [5.44 ; 46.12]    |     | 
| **Fréquence**     |            |          |               | _0.33_       |
| < 180             | 343 (89.6) | 66       | _Reference_   |              |
| ≥ 180             | 40 (10.4)  | 10       | 1.62 [0.59 ; 4.20] |         | 
| **SpO2**          |            |          |               | _0.02_       |
| > 92              | 340 (88.8) | 56       | _Reference_   |              | 
| ≤ 92              | 43 (11.2)  | 20       | 2.69 [1.62 ; 6.93] |         | 
| **Wood**          |            |          |               | _0.011_      |
| 0-2.5             | 247 (65.0) | 34       | _Reference_   |              |  
| ≥ 3               | 133 (35.0) | 42       | 3.31 [1.62 ; 6.93]   |       | 
| **Apnées maison** |            |          |               | _0.67_       |
| Non               | 372 (97.2) | 71       | _Reference_   |              |
| Oui               | 11 (2.8)   | 5        | 1.45 [0.23 ; 7.92] |         | 
| **Malaise**       |            |          |               | _0.65_       |
| Non               | 363 (94.8) | 68       | _Reference_   |              |
| Oui               | 20 (5.2)   | 8        | 1.32 [0.38 ; 4.23] |         | 
| **ROXI**          | 8.33 [6.7 - 10.4] |   |0.98 [0.90 ; 1.07] | _0.66_ | 

---

#### Model check

##### Multicollinearity

| Term | VIF [95% CI] | Increased SE | Tolerance [95%CI] |
| :----| :----        | :----        | :----             |
| Terme | 1.11 [1.03, 1.37] | 1.05 | 0.90 [0.73, 0.97] |
| Age | 1.32 [1.18, 1.56] | 1.15 | 0.76 [0.64, 0.84] |
| FC | 1.12 [1.04, 1.37] | 1.06 | 0.90 [0.73, 0.96] |
| SpO2 | 1.18 [1.08, 1.41] | 1.09 | 0.85 [0.71, 0.93] |
| Wood | 1.31 [1.17, 1.55] | 1.14 | 0.76 [0.65, 0.85] |
| Apnees | 1.13 [1.05, 1.37] | 1.06 | 0.88 [0.73, 0.96] |
| Malaise | 1.18 [1.08, 1.41] | 1.09 | 0.84 [0.71, 0.92] | 
| ROXI | 1.30 [1.17, 1.54] | 1.14 | 0.77 [0.65, 0.85] |

##### Hosmer-Lemeshow goodness-of-fit 

|           |           |
| :---      | :---      |
| Statistic | 16.7416 |
| degrees of freedom | 8 |
|  p-value | 0.032915 |

##### Pseudo-R2 de McFadden

_R2 = 0.3633187 (df=10)_

##### Accuracy of Model Predictions (bootstrap)

_Accuracy (95% CI): 82.00% [75.92%, 87.63%]_ \
_(Method: Area under Curve)_

<br>

---

### VNI 
  
**Logistic Regression** 

|                   | (N = 383)  | Events (n = 73)  | OR [95%CI]    |  _p-value_     |
| :---------        | :-------        |  :-------    |:---------            | -----:          |
| **Terme**         |            |          |               | _0.009_      | 
| > 37              | 334 (87.2) | 59       | _Reference_   |              | 
| ≤ 37              | 49 (12.8)  | 14       | 3.21 [1.31 ; 7.82]    |      |
| **Age**           |            |          |               | _<0.001_     | 
| > 3 mois          | 128 (33.4) | 5        | _Reference_   |              |
| [1-3] mois        | 175 (45.7) | 37       | 12.2 [3.70 ; 58.8]    |      |           
| < 1 mois          | 80 (20.9)  | 31       | 38.6 [10.6 ; 201.5]    |     | 
| **Fréquence**     |            |          |               | _0.44_       |
| < 180             | 343 (89.6) | 65       | _Reference_   |              |
| ≥ 180             | 40 (10.4)  | 8        | 1.52 [0.49 ; 4.40] |         | 
| **SpO2**          |            |          |               | _0.34_       |
| > 92              | 340 (88.8) | 58       | _Reference_   |              | 
| ≤ 92              | 43 (11.2)  | 15       | 1.56 [0.61 ; 3.89] |         | 
| **Wood**          |            |          |               | _0.04_       |
| 0-2.5             | 247 (65.0) | 38       | _Reference_   |              |  
| ≥ 3               | 133 (35.0) | 35       | 2.17 [1.05 ; 4.56]   |       | 
| **Apnées maison** |            |          |               | _0.81_       |
| Non               | 372 (97.2) | 69       | _Reference_   |              |
| Oui               | 11 (2.8)   | 4        | 0.80 [0.09 ; 4.81] |         | 
| **Malaise**       |            |          |               | _0.06_       |
| Non               | 363 (94.8) | 65       | _Reference_   |              |
| Oui               | 20 (5.2)   | 8        | 3.24 [0.92 ; 11.34] |        | 
| **ROXI**          | 8.33 [6.7 - 10.4] |   | 0.94 [0.86 ; 1.04] | _0.30_ | 

---

#### Model check 

##### Multicollinearity

| Term | VIF [95%CI] | Increased SE | Tolerance [95%CI] |
| :---- | :---- | :---- | :---- | 
| Terme | 1.10 [1.03, 1.37] | 1.05 | 0.91 [0.73, 0.97] |
| Age | 1.31 [1.18, 1.55] | 1.14 | 0.76 [0.65, 0.85] |
| FC | 1.14 [1.05, 1.38] | 1.07 | 0.88 [0.72, 0.95] |
| SpO2 | 1.17 [1.07, 1.40] | 1.08 | 0.86 [0.71, 0.93] |
| Wood | 1.25 [1.13, 1.49] | 1.12 | 0.80 [0.67, 0.88] |
| Apnees | 1.11 [1.03, 1.37] | 1.05 | 0.90 [0.73, 0.97] |
| Malaise | 1.20 [1.09, 1.43] | 1.10 | 0.83 [0.70, 0.91] |
| ROXI | 1.31 [1.17, 1.55] | 1.14 | 0.76 [0.65, 0.85] |

##### Hosmer-Lemeshow goodness-of-fit test

|           |       |
| :-        | :-    |
| Statistic | 11.10529 |
| degrees of freedom | 8 |
| p-value | 0.1958 |

##### Pseudo-R2 de McFadden

_R2 = 0.3929356 (df=10)_

##### Accuracy of Model Predictions (bootstrap)

_Accuracy (95% CI): 82.08% [75.45%, 88.41%]_ \
_(Method: Area under Curve)_

---

### ROC CURVES and YOUDEN

##### VNI

- ROC AUC = 0.6467101
- optimal threshold / slope = 0.2045
- optimal point in the distribution (ROX Index) = 7.6

##### TRANSFER

- ROC AUC = 0.6597371
- optimal threshold / slope = 0.2387867
- optimal point in the distribution (ROX Index) = 6.86

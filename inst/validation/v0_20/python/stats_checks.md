# Numerical checks for disappR 0.20.2 (Python; not the R package)

## 1. Step 4: which ageing function wins - the true likelihood, Poisson AICc and QAICc

100 simulated datasets per row, 80 individuals each; the true function is quadratic on the log scale. Share of datasets in which each function has the lowest mean difference across individuals (the ranking of the step-4 comparison table). 'Oracle' uses the correct likelihood with the true dispersion or zero-inflation: the benchmark a criterion should match.

| Design | Counts | Criterion | Linear | Quadratic (true) | Cubic | mean c-hat |
|---|---|---|---|---|---|---|
| A: 5-12 records, mild curvature | Poisson | oracle (true likelihood) | 0.87 | 0.13 | 0.00 |  |
| A: 5-12 records, mild curvature | Poisson | Poisson AICc (0.19.6) | 0.87 | 0.13 | 0.00 |  |
| A: 5-12 records, mild curvature | Poisson | QAICc (0.20.x) | 0.93 | 0.07 | 0.00 | 1.04 |
| A: 5-12 records, mild curvature | negative binomial (theta = 2) | oracle (true likelihood) | 1.00 | 0.00 | 0.00 |  |
| A: 5-12 records, mild curvature | negative binomial (theta = 2) | Poisson AICc (0.19.6) | 0.02 | 0.98 | 0.00 |  |
| A: 5-12 records, mild curvature | negative binomial (theta = 2) | QAICc (0.20.x) | 1.00 | 0.00 | 0.00 | 3.60 |
| A: 5-12 records, mild curvature | zero-inflated Poisson (30% zeros) | oracle (true likelihood) | 0.99 | 0.01 | 0.00 |  |
| A: 5-12 records, mild curvature | zero-inflated Poisson (30% zeros) | Poisson AICc (0.19.6) | 0.16 | 0.84 | 0.00 |  |
| A: 5-12 records, mild curvature | zero-inflated Poisson (30% zeros) | QAICc (0.20.x) | 1.00 | 0.00 | 0.00 | 3.41 |
| B: 10-16 records, strong curvature | Poisson | oracle (true likelihood) | 0.00 | 1.00 | 0.00 |  |
| B: 10-16 records, strong curvature | Poisson | Poisson AICc (0.19.6) | 0.00 | 1.00 | 0.00 |  |
| B: 10-16 records, strong curvature | Poisson | QAICc (0.20.x) | 0.00 | 1.00 | 0.00 | 1.00 |
| B: 10-16 records, strong curvature | negative binomial (theta = 2) | oracle (true likelihood) | 0.00 | 1.00 | 0.00 |  |
| B: 10-16 records, strong curvature | negative binomial (theta = 2) | Poisson AICc (0.19.6) | 0.00 | 1.00 | 0.00 |  |
| B: 10-16 records, strong curvature | negative binomial (theta = 2) | QAICc (0.20.x) | 0.00 | 1.00 | 0.00 | 2.57 |
| B: 10-16 records, strong curvature | zero-inflated Poisson (30% zeros) | oracle (true likelihood) | 0.00 | 1.00 | 0.00 |  |
| B: 10-16 records, strong curvature | zero-inflated Poisson (30% zeros) | Poisson AICc (0.19.6) | 0.00 | 1.00 | 0.00 |  |
| B: 10-16 records, strong curvature | zero-inflated Poisson (30% zeros) | QAICc (0.20.x) | 0.00 | 1.00 | 0.00 | 2.33 |

## 2. Direction of ageing: first coefficient's sign against the predictions' slope

Trait = 10 -/+ 3 (1 - exp(-age / 3)) + noise, 3000 records, ages 1-12; ordinary least squares with each basis. The predictions' slope is taken between the 25th and 75th age percentiles (as the 0.20.2 evidence summary does).

| Basis | Truth | Sign of f1 coefficient | Sign of predicted slope | Coefficient rule right? | Prediction rule right? |
|---|---|---|---|---|---|
| Linear | declining | - | - | yes | yes |
| Linear | increasing | + | + | yes | yes |
| Quadratic | declining | - | - | yes | yes |
| Quadratic | increasing | + | + | yes | yes |
| Cubic | declining | - | - | yes | yes |
| Cubic | increasing | + | + | yes | yes |
| Logarithmic | declining | - | - | yes | yes |
| Logarithmic | increasing | + | + | yes | yes |
| Asymptotic exponential | declining | + | - | **no** | yes |
| Asymptotic exponential | increasing | - | + | **no** | yes |

## 3. Does standardising age change the AIC?

20 simulated datasets (150 individuals, individual differences in ageing rate), Model 1 with linear ageing, maximum likelihood; |AIC(raw age) - AIC(standardised age)|.

| Random effects | median | max |
|---|---|---|
| none | 4.9e-09 | 4.6e-08 |
| correlated | 2.9e-08 | 2.8e-07 |
| uncorrelated | 31 | 50 |

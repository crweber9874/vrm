# vrm: Visualization, Regression, and Marginal Effects Toolkit

`vrm` provides a unified toolkit for Bayesian and frequentist regression workflows in R. It wraps `brms` for ordinal, linear, logit, and nominal models with consistent interfaces for:
- **Model fitting** (`brms` wrappers for ordinal, linear, logit, nominal)
- **Posterior prediction and marginal effects** (posterior means, marginal effects, credible intervals)
- **Counterfactual analysis** (g-computation for individual treatment effects)
- **Propensity score weighting** (GLM, GBM, random forest methods with IPTW)
- **Data recoding utilities** (bulk survey recoding, common scale transformations)
- **Publication-quality visualization** (split violins, sunflower plots, ridgeplots, forest plots, marginal effect panels)

## Installation

```r
# Install from local source
devtools::install("/path/to/vrm")

# Or from GitHub (once published)
devtools::install_github("crweber9874/vrm")
```

## Quick Start

```r
library(vrm)

# Fit a Bayesian ordinal model
fit <- brms.ordinal(dv = "contestation_value", iv = "prepost * vote_trump",
                    data = mydata, cores = 4)

# Generate posterior predicted means
preds <- posterior_means(fit, iv_name = "prepost", moderator = "vote_trump",
                         data = mydata)

# Counterfactual g-computation
cf_data <- generate_counterfactual_data(mydata, treatment_var = "prepost")

# Propensity score weights
ps <- ps_weight(mydata, "treatment", ~ age + gender + education)

# Rescale to [0,1]
mydata$age_01 <- zero_one(mydata$age)
```

## Function Reference

### Model Fitting
| Function | Description |
|---|---|
| `brms.ordinal()` | Ordinal regression via brms |
| `brms.linear()` | Linear regression via brms |
| `brms.logit()` | Binary logit via brms |
| `brms.nominal()` | Nominal/multinomial via brms |
| `estimate_brms_models()` | Batch estimation wrapper |
| `fit_ordered_logit()` | Frequentist ordered logit (MASS::polr) |

### Posterior / Prediction
| Function | Description |
|---|---|
| `posterior_means()` | Posterior predicted means with interactions |
| `posterior_pme()` | Predictive marginal effects |
| `spreadDraw()` | Merge BRMS draws to secondary data |
| `summarize_predictions()` | Summarize draws with CIs |
| `predict_ordered_probs()` | Predicted probabilities from polr |
| `predict_logit_probs()` | Predicted probabilities from glm |

### Causal Inference
| Function | Description |
|---|---|
| `generate_counterfactual_data()` | Create counterfactual datasets for g-computation |
| `calculate_individual_effects()` | Individual-level treatment effects |
| `ps_weight()` | Propensity score estimation and IPTW weighting |

### Visualization
| Function | Description |
|---|---|
| `ggMargins_categories()` | Marginal effects for categorical outcomes |
| `plotSunflower()` | Sunflower distribution plots |
| `plotFlower()` | Connected sunflower plots |
| `plotPoint()` | Minimal dot plots |
| `plotMarginalEffect()` | Marginal effect error bar plots |
| `gg_posterior_ridges()` | Posterior ridgeline plots |
| `gg_pred_diff()` | Prediction difference plots |
| `combined_sunflower_marginal()` | 2x2 panel plots |

### Data Utilities
| Function | Description |
|---|---|
| `zero_one()` | Rescale variable to [0, 1] |
| `recodeColumn()` | Recode a single column |
| `recodeList()` | Bulk recode multiple columns |
| `five_r`, `four_r`, `five_n` | Common Likert recode vectors |

## License

MIT

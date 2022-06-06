Fitting models with parsnip
================
Mubarak Ganiyu
2022-06-06

### Package Installation

``` r
library(tidymodels)
```

    ## -- Attaching packages -------------------------------------- tidymodels 0.2.0 --

    ## v broom        0.8.0     v recipes      0.2.0
    ## v dials        0.1.1     v rsample      0.1.1
    ## v dplyr        1.0.9     v tibble       3.1.6
    ## v ggplot2      3.3.5     v tidyr        1.2.0
    ## v infer        1.0.0     v tune         0.2.0
    ## v modeldata    0.1.1     v workflows    0.2.6
    ## v parsnip      0.2.1     v workflowsets 0.2.1
    ## v purrr        0.3.4     v yardstick    0.0.9

    ## -- Conflicts ----------------------------------------- tidymodels_conflicts() --
    ## x purrr::discard() masks scales::discard()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()
    ## x recipes::step()  masks stats::step()
    ## * Use tidymodels_prefer() to resolve common conflicts.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v readr   2.1.1     v forcats 0.5.1
    ## v stringr 1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x readr::col_factor() masks scales::col_factor()
    ## x purrr::discard()    masks scales::discard()
    ## x dplyr::filter()     masks stats::filter()
    ## x stringr::fixed()    masks recipes::fixed()
    ## x dplyr::lag()        masks stats::lag()
    ## x readr::spec()       masks yardstick::spec()

``` r
tidymodels_prefer()
```

``` r
library(modeldata)
data(ames)

# or in one line
data(ames, package = "modeldata")
# Set the random number stream using `set.seed()` so that the results can be
# reproduced later.
set.seed(501)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
```

### Observing different modeling syntaxes

``` r
linear_reg() %>% set_engine("lm")
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
linear_reg() %>% set_engine("glmnet")
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: glmnet

``` r
linear_reg() %>% set_engine("stan")
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: stan

With translate()

``` r
linear_reg() %>% set_engine("lm") %>% translate()
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm 
    ## 
    ## Model fit template:
    ## stats::lm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

``` r
linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   penalty = 1
    ## 
    ## Computational engine: glmnet 
    ## 
    ## Model fit template:
    ## glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
    ##     family = "gaussian")

``` r
linear_reg() %>% set_engine("stan") %>% translate()
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: stan 
    ## 
    ## Model fit template:
    ## rstanarm::stan_glm(formula = missing_arg(), data = missing_arg(), 
    ##     weights = missing_arg(), family = stats::gaussian, refresh = 0)

``` r
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- lm_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_form_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## stats::lm(formula = Sale_Price ~ Longitude + Latitude, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)    Longitude     Latitude  
    ##  -129282723      -785980      1328939

``` r
lm_xy_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ## 
    ## Coefficients:
    ## (Intercept)    Longitude     Latitude  
    ##  -129282723      -785980      1328939

### Random forest Test

``` r
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  translate()
```

    ## Random Forest Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   trees = 1000
    ##   min_n = 5
    ## 
    ## Computational engine: ranger 
    ## 
    ## Model fit template:
    ## ranger::ranger(x = missing_arg(), y = missing_arg(), case.weights = missing_arg(), 
    ##     num.trees = 1000, min.node.size = min_rows(~5, x), num.threads = 1, 
    ##     verbose = FALSE, seed = sample.int(10^5, 1))

``` r
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger", verbose = TRUE) %>%
  set_mode("regression")
```

    ## Random Forest Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   trees = 1000
    ##   min_n = 5
    ## 
    ## Engine-Specific Arguments:
    ##   verbose = TRUE
    ## 
    ## Computational engine: ranger

### Use the model’s result

``` r
lm_form_fit %>% extract_fit_engine()
```

    ## 
    ## Call:
    ## stats::lm(formula = Sale_Price ~ Longitude + Latitude, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)    Longitude     Latitude  
    ##  -129282723      -785980      1328939

``` r
lm_form_fit %>% extract_fit_engine() %>% vcov()
```

    ##               (Intercept)    Longitude      Latitude
    ## (Intercept)  4.436113e+13 339521704130 -298977475561
    ## Longitude    3.395217e+11   3569820717    -124513793
    ## Latitude    -2.989775e+11   -124513793    6835274357

``` r
model_res <-
  lm_form_fit %>%
  extract_fit_engine() %>%
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
```

    ## [1] "matrix" "array"

``` r
param_est
```

    ##                 Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) -129282722.5 6660415.20 -19.41061 6.069756e-78
    ## Longitude      -785979.5   59747.98 -13.15491 3.484586e-38
    ## Latitude       1328939.4   82675.72  16.07412 3.137922e-55

#### Tabulating model results

``` r
tidy(lm_form_fit)
```

    ## # A tibble: 3 x 5
    ##   term           estimate std.error statistic  p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept) -129282723.  6660415.     -19.4 6.07e-78
    ## 2 Longitude      -785980.    59748.     -13.2 3.48e-38
    ## 3 Latitude       1328939.    82676.      16.1 3.14e-55

### Predictions

``` r
ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)
```

    ## # A tibble: 5 x 1
    ##     .pred
    ##     <dbl>
    ## 1 210673.
    ## 2 207465.
    ## 3 207175.
    ## 4 193826.
    ## 5 192687.

``` r
ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))
```

    ## # A tibble: 5 x 4
    ##   Sale_Price   .pred .pred_lower .pred_upper
    ##        <int>   <dbl>       <dbl>       <dbl>
    ## 1     213500 210673.      66702.     354643.
    ## 2     236500 207465.      63503.     351427.
    ## 3     175900 207175.      63228.     351122.
    ## 4     149900 193826.      49884.     337768.
    ## 5     126000 192687.      48751.     336624.

#### A complete test-run for modeling

``` r
tree_model <-
  decision_tree(min_n = 2) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_fit <-
  tree_model %>%
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(tree_fit, ames_test_small))
```

    ## # A tibble: 5 x 2
    ##   Sale_Price   .pred
    ##        <int>   <dbl>
    ## 1     213500 214754.
    ## 2     236500 214754.
    ## 3     175900 214754.
    ## 4     149900 154969.
    ## 5     126000 154969.

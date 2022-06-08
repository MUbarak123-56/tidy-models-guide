A Model Workflow
================
Mubarak Ganiyu
2022-06-07

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
    ## * Use suppressPackageStartupMessages() to eliminate package startup messages

``` r
tidymodels_prefer()
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")
```

### Workflow basics

``` r
lm_wflow <-
  workflow() %>% 
  add_model(lm_model)
lm_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: None
    ## Model: linear_reg()
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
lm_wflow <-
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Formula
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Sale_Price ~ Longitude + Latitude
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
lm_fit <- fit(lm_wflow, ames_train)
lm_fit
```

    ## == Workflow [trained] ==========================================================
    ## Preprocessor: Formula
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Sale_Price ~ Longitude + Latitude
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## 
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ## 
    ## Coefficients:
    ## (Intercept)    Longitude     Latitude  
    ##    -300.251       -2.013        2.782

``` r
predict(lm_fit, ames_test %>%  slice(1:3))
```

    ## # A tibble: 3 x 1
    ##   .pred
    ##   <dbl>
    ## 1  5.22
    ## 2  5.22
    ## 3  5.28

**Updating formula**

``` r
lm_fit %>% update_formula(Sale_Price ~ Longitude)
```

    ## == Workflow ====================================================================
    ## Preprocessor: Formula
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Sale_Price ~ Longitude
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

**Adding raw variables**

``` r
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c("Latitude","Longitude"))
lm_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Variables
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Outcomes: Sale_Price
    ## Predictors: c("Latitude", "Longitude")
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(ends_with("tude")))
```

    ## Warning: The workflow has no formula preprocessor to remove.

``` r
lm_wflow
```

    ## == Workflow ====================================================================
    ## Preprocessor: Variables
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Outcomes: Sale_Price
    ## Predictors: c(ends_with("tude"))
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

### Special formulas and in-line functions

``` r
library(multilevelmod)
multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  add_variables(outcome = Survived, predictors = c(Sex, Age, Parch)) %>% 
  add_model(multilevel_spec, formula = Survived ~ Sex + (Age | Parch))

library(titanic)
multilevel_fit <- fit(multilevel_workflow, data = titanic_train)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.629768 (tol = 0.002, component 1)

``` r
multilevel_fit
```

    ## == Workflow [trained] ==========================================================
    ## Preprocessor: Variables
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Outcomes: Survived
    ## Predictors: c(Sex, Age, Parch)
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Survived ~ Sex + (Age | Parch)
    ##    Data: data
    ## REML criterion at convergence: 774.3902
    ## Random effects:
    ##  Groups   Name        Std.Dev. Corr 
    ##  Parch    (Intercept) 0.155801      
    ##           Age         0.001659 -0.57
    ##  Residual             0.410970      
    ## Number of obs: 714, groups:  Parch, 7
    ## Fixed Effects:
    ## (Intercept)      Sexmale  
    ##      0.6685      -0.5458  
    ## optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings

### Creating Multiple Workflows at once

``` r
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)
```

``` r
library(workflowsets)
location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models
```

    ## # A workflow set/tibble: 4 x 4
    ##   wflow_id        info             option    result    
    ##   <chr>           <list>           <list>    <list>    
    ## 1 longitude_lm    <tibble [1 x 4]> <opts[0]> <list [0]>
    ## 2 latitude_lm     <tibble [1 x 4]> <opts[0]> <list [0]>
    ## 3 coords_lm       <tibble [1 x 4]> <opts[0]> <list [0]>
    ## 4 neighborhood_lm <tibble [1 x 4]> <opts[0]> <list [0]>

``` r
location_models$info[[1]]
```

    ## # A tibble: 1 x 4
    ##   workflow   preproc model      comment
    ##   <list>     <chr>   <chr>      <chr>  
    ## 1 <workflow> formula linear_reg ""

``` r
extract_workflow(location_models, id = "coords_lm")
```

    ## == Workflow ====================================================================
    ## Preprocessor: Formula
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Sale_Price ~ Longitude + Latitude
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
location_models <-
   location_models %>%
   mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
```

    ## # A workflow set/tibble: 4 x 5
    ##   wflow_id        info             option    result     fit       
    ##   <chr>           <list>           <list>    <list>     <list>    
    ## 1 longitude_lm    <tibble [1 x 4]> <opts[0]> <list [0]> <workflow>
    ## 2 latitude_lm     <tibble [1 x 4]> <opts[0]> <list [0]> <workflow>
    ## 3 coords_lm       <tibble [1 x 4]> <opts[0]> <list [0]> <workflow>
    ## 4 neighborhood_lm <tibble [1 x 4]> <opts[0]> <list [0]> <workflow>

``` r
location_models$fit[[1]]
```

    ## == Workflow [trained] ==========================================================
    ## Preprocessor: Formula
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ----------------------------------------------------------------
    ## Sale_Price ~ Longitude
    ## 
    ## -- Model -----------------------------------------------------------------------
    ## 
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ## 
    ## Coefficients:
    ## (Intercept)    Longitude  
    ##     -176.46        -1.94

### Evaluating the Test Set

``` r
final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res
```

    ## # Resampling results
    ## # Manual resampling 
    ## # A tibble: 1 x 6
    ##   splits             id               .metrics   .notes   .predictions .workflow
    ##   <list>             <chr>            <list>     <list>   <list>       <list>   
    ## 1 <split [2342/588]> train/test split <tibble [~ <tibble~ <tibble [58~ <workflo~

``` r
fitted_lm_wflow <- extract_workflow(final_lm_res)
```

``` r
collect_metrics(final_lm_res)
```

    ## # A tibble: 2 x 4
    ##   .metric .estimator .estimate .config             
    ##   <chr>   <chr>          <dbl> <chr>               
    ## 1 rmse    standard       0.160 Preprocessor1_Model1
    ## 2 rsq     standard       0.208 Preprocessor1_Model1

``` r
collect_predictions(final_lm_res) %>% slice(1:5)
```

    ## # A tibble: 5 x 5
    ##   id               .pred  .row Sale_Price .config             
    ##   <chr>            <dbl> <int>      <dbl> <chr>               
    ## 1 train/test split  5.22     2       5.02 Preprocessor1_Model1
    ## 2 train/test split  5.22     3       5.24 Preprocessor1_Model1
    ## 3 train/test split  5.28     5       5.28 Preprocessor1_Model1
    ## 4 train/test split  5.24    28       5.06 Preprocessor1_Model1
    ## 5 train/test split  5.31    39       5.60 Preprocessor1_Model1

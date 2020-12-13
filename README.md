# Function for Time Series Cross-Validation

Easily compare multiple time series models using sliding and/or expanding-window cross validation.

## Usage

First, set up your models. Use this format:

```r
models = list(
  # for each model, add a list in this format:
  list(
    enabled = TRUE, # (optional) use enabled = FALSE to exclude a model from CV.
    name = 'Model Name',
    fit = function(train, train_xreg, ...){
      # function to fit the model.
      #  in: 
      #    train: time series during the training period
      #    train_xreg: xreg features during the training period
      #    ... extra arguments passed to the cv function will also be passed to model functions.
      #   out: must return a fitted model
    },
    forecast = function(m, h, train, train_xreg, ...){
      # (optional) function to forecast
      #  if left blank will use: function(m, h, ...) forecast(m, h)$mean
      #  in: 
      #    m: model output from fit
      #    h: forecast horizon
      #    train: time series during the training period
      #    train_xreg: xreg features during the training period
      #    ... extra arguments passed to the cv function will also be passed to model functions.
      #  out: numeric vector of length h containing point forecasts (no confidence intervals)
    },
    residuals = function(m, ...){
      # (optional) function to get model residuals.
      # if left blank will use: function(m, h, ...) as.numeric(tail(idt$model$residuals(im), h))
      # in:
      #    m: model output from fit
      #    ... extra arguments passed to the cv function will also be passed to model functions.
      # out: numeric vector containing residuals (actual - predicted/fitted value) from the model.
    }
  ),
  ...
)
```

Then choose your CV paremeters.

```r
modelcv = function(
    train, # time series after removing holdout.
    xreg = NULL, # additional training features to pass to model, with window equivalent to train.
    models, # list with model functions.
    windowsize = 160, # width of sliding window (and minimum expanding window).
    h = 12, # forecast horizon (number of periods to forecast, and residuals to use for measuring training error).
    numcvs = NULL, # number of CVs to run. the CV will take the latest possible windows.
    runtypes = c('sliding', 'expanding'), # types of CVs to run.
    verbose = TRUE, # print information about run status?
    numcores = 2, # number of parallel processes to use. set to 1 for easier troubleshooting.
    packages = c(), # any packages (other than forecast) required for your models. will be passed to parallel processes.
    ... # other args that will be passed to modeling functions.
){
```

See an example at [example.R](https://github.com/superchordate/time-series-cv/blob/main/examples/example.R).

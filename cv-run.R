require(easyr)
begin()

require(forecast)
require(vars)

source('cv-fn.R')

dt = read.any('daily.csv')

# remove holdout.
test = tail(dt, 7)
train = head(dt, nrow(dt) - 7)

models = list(
    
    # example:
    # add an entry to this list for each model.
    list(
      enabled = FALSE,
      # give your model a name to be used in charts, etc.
      name = 'ARIMA(1,0,1)[0,1,2][24]',
      
      # function to fit the model. 
      #   in: y (time series), ... (unused inputs, add this for flexibility)
      #   out: model object
      fit = function(y, ...){
        m = Arima(
          y,
          order = c(1, 0, 1),
          seasonal = list(order = c(0, 1, 2), period = 24)
        )
        return(m)
      }
      
      # function to forecast.
      #   in: m (output from fit), h (forecast horizon)
      #   out: point forecasts (no confidence intervals)
      #   if left blank will use: function(object, h, ...) forecast(object, h)$mean
      
    ),
  
    # VAR(2)
    list(
      name = 'VAR(2)',
      fit = function(y, xreg){
        VAR(ts(cbind(y, xreg$out_temperature), frequency = 7), p = 2, type = "both", season = 7)
      },
      forecast = function(m, h, ...) head(data.frame(forecast(m, h = h, frequency = 7))$Point.Forecast, h),
      residuals = function(m) residuals(m)[, 'y']
    ),
    
    # VAR(3)
    list(
      name = 'VAR(3)',
      fit = function(y, xreg){
        VAR(ts(cbind(y, xreg$out_temperature), frequency = 7), p = 3, type = "both", season = 7)
      },
      forecast = function(m, h, ...) head(data.frame(forecast(7))$Point.Forecast, h),
      residuals = function(m) residuals(m)[, 'y']
    ),
    
    # Fourier (K = 2)
    list(
      name = 'Fourier (K = 2)',
      fit = function(y, xreg){
        Arima(          
          ts(y, frequency = 7),
          xreg = fourier(x = ts(y, frequency = 7), K = 2)
        )
      },
      forecast = function(m, h, y, ...){
        forecast(m, xreg = fourier(x = ts(y, frequency = 7), K = 2, h = h))$mean
      }
    ),
    
    # Fourier (K = 3)
    list(
      name = 'Fourier (K = 3)',
      fit = function(y, xreg){
        Arima(          
          ts(y, frequency = 7),
          xreg = fourier(x = ts(y, frequency = 7), K = 3)
        )
      },
      forecast = function(m, h, y, ...){
        forecast(m, xreg = fourier(x = ts(y, frequency = 7), K = 3, h = h))$mean
      }
    ),
    
    # tbats
    list(
      name = 'tbats',
      fit = function(y, xreg){
        tbats(ts(y, frequency = 7))
      },
      bic = function(m) as.numeric(NA) # BIC is not implemented for TBATS
    ),
    
    # Reg w/ ARIMA errors
    list(
      name = 'Reg w/ ARIMA errors',
      fit = function(y, xreg){
        xreg$is_weekend = 1 * xreg$is_weekend
        Arima(
          ts(y, frequency = 7),
          order = c(0,0,1),
          seasonal = list(order = c(1,0,0), period = 7),
          xreg = as.matrix(xreg[, c('out_temperature', 'is_weekend')])
        )
      },
      forecast = function(m, h, xreg, ...){
        xreg$is_weekend = 1 * xreg$is_weekend
        forecast(
          m, 
          h = h, 
          # repeat last row h times.
          xreg = as.matrix(xreg[ rep(length(xreg), h), ][, c('out_temperature', 'is_weekend')])
        )$mean
        
      }
    ),
    
    # ETS
    list(
        name = 'ets(model = "MAM")',
        fit = function(y, ...){
            m = ets(ts(y, frequency = 24), model = 'MAM')
            return(m)
        }
    )
)

cv = modelcv(
    y = train$y,
    xreg = train[, c('out_temperature', 'is_weekend')],
    models = models,
    windowsize = 7 * 3,
    h = 7,
    numcvs = 10,
    runtypes = c('sliding', 'expanding'),
    numcores = 8
)

cv$summary %>%
  group_by(model, window) %>% 
  summarize(
    forecast_horizons = length(forecast_horizon),
    cv_count = mean(cv_count),
    mae_fit = mean(mae_fit), 
    mae_test = mean(mae_test), 
    rmse_fit = mean(rmse_fit), 
    rmse_test = mean(rmse_test), 
    mean_bic = mean(mean_bic)
  ) %>%
  arrange(rmse_test) %>% View

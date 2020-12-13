# standard easyr setup (https://github.com/oliver-wyman-actuarial/easyr)
require(easyr)
require(forecast)
begin()

# read the function.
source('../cv-fn.R')

# read data and remove your holdout.
# data is from https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction
# Luis M. Candanedo, Veronique Feldheim, Dominique Deramaix, Data driven prediction models of energy use of appliances in a low-energy house, Energy and Buildings, Volume 140, 1 April 2017, Pages 81-97, ISSN 0378-7788
dt = read.any('../data/daily.csv')
test = tail(dt, 7)
train = head(dt, nrow(dt) - 7)

# set up models.
models = list(

    list(
      name = 'ARIMA(2,0,2)',
      fit = function(train, ...) Arima(
        train,
        order = c(2, 0, 2)
      ) 
    ),

    list(
      name = 'Fourier (K = 2)',
      fit = function(train, ...) Arima(          
        ts(train, frequency = 7),
        xreg = fourier(x = ts(train, frequency = 7), K = 2)
      ),
      forecast = function(m, h, train, ...) forecast(m, xreg = fourier(x = ts(train, frequency = 7), K = 2, h = h))$mean
    ),
    
    list(
      name = 'ARIMAX',
      fit = function(train, train_xreg, ...) Arima(
        ts(train, frequency = 7),
        order = c(0,0,1),
        seasonal = list(order = c(1,0,0), period = 7),
        xreg = train_xreg$out_temperature
      ),
      forecast = function(m, h, train_xreg, ...) forecast(
        m, 
        h = h, 
        # xreg = repeat last value h times (naive)
        xreg = rep(tail(train_xreg$out_temperature, 1), h)
      )$mean
    ),
    list(
        name = 'ETS(model = "MAM")',
        fit = function(train, ...)  ets(ts(train, frequency = 7), model = 'MAM')
    )
)

# run the CV.
cv = modelcv(
    train = train$y,
    xreg = train[, c('out_temperature'), drop = FALSE],
    models = models,
    windowsize = 7 * 3, # 3 weeks
    h = 7, # 1 week
    numcvs = 7 * 3, # 3 weeks
    runtypes = c('sliding', 'expanding'),
    numcores = 1
)

head(cv$summary)

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
  arrange(rmse_test)
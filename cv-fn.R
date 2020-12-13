require(glue)
require(dplyr)
require(doSNOW)

# create a list with information for all the models.
modelcv = function(
    train, # time series
    xreg = NULL, # additional features to pass to model fitting.
    models, # list with model functions.
    windowsize = 160, # width of sliding window (and min expanding window).
    h = 12, # forecast horizon (number of periods to forecast).
    numcvs = NULL, # number of CVs to run. CVs will use the latest possible data.
    runtypes = c('sliding', 'expanding'), # types of CVs to run.
    verbose = TRUE, # print information about run status.
    numcores = 2, # number of parallel processes to use. set to 1 for easier troubleshooting.
    packages = c(), # add'l packages required for your models. will be passed to parallel processes.
    ... # other args that will be passed to modeling functions.
){
    
    # calculating where to start is a bit tricky.
    #  first we calculate what the last training window will be (depends on forecast interval h)
    #  then we subtract the number of cvs from that to get our starting point
    lastwindow = (length(train) - h - windowsize + 1):(length(train) - h)
    startat = if(is.null(numcvs)){ 1 }  else { lastwindow[1] - numcvs + 1 }
    iterateoverstarts = startat:lastwindow[1]
    
    if(!is.null(numcvs) && windowsize + numcvs + h > length(train)) stop(glue('
       windowsize + numcvs + h ({windowsize} + {numcvs} + {h} = {windowsize + numcvs + h} exceeds number of observations ({length(y)})) 
    '))
    
    # set up the function to use at each iteration.
    dofn = function(idt){
        
        #if(grepl('ARIMAX', idt$model$name)) browser()
        
        # fill missing items.
        h = length(idt$test)
        if(is.null(idt$model$forecast)) idt$model$forecast = function(m, h, ...){
            forecast(m, h = h)$mean
        }
        if(is.null(idt$model$residuals)) idt$model$residuals = function(m) residuals(m)

        # fit, forecast, residuals and bic can all fail. 
        # run them separate so we can give a helpful error.
        im = tryCatch({
            idt$model$fit(train = idt$train, train_xreg = idt$train_xreg, ...)
        }, error = function(e) stop(glue('model$fit failed on [{idt$model$name}]: {e}')))
        
        iforecast = tryCatch({
            as.numeric(idt$model$forecast(m = im, h = h, train = idt$train, train_xreg = idt$train_xreg, ...))
        }, error = function(e) stop(glue('model$forecast failed on [{idt$model$name}]: {e}')))
        
        iresiduals = tryCatch({
            as.numeric(tail(idt$model$residuals(im, ...), h))
        }, error = function(e) stop(glue('model$residuals failed on [{idt$model$name}]: {e}')))
        
        tryCatch({
           ibic = BIC(im)
           if(length(ibic) == 0) ibic = as.numeric(NA)
        }, error = function(e) stop(glue('model$bic failed on [{idt$model$name}]: {e}')))
        
        data.frame(
            window = idt$windowtype,
            model = idt$model$name,
            train_from = idt$train_from,
            train_thru = idt$train_thru,
            test_from = idt$test_from,
            test_thru = idt$test_thru,
            bic = ibic,
            forecast_horizon = 1:h,
            forecasterr = iforecast - idt$test,
            forecast = list(iforecast),
            test = list(idt$test),
            modelerr = iresiduals
        )        
        
    }

    # build the list of CV info.
    if(verbose) cat('\n Building test/train datasets.')
    dt = list()
    for(iwindowstart in iterateoverstarts){

        # expanding window.        
        if('expanding' %in% runtypes) for(imodel in models) if(is.null(imodel$enabled) || imodel$enabled){

            idt = list(
                train_from = 1,
                train_thru =(iwindowstart + windowsize - 1),
                test_from = (iwindowstart + windowsize),
                test_thru = (iwindowstart + windowsize + h - 1)
            )
            idt$train = train[idt$train_from:idt$train_thru]
            idt$test = train[idt$test_from:idt$test_thru]
            if(!is.null(xreg)){
                idt$train_xreg = xreg[idt$train_from:idt$train_thru, ]
                idt$test_xreg = xreg[idt$test_from:idt$test_thru, ]
            }

            idt$model = imodel
            idt$windowtype = 'expanding'            

            dt[[length(dt) + 1]] <- idt
            rm(idt)

        }

        if('sliding' %in% runtypes) for(imodel in models) if(is.null(imodel$enabled) || imodel$enabled){

            idt = list(
                train_from = iwindowstart,
                train_thru =(iwindowstart + windowsize - 1),
                test_from = (iwindowstart + windowsize),
                test_thru = (iwindowstart + windowsize + h - 1)
            )
            
            idt$train = train[idt$train_from:idt$train_thru]
            idt$test = train[idt$test_from:idt$test_thru]
            if(!is.null(xreg)){
                idt$train_xreg = xreg[idt$train_from:idt$train_thru, ]
                idt$test_xreg = xreg[idt$test_from:idt$test_thru, ]
            }
            
            idt$model = imodel
            idt$windowtype = 'sliding'

            dt[[length(dt) + 1]] <- idt
            rm(idt)

        }

    }
    
    # make sure each of the models can be run.    
    for(i in 1:length(models)) if(is.null(models[[i]]$enabled) || models[[i]]$enabled){
        if(verbose) cat('\n Checking ', models[[i]]$name)
        # find the last CV datset that has this model.
        for(j in length(dt):1) if(dt[[j]]$model$name == models[[i]]$name) break
        # run the CV function on that data and model.
        dofn(dt[[j]])
    }

    # run the CVs in parallel.
    if(verbose) cat(glue('\n \n Running {length(dt)} CV models \n'))
    if(is.null(numcores) || numcores == 1){
        # if 1 core, just use lapply for easier troubleshooting.
        results = list()
        for(n in 1:length(dt)){
            if(verbose && n %% 10 == 0) cat("\rCV ", n, " of ", length(dt), " complete \n")
            results[[n]] <- dofn(dt[[n]])
        }
    } else {
        if(verbose) cat('\t in parallel')
        cl = makeSOCKcluster(numcores)
        registerDoSNOW(cl)
        results = tryCatch({
            foreach(
                i = dt, 
                .packages = c('glue', 'forecast', packages),
                .options.snow = if(verbose) list(progress = function(n) if(n %% 10 == 0) cat("\rCV ", n, " of ", length(dt), " complete \n"))
            ) %dopar% dofn(i)
        # on error, stop the cluster.
        }, error = function(e){
            stopCluster(cl)
            stop(e)
        })
        stopCluster(cl)
    }

    # combine into a single data frame.
    results = bind_rows(results)
    
    return(list(
        results = results, 
        summary = results %>%
            group_by(model, window, forecast_horizon) %>% 
            summarize(
                cv_count = n(),
                mae_fit = mean(abs(modelerr)),
                mae_test = mean(abs(forecasterr)),
                rmse_fit = sqrt(mean(modelerr ** 2)),
                rmse_test = sqrt(mean(forecasterr ** 2)),
                mean_bic = mean(bic)
            ) %>%
            arrange(forecast_horizon, rmse_test)
    ))

}

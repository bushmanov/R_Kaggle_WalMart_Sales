# R_Kaggle_WalMart_Sales

### Just another Kaggle competition

https://www.kaggle.com/c/walmart-recruiting-sales-in-stormy-weather

The task was to predict the sales of weather sensitive products in a 3-day window around an event: rain or anow storm.

The features are weather time-series (temperature, humidity, dow point etc) and sales time-series.

The models tried are:

- ARIMAX (`forecast` package with `xreg` as exogenous predictors)
- RF and regularized GLM (l1 and l2) with induced time dimension:
    - days prior- post-event
    - moving avragaes
    - 7-day lagging
    - seasonal dummies

Model performance was measured on log-loss

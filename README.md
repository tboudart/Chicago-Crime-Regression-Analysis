# Chicago-Crime-Regression-Analysis
As part of a group project, I developed separate regression models using R to predict the daily number of batteries and robberies in Chicago using four different datasets. I tested interactive and second-order terms and used stepwise feature selection to find the best model with the given data. I tested several potential models using cross-validation and chose the model that minimized the cross-validation errors while striking a balance with the model's simplicity. I checked the residual assumptions and both models exhibit autocorrelation as indicated by rejecting the null hypothesis of the Durbin-Watson Test. If I had more time, I would try using an ARMA model instead of multiple regression.

The Crime data came from Kaggle at https://www.kaggle.com/chicago/chicago-crime/metadata
The Chicago Park District Event Permit data came from the Chicago Data Portal at https://data.cityofchicago.org/Events/Chicago-Park-District-Event-Permits/pk66-w54g
The weather data was from the Midwestern Regional Climate Center found athttps://mrcc.illinois.edu/
The unemployment data is from the United States Bureau of Labor Statistics at https://www.bls.gov/charts/employment-situation/civilian-unemployment-rate.htm

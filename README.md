# ModelCompareApp

A Shiny app to compare 2 models with a Lorenz curve/Gini plot after running a regression.

## R Code

R code showing postestimation values - for InSample and OutOfSample (currently when the dependant variable is NULL)  

The name of the sample indicator is `sample` - 0 for InSample; 1 for OutOfSample.
 
The dependant variable is `keshel_Future` - this will be generalized in the future.  

## Includes

* Postestimation values - Gini and AUC 
* Gini plot

## Dependencies
* ggplot2
* broom
* dplyr
* Shiny
  
## Todos
* Generalize dependant variable with a widget
* Add labels to curves in graphs
* Make an option for non-regression calculation

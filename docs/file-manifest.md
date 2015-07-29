### File Manifest
My work on this project can be found in the `enhancer_pred/mlAlgoAW` directory.

`enhancer_pred/mlAlgoAW/analysis` contains the R code to for the entire pipeline.    
`enhancer_pred/mlAlgoAW/data`  contains the final results of the prediction.    
`enhancer_pred/mlAlgoAW/AdamPlots` has plots for explority analysis, model type selection, and model tuning.    
`enhancer_pred/mlAlgoAW/scripts` is the location of master scripts to run all of my analysis.    

### /enhancer_pred/mlAlgoAW/analysis
`main.R` contains the functions to run the find the best ml algorithm type, optimize gbm models, and make a prediction on the genome. For each species/tissue type, a different function exists. A good example of this is the function `main.heart`, on line 146.
`dataInput.R` is responsible for loading, cleaning, and returning the datasets needed for an analysis. 
`runPredOnData.R` implements `accumMlAlgos`, that tests different kinds of ml algorithms, and `getBestGBMparams`, which optimizes a gbm learner.
`plotResults.R` contains the ggplot2 code for both exploratory and algorithmic performance visualization
`predLib.R` is a collection of various utility and helper functions used during development.     

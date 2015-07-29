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

### /enhancer_pred/mlAlgoAW/scripts
`0_run_me.r` evaluates the what category of ml algo is best.    
`gbm_run_me.r` finds the best parameters for the gbm model.    
`runGbmOnGenome.r` trains an optimized gbm model and makes predictions on the genome.    

Contains the the results from the first round of machine learning, and stores the training data, after cleaning, that is used to create a model. To keep the size of the repo down, Dropbox was used to store the majority of large, non-changing dta files.      

### /enhancer_pred/mlAlgoAW/AdamPlots
`algocompare` directory contains plots on the features of the dataset, and the results of the first model comparision for each species/tissue of interest.
`gbm` has the performance results of running the gbm algorithm with a series of parameters.  The R call, and data, that calls the gbm function is also saved here.
`gbmgenomicprediction` visualizes the algorithm performance and feature importance for the gbm call that created the predictions on the entire genome.  

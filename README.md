#Enhancer Prediction Project

# Objective
## Find regions of the genome likely to be enhancer regions in different cell types and species, critical regions responsible for regulating a cell's protein production. 

# Approach:
## Use a training set of known enhancers, then make predictions on the mouse genome. The predictions from our group, along with others, will be combined, and the most predicted regions will be tested in vivo for enhancer activity. 

# Data
## Epigenetic information was measured in a variety of tissues from both mouse and human genome. The training set consisted of genomic regions which are known to be enhancers and regions that are not. For the prediction set, genome was divided into discrete regions, and a vector of epigenitic information was assigned to each region, matching the data vectors of the training set. The top 10,000 most likely regions were submitted to the ENCODE consortium.  


# Machine Learning Pipeline
## For each species/tissue type a seperate training and prediction was performed. Data was imported and cleaned. Next, a series of machine learning algorithms was applied The first series consisted of many different types, and the next series consisted of optimizing the parameters and data normalization for the best performing model, Generalized Boosted Regression Models. To better understand experimental results, performance measures of the prediction algorithms were plotted.  


# Conclusions
## Generalized Boosting gives the best results for the test set. Unfortunately, the biological validation of our results, combined with the other groups, were not promising. 

# Lessons Learned
## Projects are much easier if all major pipeline components are seperate
## Testing Multiple algorithms allows you to select the best perfoming algorithm without specific knowledge of how that algorithms set of decision boundaries affects your data
## Premature optimization can be reduced by surveying a field of algorithms before focusing efforts on optimizing a complex algorithm



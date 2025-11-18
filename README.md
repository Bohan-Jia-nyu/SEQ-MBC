# SEQ-MBC
## Model Code
We provie the code of 4-4 and 5-5 models as examples.

### 44_combine.R
This file contains the R and STAN code that build the 4–4 semi-concordant, 4–4 fully-concordant, and 4–4 independent models. The R code first loads the MVAD dataset, transforms it to the long format, creates the outcome variable, and selects the variables needed in the model. Then it picks a training set where 80% of the items are randomly selected and the rest of them become the testing set. After that, the training set is transformed to the list format that fits the STAN running.

According to the different seeds generated systematically, the 4–4 semi-concordant, 4–4 fully-concordant, and 4–4 independent STAN code are presented. The data part introduces: the total number of observations, the number of times when one observation is recorded, the number of states, the max power of time, the number of items, all the observed states, the design matrices used for modeling the latent growth curve, and all the observed outcomes. The parameter part defines: discrete auto-regressive “error,” population-level class-specific probabilities, parameters used for the latent growth curve, class-specific mean outcomes in the second phase, the shared standard deviation across the classes in the second phase, and the simplexes of the second-phase classes that depend on the classes from the first phase. The transformed parameters part creates the likelihood function using the data and the parameters provided in the first two parts. The model part shows the priors used for all the parameters. The generated quantities part calculates the inverse multinomial logit function for every state by using the posterior distribution of all the parameters.

The R code then set the initial points for all the parameters, the total number of the iterations, the number of burned iterations and the number of chains for running. 

### 55_combine.R
This file contains the R and STAN code that build the 5–5 semi-concordant, 5–5 fully-concordant, and 5–5 independent models. The R code first loads the MVAD dataset, transforms it to the long format, creates the outcome variable, and selects the variables needed in the model. Then it picks a training set where 80% of the items are randomly selected and the rest of them become the testing set. After that, the training set is transformed to the list format that fits the STAN running.

According to the different seeds generated systematically, the 5–5 semi-concordant, 5–5 fully-concordant, and 5–5 independent STAN code are presented. The data part introduces: the total number of observations, the number of times when one observation is recorded, the number of states, the max power of time, the number of items, all the observed states, the design matrices used for modeling the latent growth curve, and all the observed outcomes. The parameter part defines: discrete auto-regressive “error,” population-level class-specific probabilities, parameters used for the latent growth curve, class-specific mean outcomes in the second phase, the shared standard deviation across the classes in the second phase, and the simplexes of the second-phase classes that depend on the classes from the first phase. The transformed parameters part creates the likelihood function using the data and the parameters provided in the first two parts. The model part shows the priors used for all the parameters. The generated quantities part calculates the inverse multinomial logit function for every state by using the posterior distribution of all the parameters.

The R code then set the initial points for all the parameters, the total number of the iterations, the number of burned iterations and the number of chains for running. 


## Model Evaluation Code

### RMSE.R & RMSE_training.R
### entropy.R & entropy_test.R
### matching.R
### plots_draw.R

## Retrospective Analysis Code
### backward.R

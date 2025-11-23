# SEQ-MBC
## Model Code
The code of 4-4 and 5-5 models are provided as examples.

### 44_combine.R
This file contains the R and STAN code that build the 4–4 semi-concordant, 4–4 fully-concordant, and 4–4 independent models. The R code first loads the MVAD dataset, transforms it to the long format, creates the outcome variable, and selects the variables needed in the model. Then it picks a training set where 80% of the items are randomly selected and the rest of them become the testing set. After that, the training set is transformed to the list format that fits the STAN running.

According to the different seeds generated systematically, the 4–4 semi-concordant, 4–4 fully-concordant, and 4–4 independent STAN code are presented. The data part introduces: the total number of observations, the number of times when one observation is recorded, the number of states, the max power of time, the number of items, all the observed states, the design matrices used for modeling the latent growth curve, and all the observed outcomes. The parameter part defines: discrete auto-regressive “error,” population-level class-specific probabilities, parameters used for the latent growth curve, class-specific mean outcomes in the second phase, the shared standard deviation across the classes in the second phase, and the simplexes of the second-phase classes that depend on the classes from the first phase (For the independent model, there is only one simplex in the second-phase and it does not depend on the classes from the first phase). The transformed parameters part creates the likelihood function using the data and the parameters provided in the first two parts. The model part shows the priors used for all the parameters. The generated quantities part calculates the inverse multinomial logit function for every state across time by using the posterior distribution of all the parameters.

The R code then set the initial points for all the parameters, the total number of the iterations, the number of burned iterations and the number of chains for running. 

### 55_combine.R
This file contains the R and STAN code that build the 5–5 semi-concordant, 5–5 fully-concordant, and 5–5 independent models. The R code first loads the MVAD dataset, transforms it to the long format, creates the outcome variable, and selects the variables needed in the model. Then it picks a training set where 80% of the items are randomly selected and the rest of them become the testing set. After that, the training set is transformed to the list format that fits the STAN running.

According to the different seeds generated systematically, the 5–5 semi-concordant, 5–5 fully-concordant, and 5–5 independent STAN code are presented. The data part introduces: the total number of observations, the number of times when one observation is recorded, the number of states, the max power of time, the number of items, all the observed states, the design matrices used for modeling the latent growth curve, and all the observed outcomes. The parameter part defines: discrete auto-regressive “error,” population-level class-specific probabilities, parameters used for the latent growth curve, class-specific mean outcomes in the second phase, the shared standard deviation across the classes in the second phase, and the simplexes of the second-phase classes that depend on the classes from the first phase (For the independent model, there is only one simplex in the second-phase and it does not depend on the classes from the first phase). The transformed parameters part creates the likelihood function using the data and the parameters provided in the first two parts. The model part shows the priors used for all the parameters. The generated quantities part calculates the inverse multinomial logit function for every state across time by using the posterior distribution of all the parameters.

The R code then set the initial points for all the parameters, the total number of the iterations, the number of burned iterations and the number of chains for running. 


## Model Evaluation Code

### RMSE.R & RMSE_training.R
The likelihood of each path in the latent growth model is computed for every item in the training/testing dataset, using the posterior means of all parameters. Note that the outcome likelihoods are not included in this calculation. For each item, the path-specific likelihoods are normalized by dividing each path likelihood by the sum of all path likelihoods, producing item-specific weights for each path. For the fully-concordant and semi-concordant models, the posterior means of the outcomes are marginalized by the class-specific simplex in the second phase to obtain the path-specific estimated outcomes. For the independent model, the posterior mean of the outcomes is marginalized using the single simplex in the second phase, resulting in the same set of estimated outcomes for all paths. Next, the path-specific estimated outcomes are marginalized using the item-specific weights to obtain the individual-level estimated outcomes. The squared differences between these estimated outcomes and the actual outcomes are summed and divided by the total number of items in the training/testing dataset. The training/testing RMSE is then the square root of this quantity.

### entropy.R & entropy_test.R
The item-specific weights for each path are computed using the same procedure as in RMSE.R and RMSE_training.R. The entropy is then calculated directly from these weights.

### matching.R
The item-specific weights for each path are computed using the same procedure as in RMSE.R and RMSE_training.R. Each item is then classified into a class according to the path with the highest weight. The 3–3 independent model, 4–4 independent model, and 5–5 independent model are treated as the baseline models. Agreement tables between each baseline model and the models that have the same number of classes in the first phase are constructed based on these classification results. Keeping the class order fixed for the baseline models, the classes in the other models are re-ordered to maximize the sum of the diagonal elements in each agreement table. The matching accuracies are then calculated by dividing these maximum diagonal sums by the number of items in the training set.

### plots_draw.R
Since the inverse multinomial logit functions for each state in the STAN code are computed from the posterior distribution of the parameters, each inverse multinomial logit function at a specific state and time has the same number of samples as the total number of posterior draws. For visualization, the last 40 samples of each inverse multinomial logit function are selected to generate the plots. Class by class, one latent growth curve for each state across time is produced by displaying a set of corresponding multinomial logit function samples at different time points. Thus, for each state, 40 curves are shown within each class, illustrating both the overall trend and the variability. All models are re-ordered according to the matching results. For any independent or any semi-concordant model, the mean outcomes are shown in the plot with an ascending orders. For any fully-concordant models, the mean outcomes are shown in the plot with same the order of the classes in the first phase.The population-level class-specific probabilities and the simplexes in the second phase are also re-ordered accordingly and presented in the plots. 

## Retrospective Analysis Code

### backward.R
The 5–5 semi-concordant model and the 5–5 fully-concordant model are used for the retrospective analysis. Following the class ordering and the ordering of the mean outcomes used in the plot generation, the probability of a path given an observed outcome value Y is computed as 
```math
P(path_{k_1^*}|Y)=\sum_{k_2=1}^{K_2}\frac{P(Y|path_{k_1^*},outcome_{k_2}) \times P(outcome_{k_2}|path_{k_1^*}) \times P(path_{k_1^*})}{\sum_{k_1=1}^{K_1} \sum_{k_2=1}^{K_2}P(Y|path_{k_1},outcome_{k_2})\times P(outcome_{k_2}|path_{k_1})\times P(path_{k_1})}
```
Here, Y takes values in {0,3,6,9,12}.
The terms
```math 
P(path_{k_1}), k_1=1,...,K_1
```
are the population-level class-specific probabilities, and
```math
P(outcome_{k_2}|path_{k_1}), k_2 = 1,....,K_2
```
are the simplex weights corresponding to the second-phase outcome classes. 
The likelihood term is given by
```math
P(Y|path_{k_1},outcome_{k_2})=Normal(Y,outcome_{k_2}, \sigma^2)
```
where 
```math
\sigma
```
 is the shared standard deviation across the second-phase classes.
The item-specific weights for each path are computed using the same procedure as in RMSE.R and RMSE_training.R. The item-specific contributions for each path are then obtained by normalizing these weights:
```math
contribution_{k_1,i}=\frac{\omega_{k_1,i}}{\sum_{i} \omega_{k_1,i}}
```
By marginalizing the binary outcomes male and fmpr over these contributions,
```math
P(male|path_{k_1}), P(fmpr|path_{k_1})
```
are obtained.
Finally, the retrospective probabilities are calculated as
```math
P(male|Y)=\sum_{k_1=1}^{K_1}P(male|path_{k_1}) \times P(path_{k_1}|Y)
```
```math
P(fmpr|Y)=\sum_{k_1=1}^{K_1}P(fmpr|path_{k_1}) \times P(path_{k_1}|Y)
```
These results provide the retrospective analysis for the outcome Y.

## Note
Each type of model was run 10 times under 10 different seeds, using one chain at a time. This resulted in 10 WAIC values for each model type. For each type, all analyses described above were carried out using the model run with the lowest WAIC. The average WAIC for each model type was also computed for model comparison and for selecting the best-performing model.

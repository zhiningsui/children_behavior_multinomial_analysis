# children_behavior_multinomial_analysis
## Description 
The purpose of this project is to revisit observational data collected in rural Taiwan in the 1950s by Arthur Wolf and employ a multilevel multinomial model in addition to traditional anthropological analysis to reveal the patterns of proactive behaviors of young kids.

We run four models, 
- model_i: a multinomial model with only individual random effects from initiator and recipient.
- model_iF: a multinomial model with individual random effects from initiator and recipient, as well as fixed effects from covariates (age at observation, gender, whether the initiator and recipient were in the same household).
- model_ih: a multinomial model with both individual and household random effects from initiator and recipient.
- model_ihF: a multinomial model with both individual and household random effects from initiator and recipient, as well as fixed effects from covariates (age at observation, gender, whether the initiator and recipient were in the same household).

## Folder Structure
### `data`: This folder contains two files.
1. `proactive_behaviors.RData`: a pre-processed dataframe of the proactive behavioral data. All children recorded in this dataframe aged 0 to 12 at the end of the field study.
2. `proactive-multinomial-model-data.csv`: a file containing all processed variables required to run the multinomial analysis, which is processed from `proactive_behaviors.RData`.
### `code`: This folder contains two R files.
1. `run_multinomial.R`: the code required to run the multinomial analysis.
2. `results_analysis_newest.R`: the code required to analyze the results from fitting the models. Figures and tables shown in the manuscript are created using this R file.

### `output`: This folder contains the following files.
1. Files ends with `*_corr.csv`: contains the correlation between the random effects (rows starting with "Rho").
2. Files ends with `*_random_effect.csv`: contains the variance of the random effects (rows starting with "sigma") and the intercepts (rows starting with "a").
3. `WAIC.csv`: contains the WAIC values for four models.
4. In the subfolder `figure`:
- Figures starts with `heatmap` are the heatmaps for the correlation between individual random effect across different behaviors.
- Figures starts with `predicted` are the predicted probabilities of different behaviors at different values of covariate as specified in the filenames.
5. In the subfolder `table`:
- Files ends with `*_corr.csv` are the correlation matrices between individual random effects of four models.

## Usage
To run the analysis, you can use the code provided in the `code` folder. The results can be analyzed using the `results_analysis_newest.R` file. 



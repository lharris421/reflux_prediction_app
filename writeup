# Statements

Logistic GEE models were fit using PROC GENMOD in SAS.

The models were deployed using RShiny for ease of access, taking in patient information and providing predictions for 1 and 2 year resolution.

# Methods

Age, sex, laterality of reflux, and presenting symptom were available for all patients. Additional measurements of interest included UDR, bladder volume at onset of reflux, and reflux grade. These measurements were not available for all patients at all visits, so models were fit based on the availability of each measurement. Five predictor sets were considered: (1) UDR alone, (2) volume at onset alone, (3) reflux grade alone, (4) UDR and reflux grade, and (5) volume at onset and reflux grade.

For each predictor set, separate logistic generalized estimating equation (GEE) models were fit to predict resolution at 1 year and at 2 years, for a total of 10 models. Models were fit using PROC GENMOD in SAS with a binomial distribution, logit link, and an exchangeable correlation structure to account for repeated measurements within patients.

Model performance was evaluated using 10-fold cross-validation with ROC curves generated for each model. For each pair of 1- and 2-year models, the threshold was set to achieve at least 50\% sensitivity for the 1-year model and applied unchanged to the corresponding 2-year model to reduce discordant predictions between timepoints. The prediction model selected for a given patient in the application depends on which measurements are available, ensuring maximal use of existing data.

Details of each model, including probability cutoffs, sensitivity, specificity, and accuracy for both 1- and 2-year predictions, are presented in Table X (Results). The models were deployed using RShiny for ease of access, taking in patient information and providing predictions for 1- and 2-year resolution along with the corresponding sensitivity and specificity values.

# Results

Model performance metrics for the five predictor sets are shown in Table X. Probability cutoffs were chosen to achieve at least 50\% sensitivity for the 1-year prediction, with the same cutoff applied to the corresponding 2-year model. This approach generally yielded higher specificity for 1-year predictions and higher sensitivity for 2-year predictions.

| Model                  | Cutoff   | 1-year Sensitivity | 1-year Specificity | 1-year Accuracy | 2-year Sensitivity | 2-year Specificity | 2-year Accuracy |
|------------------------|----------|--------------------|--------------------|-----------------|--------------------|--------------------|-----------------|
| Reflux Grade           | 0.25962  | 50.00\%            | 82.49\%            | 77.00\%         | 75.67\%            | 57.20\%            | 62.14\%         |
| Volume                 | 0.21548  | 50.38\%            | 77.52\%            | 72.94\%         | 75.54\%            | 52.30\%            | 61.21\%         |
| Reflux Grade + Volume  | 0.27574  | 50.43\%            | 87.65\%            | 81.56\%         | 67.63\%            | 70.62\%            | 69.85\%         |
| UDR                    | 0.20052  | 50.63\%            | 75.66\%            | 71.69\%         | 80.68\%            | 48.08\%            | 56.97\%         |
| Reflux Grade + UDR     | 0.22986  | 50.00\%            | 81.48\%            | 76.62\%         | 78.41\%            | 56.17\%            | 62.23\%         |

Table X: Performance metrics for logistic GEE models predicting 1- and 2-year resolution. Probability cutoffs were chosen to achieve at least 50\% sensitivity for the 1-year model and the same cutoffs were applied to the corresponding 2-year model.

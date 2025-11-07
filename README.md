# Functional-Decline-ENRICA-BKMR





In this repository, you will find the code and data required to replicate the results from the paper:



> \*"Trace element mixtures and functional decline markers related to sarcopenia among older adults: a Bayesian kernel machine regression study in the ENRICA cohort"\*  

> Álvaro Santos Cuerva, Esther García García-Esquinas, 2025



The BKMR analyses were conducted in \*\*RStudio\*\*, while descriptive analyses were run in \*\*Stata\*\*.



---



\## Repository Structure



\- `code/Stata`: Stata scripts (`.do`) for exploratory analyses of baseline characteristics and multivariate logistic regression models.

\- `code/R`: R script (`.R`) for BKMR. This script is flexible — edit parameters as needed for each outcome, mixture, and model.

\- `fit\_models`: Stored BKMR model objects.

\- `knots`: Knot matrices used for BKMR models.

\- `plots`: PDFs containing plots and other relevant outputs.

\- `saved\_model`: Models generated for plotting purposes.

\- `tables`: Tables with sociodemographic analyses by outcome and multivariate logistic regressions by metal for each outcome.

\- `README.md`: This file, containing general information about the repository.



---



\## Instructions to Replicate the Analyses



> \*\*Note:\*\* These steps assume that Git is already installed and configured.





1. \*\*Clone the repository:\*\*



```bash

git clone https://github.com/AlvaroSantos-Epi/Functional-Decline-ENRICA-BKMR.git



2\. Open the folder where the repository has been cloned (if unsure whereit was cloned, run the command \*pwd\* on Git Bash)



3\. Run the scripts:

* Sociodemographic results (no comorbilities): Run code/Stata/table1\_generator\_model1.do
* Sociodemographic results (comorbilities): Run code/Stata/table1\_generator.do
* Multivariate logistic regression for each metal: Run code/Stata/table2\_generator.do
* BKMR: Run code/R/BKMR\_flex.R and edit the parameters for outcome, matrix and mixture group for each analysis. Number of knots and iterations are also editable.







**Contact**

For questions and collaboration:



Álvaro Santos Cuerva

Department of Preventive Medicine and Public Health and Microbiology, Universidad Autónoma de Madrid

📧 alvaro.santos@uam.es

🔗 \[GitHub: @AlvaroSantos-Epi](https://github.com/AlvaroSantos-Epi)



📚 If you use this repository or part of its code, please cite:

Santos A, García-Esquinas E. Trace element mixtures and functional decline markers related to sarcopenia among older adults: a Bayesian kernel machine regression study in the ENRICA cohort. 2025.




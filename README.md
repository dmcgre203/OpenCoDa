# OpenCoDa
# Code for running regression tool on local Shiny Server

Brief Guidance on using Linear Regression Tool

DATA UPLOAD

Data must be uploaded in .csv format. Any "data cleaning" should be done outside of the tool. 
In particular, the tool may not recognize all "n/a" indicators. We recommend such fields be left as blanks, or the records omitted entirely.
Once the data is uploaded it will display on the right hand side of the screen. Depending on the size of your data it may be necessary to scroll right to see all of the fields.
If you just want to try out the tool without using your own data there is a simple dummy dataset below.

IDENTIFYING DATA
The next stage is to identify the response variable / outcome in your data, the standard covariates / confounding variables you would like to include, and last (but not least) the compositional variables in your data.
You will need to indicate continuous and categorical variables separately.

MODEL SELECTION - STAGE ONE
The tool includes some limited functionality for selecting covariates for inclusion (AIC, and likelihood ratio tests on the effect of dropping individual variables) displayed on the second tab.
Changing the selected covariates will cause these calculations to update automatically.

ILR COORDINATES
Once you have identified the compositional variables, and given the tool a little time to compute, it will provide a list of the available transformed CoDa variables for carrying out the regression.
These are calculated using the "robcompositions" package in R.
We have used a similar naming convention to robcompositions:
"_" indicates division, "." indicates geometric averaging, so for example "SB_MV.LI" would indicate the ilr coordinate is based on the logratio between "SB", and the geometric average of "MV" and "LI." The original variable names are often abbreviated in the ilr coordinate names.
Currently the tool has been built to handle 3 or 4 components. In addition, the list of ilr coordinates for 4 components is restricted to those built on the pivot coordinate approach. We hope to expand the functionality to higher dimensions, and a more general set of balances, in the near future.
The ilr coordinates for individual members are shown on the "ilr coordinates" tab.
The results of the full regression analysis, including covariates are shown on the final tab.
The analysis is fuller than the stage one model selection, but for many cases more testing may be advisable.

MODEL SELECTION - STAGE TWO
It is straightforward to switch between different rotations, or indeed omit components entirely (which can be a valuable method for better understanding what is driving a response). 
The calculations will update automatically each time the ilr coordinates are changed.

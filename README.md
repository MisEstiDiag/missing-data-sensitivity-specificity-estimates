This project belongs to the manuscript entitled "Comparison of methods to handle missing values in a binary index test in a diagnostic accuracy study -- a simulation study" by D Juljugin (1), K Stahlmann (1), A Zapf (1). 1 Institute of Medical Biometry and Epidemiology, University Medical Center Hamburg Eppendorf, Hamburg, Germany

It contains the code to replicate the simulation study and its results. The simulated data are provided as well. 

Author of the code: Dennis Juljugin

Corresponding author: Katharina Stahlmann (k.stahlmann@uke.de)

How to replicate the simulation study
 
Folder: "Simulation Code"

"simulation code.R" and "simulation parallel code.R" are the main R files that are needed to run the simulation. Both of them will produce the same results, "simulation parallel code.R" just runs faster as it is coded in a way that makes use of parallel processing. The main simulation code sources information from "grid.R" (data frame of all simulation scenarios), "catdata.R" (ACD package) and "functions 3.R" (data generation function and worst case scenario function). An additional short simulation was conducted with only MICE and m = 50 imputations through "mice 50.R" which sources "grid mice50.R" instead of "grid.R". 


Folder: "Analysis Code"

"results rsimsum.R" is the main file to calculate the results (and generate nested loop plots) using the rsimsum package with the data sets generated from the simulation. 
"coverage preparation.R" is additionally needed to calculate the coverages. "logit coverage sensitivity.R", "logit coverage specificity.R", "wald coverage sensitivity.R" and "wald coverage specificity.R" can be used to generate the nested loop plots for the respective coverages.
"power calculation cca.R", "power calculation mice.R", "power plots.R", "power mcse.R" can recreate the power calculations and plots in this study.
"results rsimsum mice 50.R" and "mice 50 coverage" calculate the correponding results and generate the plots for the short additional simulation with only MICE and m = 50. "results only mice.R" generates nested loop plots only for MICE with m = 5 which can be seen in the supplements of this manuscript.





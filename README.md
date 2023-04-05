# PostcodeGeodemographic

## Patrick Ballantyne, Alex Singleton

*A postcode-level classification utilising the latest release of the decennial census, behavioural and opinion-based data from the PDV and the outputs of a segmentation which utilised satellite imagery to classify postcodes in Great Britain.*


## Project Overview

This project is divided into a number of different sections, each with their own set of outputs:

1. Pulling in variables from the 2021 census and identifying which would be most useful to build a 'general purpose' geodemographic classification. Code available [here]("Code/1. Cleaning and Processing Census Data.R").

2. Building a weighting mechanism using UPRNs to provide a lookup between census data at output area level and postcodes across England and Wales. Code available [here]("Code/2. Assembling Postcode-level Weights.R").

3. Modelling output area census data down to postcodes. Code available [here]("Code/3. Modelling Census Data to Postcodes").

4. Using graph structures to identify correlations and prune from a large pool of potential input variables. Code available [here]("Code/4. Correlation Testing and Variable Pruning").

5. TBC.

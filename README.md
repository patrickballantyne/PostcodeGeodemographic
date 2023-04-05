# PostcodeGeodemographic

## Patrick Ballantyne, Alex Singleton

*A postcode-level classification utilising the latest release of the decennial census, behavioural and opinion-based data from the PDV and the outputs of a segmentation which utilised satellite imagery to classify postcodes in Great Britain.*


## Project Overview

This project is divided into a number of different sections, each with their own set of outputs:

1. Pulling in variables from the 2021 census and identifying which would be most useful to build a 'general purpose' geodemographic classification. Code available [here](https://github.com/patrickballantyne/PostcodeGeodemographic/blob/main/Code/1.%20Cleaning%20and%20Processing%20Census%20Data.R).

2. Building a weighting mechanism using UPRNs to provide a lookup between census data at output area level and postcodes across England and Wales. Code available [here](https://github.com/patrickballantyne/PostcodeGeodemographic/blob/main/Code/2.%20Assembling%20Postcode-level%20Weights.R).

3. Modelling output area census data down to postcodes. Code available [here](https://github.com/patrickballantyne/PostcodeGeodemographic/blob/main/Code/3.%20Modelling%20Census%20Data%20to%20Postcodes.R).

4. Using graph structures to identify correlations and prune from a large pool of potential input variables. Code available [here](https://github.com/patrickballantyne/PostcodeGeodemographic/blob/main/Code/4.%20Correlation%20Testing%20and%20Variable%20Pruning.R).

5. TBC.

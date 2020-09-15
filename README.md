# Surrey Crosswalk Project
This is a Shiny application designed for city engineers in the City of Surrey to predict optimal crosswalk locations to enhance road safety and create equity. This is achieved by visualizing spatial crosswalk data and calculating an index to prioritize locations to build a crosswalk. 

## Background
Currently, the city collects data and complete the analysis for locations provided through resident requests. However, there are 3 times more requests from high income areas compared to low income areas. At the same time, there are 4 times more pedestrian collisions in low income areas than in high income areas. This project and application generate locations where there is a high probability of crosswalk being required using census, land use, existing crosswalk, road and transportation infrastructure, and other datasets. Machine learning is applied for incomplete datasets such as the pedestrian and traffic volume; shown as the Annual Average Daily Traffic (AADT) number.

R Shiny was chosen to develop the visualization platform because of its abundant visualization tool for the map, flexibility in the interface to change the data used globally, and functionality to change and save the scoring matrix for the index.

## Features
1. Visualization: The user is able to visualize the spatial data of the intersections and its surroundings. These interactions are quantified through the Potential Pedestrian Index and Pedestrian Deficiency Index by providing a score for the user to rank the locations. The indexes are plotted in a donut chart. The predicted and (if provided) actual AADT values are plotted for each intersection.
![](surrey_map.gif)

2. Score Matrix: The user is able to explore the data structure of each variable in the respective index and adjust the scoring matrix, which is provided in a stepwise function. Once the user saves the new scoring matrix, a csv file will be downloaded and the score is globally reflected in the app.
![](surrey_scoring.gif)

3. Ranking locations: According to the scores, the locations are ranked by percentile.

## Usage
The data used for this project was City-owned data, including open data available on their website (https://data.surrey.ca), Insurance Corporation of British Columbia (ICBC) data, and Translink data on stop usage and locations. Since some of the preprocessed data are confidential, the raw data are not available in this repository. Visit [the app](https://alinac20.shinyapps.io/SurreyCrosswalk/) on my shinyapps.io page.

## Support
More detailed information and a description of our models can be found in the [Crosswalk Project report and recorded presentation](https://dsi.ubc.ca/data-science-social-good).

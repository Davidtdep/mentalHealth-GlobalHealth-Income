# Mental Health Research and Global Health Indicators
This repository contains the R scripts used for the analysis of **mental health research**, categorized by **income groups**, and their associations with **global health indicators** from global databases, including WHO, World Bank, Our World in Data, and IHME.

## Purpose
This repository provides a reproducible workflow for analyzing global mental health research trends and their association with various global health indicators by income groups. The analyses aim to:
- Perform comprehensive **linear regression models** to assess publication volumes against global health indicators.
- Conduct **meta-analyses** to estimate pooled associations across income groups.
- Perform **meta-regression** analyses using each of the 60 income-level indicators, along with selected bibliometric indicators, as individual moderators for the meta-analysis results


## Repository contents
This repository includes the scripts:
- `pre_analysis.R`: This script organizes and defines the indicator groups. It includes data preprocessing tasks, such as reading external datasets and merging indicators from various sources.
  
- `main_analysis.R`: Asign the indicators' roles (dependent or independent), executes the primary regression models by income group, runs meta-analyses on the regression results, and meta-regression analyses.

- `plots.R`: Contains functions and procedures to visualize the results using `ggplot2`.

## Data Sources and Preprocessing
A total of **386,671 mental health research articles** were included in the analysis. Metadata included **publication year**, **country of the first author**, **journal H-index**, **citations**, **open access status**, and **journal quartile**. Each article was assigned to one of the **income groups** based on the first author's country, as classified by the World Bank.

We gathered national indicators from various sources:
- WHO Global Health Observatory
- The World Bank
- Our World in Data
- Institute for Health Metrics and Evaluation (IHME)

### Indicators Organized Into Thematic Categories
1. **Economy, Development, and Education**  
2. **Global Health**  
3. **Inequality and Poverty**  
4. **Governance and Rights**

All indicators were summarized by income group and year, enabling cross-group comparisons. Complete lists are available in supplementary tables.

## Analyses Included
1. **Data preparation and indicator grouping**: Created data frames for different thematic categories, defining roles for independent and dependent variables.
  
2. **Linear regression models**: Fitted models to explore the relationships between each indicator and the total number of publications. Separate models were computed for each income group.
  
3. **Meta-analysis**: Conducted to synthesize results from the regression analysis across income groups, utilizing the random-effects model.
  
4. **Heatmap visualizations**: Generated to illustrate standardized coefficients of the regression results, annotated with significance levels.

5. **Meta-regression analyses**: Explored moderators that may influence the relationships between bibliometric metrics and contextual socio-economic indicators.

## Data Availability
This project uses openly available data from public databases. The mental health bibliometric dataset used is **available upon reasonable request**.
  
## License
This repository is licensed under the **MIT License**, allowing free use, modification, and distribution with attribution. See `LICENSE` file for more details.

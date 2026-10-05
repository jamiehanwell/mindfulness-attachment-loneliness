# Attachment Dimensions Mediate an Inverse Relationship Between Trait Mindfulness and Loneliness in Young Adults

## Overview

This project is a reproducible reanalysis of my undergraduate psychology dissertation.

The analysis was originally conducted in SPSS and has been recreated in R as a practical exercise in reproducible quantitative research.

## Research questions

1. Is trait mindfulness an independent predictor of loneliness after accounting for established predictors?
2. Can attachment anxiety and avoidance statistically account for part of the association between trait mindfulness and loneliness?

## Measures
- Revised UCLA Loneliness Scale
- Experiences in Close Relationships Scale Short Version
- Social Interaction Anxiety Scale Straightforward
- Patient Health Questionnaire 2
- Five Facet Mindfulness Questionnaire

## Analysis

- Hierarchical multiple regression
- Parallel mediation analysis with bootstrap confidence intervals

This project was developed in RStudio using packages 'tidyverse', 'haven', 'psych', 'apaTables', 'lm.beta', 'lmtest', 'performance', 'car', 'lavaan', 'DiagrammeR', and 'DiagrammeRsvg'.

## Data

The original participant-level dataset (N = 182) is not included in this repository because it is not publicly shareable.

## Results

- Trait mindfulness was negatively associated with loneliness. In the hierarchical multiple regression, trait mindfulness was not an independent predictor of loneliness after accounting for depression and social anxiety (step 1) and attachment anxiety and attachment avoidance (step 2).
- In the parallel mediation analysis, both attachment anxiety and attachment avoidance showed significant indirect effects between trait mindfulness and loneliness. The direct association between mindfulness and loneliness was not statistically significant after accounting for the attachment dimensions.
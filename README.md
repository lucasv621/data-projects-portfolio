# Lucas Veteikis Data Projects Portfolio

This repository contains different data-related projects I've worked on, including assignments from university (Master in Management + Analytics, UTDT) and personal ones.

## Brief Project Summaries

### Conjoint Analysis Applied to Apartment Rental
The goal of this project was to apply Conjoint Analysis, a market research technique that assesses preferences by analyzing how people make choices among products or services with varying features, to apartment rental. The analysis utilized the Python package Statsmodels for conducting linear regressions across the responses of all survey participants. Key findings included:
- Determining the **importance of each attribute** for each respondent
- Estimating the **willingness to pay** for upgrades in specific attributes
- **Segmentation of respondents** using unsupervised learning techniques
- **Simulation of market shares** based on preferences

### Contact Prediction Challenge
This project was part of a Kaggle Competition where our team ranked in the top 30%. The competition aimed to predict whether a post on an accommodation marketplace would receive more than 3 contacts within the initial 15 days. We dealt with a dataset containing millions of observations and numerous features. The prediction process involved:
- **EDA**: Analyzing the data to extract valuable insights and eliminate irrelevant information
- **Feature engineering**:Crafting new features to improve our predictive abilities
- Utilizing **sparse matrices**
- **Implementing various machine learning models** like Naive Bayes, Decision Trees, and ultimately XgBoost to achieve optimal performance in the competition.

### Difference in Differences Analysis in Sports
This university assignment involved conducting a quasi-experiment using the "Difference in Differences" method, a quasi-experimental approach comparing outcome changes over time between a treatment group and a control group. Specifically, this technique was utilized to evaluate the performance of Pep Guardiola, a Spanish football coach, in the Premier League. The assignment included:
- Conducting research to **identify the suitable application of the technique** and gathering data through web scraping
- **Selecting a control group**, a task requiring the demonstration of parallel trends with the treatment group
- **Executing linear regressions** using the difference-in-differences technique and interpreting the outcomes
- **Producing a concise report** summarizing the quasi-experiment, limited to a maximum of three pages.

### Linear Programming in Resource Allocation

Linear programming involves maximizing or minimizing a linear function while considering various constraints. This method is widely applicable in modeling real-life scenarios to find the most effective resource allocation strategies. In this specific project, the aim was to allocate students to medical placements, considering preferences from both students and institutions, with the goal of maximizing overall satisfaction.
The project tasks included:
- **Problem Modeling:** We defined the problem by proposing an objective function to maximize and setting up different constraints.
- **Assessing Model Efficiency:** We evaluated the effectiveness of the linear model by using key performance indicators (KPIs) we developed.
- **Optimizing with CPLEX API:** Utilizing the CPLEX API in Python, we optimized the proposed model to achieve the desired allocation.


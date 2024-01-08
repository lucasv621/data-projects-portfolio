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
- **Feature engineering**: Crafting new features to improve our predictive abilities
- Utilizing **sparse matrices**
- **Implementing various machine learning models** like Naive Bayes, Decision Trees, and ultimately XgBoost to achieve optimal performance in the competition.

### Difference in Differences Analysis in Sports
This university assignment involved conducting a quasi-experiment using the "Difference in Differences" method, a quasi-experimental approach comparing outcome changes over time between a treatment group and a control group. Specifically, this technique was utilized to evaluate the performance of Pep Guardiola, a Spanish football coach, in the Premier League. The assignment included:
- Conducting research to **identify the suitable application of the technique** and gathering data through web scraping
- **Selecting a control group**, a task requiring the demonstration of parallel trends with the treatment group
- **Executing linear regressions** using the difference-in-differences technique and interpreting the outcomes
- **Producing a concise report** summarizing the quasi-experiment, limited to a maximum of three pages.

### Linear Programming Approach for Resident Allocation in Medical Institutions
Linear programming involves maximizing or minimizing a linear function while considering various constraints. This method is widely applicable in modeling real-life scenarios to find the most effective resource allocation strategies. In this specific project, the aim was to allocate students to medical placements, considering preferences from both students and institutions, with the goal of maximizing overall satisfaction.
The project tasks included:
- **Problem Modeling:** We defined the problem by proposing an objective function to maximize and setting up different constraints.
- **Assessing Model Efficiency:** We evaluated the effectiveness of the linear model by using key performance indicators (KPIs) we developed.
- **Optimizing with CPLEX API:** Utilizing the CPLEX API in Python, we optimized the proposed model to achieve the desired allocation.

### Machine Learning Applied To Sales Prediction
Sales prediction is a widely adopted machine learning task across industries. In this project, participants engaged in a Kaggle competition aimed at developing machine learning models capable of forecasting daily sales for various stores of a renowned supermarket chain in Germany. The project process encompassed:
- **Data Processing:** The data arrived in a messy format, requiring substantial preprocessing efforts.
- **Exploratory Data Analysis (EDA):** A comprehensive EDA was conducted, focusing on ideas for feature engineering, handling missing values, and other necessary tasks.
- **Feature Engineering:** Crafting and selecting the most impactful features was crutial for optimizing model performance.
- **Machine Learning Model Implementation:** Implementing and fine-tuning XGBoosts and Random Forests using Python's Scikit-Learn package for predictive modeling.

### NYC Taxi Fare Prediction
This project focused on a machine learning competition which aim was to predict the price of different taxi rides in New York, challenging participants to create models predicting taxi fares accurately. The process involved:

- **EDA:** The primary focus was on discarding invalid values and identifying patterns crucial for generating new variables.
- **Managing Large Datasets:** Dealing with an original dataset comprising over 55 million observations was a significant challenge, requiring adept handling of vast amounts of information.
- **Feature Engineering:** With a limited number of features initially available, the key task involved crafting and optimizing features to enhance the models' predictive capabilities.
- **Machine Learning Model Implementation:** Leveraging XGBoost within the Scikit-Learn package, the project involved fine-tuning its hyperparameters and employing ensemble methods using various XGBoost algorithms to attain the highest achievable score.

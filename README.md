# Data-Science-Portfolio

The projects contained within this GitHub repository showcase my skills in data analysis, wrangling, and manipulation, as well as my statistical analysis and modelling skills. The projects are coded primarily in R and Python. If you have any questions regarding the projects contained within this repository, please do not hesitate to contact me at akashpendyala@gmail.com.

#### [Python Projects](https://github.com/akash424/Data-Science-Portfolio/tree/master/Python%20Projects)
- **Predicting Cannabis Use**:
For my Biomedical Informatics Master's capstone project, I undertook the task of trying to predict marijuana use in adolescents given information about the child themself as well as their parents. This information included genetic factors that indicated both the child and parent's predisposition towards marijuana use, also known as polygenic risk scores. The information also included non-genetic data, such as the child's behavioral tendencies, the relationship between the child and parent, and the parent's attitude towards marijuana use. Given all of this information (154 explanatory variables), I developed multinomial logistic and random forest models to predict marijuana use in adolescents. I employed k-nearest neighbors to impute missing data and an iterative variable selection approach to reduce the number of variables selected by each model until max cross-validation and testing accuracy were reached. The random forest model predicting the amount of marijuana use in the last three months in adolescents performed the best with 89.6% cross-validation accuracy and 90.5% testing accuracy.

- **Data Quality Analysis**:
I took the initiative to create a generic template for performing data quality analysis on text data upon request from my Senior Manager at Deloitte. This generic template is a custom-built function that takes several user inputs to filter or transform a dataset based on those inputs. For example, a user may want to find all rows matching a particular regex expression across multiple columns. The function will return all the rows where the columns specified by the user match the regex expression.

- **(NBA) Scoring_vs_Possessions**:
A simple NBA analytics analysis where I attempted to rank the best scorers in the NBA by finding the number of points they scored above the mean.

- **ATL Hawks Data Analyst - Programming Assessment**:
Completed a programming assessment for the ATL Hawks where I calculated the highest assister-to-scorer combos in the NBA and analyzed the ATL Hawks team's four factors. 

#### [R Projects](https://github.com/akash424/Data-Science-Portfolio/tree/master/R%20Projects)
- **KangaTech Measurements**:
While working for the Angels Baseball team, I created an R script that automated the process of calculating differences in players' KangaTech (neuromuscular strength) measurements over time. These calculations would have taken several hours to complete in Excel, whereas with the R script the calculations could be completed and written to an Excel file within a matter of minutes with just the click of a button ('Run').

- **Cincinnati Reds Hackathon 2024**:
The Cincinnati Reds held their first ever analytics hackathon and the prompt asked participants to analyze current pitcher roles in baseball (i.e. starters and relievers), and potentially propose new roles. My analysis focused on differentiating between starters and relievers and the findings showed that there is not much of a difference between the two role types as far as performance -- starters and relievers have similar strikeouts per inning pitched. I proposed three reasons why this could be the case: (1) starting pitchers assume a higher workload than relief pitchers; (2) starting pitchers play more tight-game situations than relief pitchers; and (3) starting pitchers pitch against higher caliber batters more often than relief pitchers. I tested these hypotheses using the data provided and created visualizations using ggplot2 to support my findings.

- **Predicting NBA Game Outcomes**:
I completed a project for my Master's program where I investigated which individual basketball statistics are the most important to winning. Due to multicollinearity, I conducted principle components analysis (PCA) to create linear combinations of the variables and then performed logistic regression using the new feature set and player position as the independent variables. The dependent variable was whether the player's team won the game. Results showed that a player's individual plus-minus is the most important statistic in predicting the outcome of a game, which is to be expected as plus-minus measures the scoring margin while a player is on the floor. Next time, the analysis could be repeated without including plus-minus in the group of basketball statistics used to predict the game outcome. 

#### [R Projects/R Shiny Visualizations](https://github.com/akash424/Data-Science-Portfolio/tree/master/R%20Projects/R%20Shiny%20Visualizations)
- **Lineup Assessment Tool**:
RShiny dashboard intended to be a useful tool for basketball coaches and front office staff to evaluate their team's lineups. The tool provides information on both the team's and opponent's statistics when the lineup in question is either on or off the court. Of course, the purpose of basketball is to outscore the other team; however, this tool allows the user to analyze the reasons behind a team's success or lack thereof. Is it defensive rebounding, three-point shooting, or some other statistic that determines how well the team fares with a particular lineup?

- **NHTS**:
RShiny dashboard that summarizes findings froms the National Household Travel Survey datasets (trip, vehicle, and person), each containing 1,000,000+ rows. Specifically, the app shows how proportion of households, persons, and trips, and household and person trip rates change across demographics and geographic region. A certain major metropolitan area may have a higher proportion of households making above 75k than another major metropolitan area. Staying on the topic of household income, there may be a greater proportion of households making less than 75k than more than 75k in a certain metropolitan area. These are examples of insights provided by the dasbhoard. The trip rates choropleth maps are not functional in the online version of the app. These can be viewed if the app is downloaded from the GitHub repository and run locally.

- **Patient Volume Visualization**:
A simple RShiny dashboard that visualizes number of patients seen by hour and by shift. There is a clear pattern where more patients are seen during the second shift, or middle of the day, across all imaging modalities (CT, MR, US). The purpose of this app was to assist the hospital's radiology department in making decisions related to staff management and allocation. 

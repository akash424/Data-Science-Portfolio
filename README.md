# Data-Science-Portfolio

The projects contained within this GitHub repository showcase my skills in data analysis, wrangling, and manipulation, as well as my statistical analysis and modelling skills. The projects are coded primarily in R and Python. If you have any questions regarding the projects contained within this repository, please do not hesitate to contact me at akashpendyala@gmail.com.

#### [R Projects](https://github.com/akash424/Data-Science-Portfolio/tree/master/R%20Projects)
- **KangaTech Measurements**:
While working for the Angels Baseball team, I created an R script that automated the process of calculating differences in players' KangaTech (neuromuscular strength) measurements over time. These calculations would have taken several hours to complete in Excel, whereas with the R script they could be completed and saved to an Excel file within a matter of minutes with just the click of a button ('Run').

- **Cincinnati Reds Hackathon 2024**:
The Cincinnati Reds held their first ever analytics hackathon and the prompt asked participants to analyze current pitcher roles in baseball (i.e. starters and relievers), and potentially propose new roles. My analysis focused on differentiating between starters and relievers and my findings showed that there is not much of a difference between the two role types as far as performance -- starters and relievers have similar strikeouts per inning pitched. I proposed three reasons why this could be the case: (1) starting pitchers assume a higher workload than relief pitchers; (2) starting pitchers play more tight-game situations than relief pitchers; and (3) starting pitchers pitch against higher caliber batters more often than relief pitchers. I tested these hypotheses using the data provided and created visualization using ggplot2 to support my findings.

- **Predicting NBA Game Outcomes**:
I completed a project for my Master's program where I investigated which individual basketball statistics are the most important to winning. Due to multicollinearity, I conducted principle components analysis (PCA) to create linear combinations of the variables and then performed logistic regression using the new feature set and player position as the independent variables. The dependent variable was whether the player's team won the game. Results showed that a player's individual plus-minus is the most important statistic in predicting the outcome of a game, which is to be expected as plus-minus measures the scoring margin while a player is on the floor. Next time, the analysis could be repeated without including plus-minus in the group of basketball statistics used to predict the game outcome. 

#### [R Projects/R Shiny Visualizations](https://github.com/akash424/Data-Science-Portfolio/tree/master/R%20Projects/R%20Shiny%20Visualizations)
- **Lineup Assessment Tool**:
RShiny dashboard intended to be a useful tool for basketball coaches and front office staff to evaluate their team's lineups. The tool provides information on both the team's and opponent's statistics when the lineup in question is either on or off the court. Of course, the purpose of basketball is to outscore the other team; however, this tool allows the user to analyze the reasons behind a team's success or lack thereof. Is it defensive rebounding, three-point shooting, or some other statistic that determines how well the team fares with a particular lineup?

- **NHTS**:
RShiny dashboard that summarizes findings froms the National Household Travel Survey datasets (trip, vehicle, and person), each containing 1,000,000+ rows. Specifically, the app shows how proportion of households, persons, and trips, and household and person trip rates change across demographics and geographic region. A certain major metropolitan area may have a higher proportion of households making above 75k than another major metropolitan area. Staying on the topic of household income, there may be a greater proportion of households making less than 75k than more than 75k in a certain metropolitan area. These are examples of insights provided by the dasbhoard. The trip rates choropleth maps are not functional in the online version of the app. These can be viewed if the app is downloaded from the GitHub repository and run locally.

- **Patient Volume Visualization**:
A simple RShiny dashboard that visualizes number of patients seen by hour and by shift. There is a clear pattern where more patients are seen during the second shift, or middle of the day, across all imaging modalities (CT, MR, US). The purpose of this app was to assist the hospital's radiology department in making decisions related to staff management and allocation. 

### Python Projects/Predicting Cannabis Use
Conducted a research project for graduate level course to predict adolescents' risk of cannabis use based on their parents' genetics, their own genetics, and other family-related information.
Cleaned and pre-processed the data, and trained multinomial regression and random forest models.
Developed a strong familiarity with common Python libraries (scikit-learn, numpy, pandas) by working on this project. 

### R Projects/KangaTech Measurements
Created an R script that generated automated reports in Excel for the performance and training staff, summarizing KangaTech measurements (neuromuscular conditioning) and identifying any red flags / problem areas for all players in the LA Angels organization.  

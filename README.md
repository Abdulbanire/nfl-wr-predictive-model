# NFL Wide Receiver Predictive Model

> Predicting next-season receiving yards for NFL wide receivers using historical play-by-play data.

## Project Overview
This project uses NFL play-by-play data to model and predict next-season receiving yards for wide receivers. The analysis focuses on identifying which performance and team-level variables best explain future receiving production.

## Data Source
Play-by-play data was loaded using the nflfastR package for seasons 2019–2024.

## Methods
The analysis includes:
- Aggregation of play-by-play data into season-level WR statistics
- Feature engineering (targets, yards, efficiency metrics)
- Exploratory data analysis and visualizations
- Correlation analysis
- Linear regression modeling
- Train/test split using season-based validation
- Prediction of 2025 receiving yards using 2024 statistics

## Model Features
Key predictors used in the model:
- Targets
- Receiving yards
- Receiving touchdowns
- Yards per target
- Team pass attempts

## Results
Correlation analysis showed strong relationships between current-season usage (targets, yards) and next-season receiving yards.  
The final regression model demonstrated reasonable predictive performance when tested on unseen season data and was used to generate rankings for projected 2025 receiving yards.

## Tools and Libraries
- R
- nflfastR
- tidyverse
- ggplot2
- janitor

## Notes
This project was created for portfolio and learning purposes. Some file paths are local and may need adjustment to run on another machine.

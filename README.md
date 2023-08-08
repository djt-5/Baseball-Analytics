# Welcome
This repository includes projects I have worked on for both the Illini Baseball team, MLB statitical work, and stats related material including

**UIUC Baseball Shiny Apps**
* Is used by the UIUC baseball team to write scouting reports
* Includes both a Pitcher & Batter version
* Works with Trackman data to visualize patterns in the strike zone, on the field, & changes in those patterns as the game goes on, with live adjusting for pitch type, break, velocity, spin, etc.
* (Pitcher App) Measures pitch tunneling by displaying release points, angles, similarity in early movement, & differences in late movement of the pitch based on pitch type
* Visualizes for pitchers the best pitch to throw for a certain outcome (if you want a whiff, a chase, a groundball...) based on the count & the previous pitch thrown. For hitters, this is displayed as a pitch guessing tool.
* Employs both regular & generalized linear regression models to help the user know which variables are relevant in determining certain patterns

**UIUC Groundball Percentage**
* Uses Trackman data to find the best combinations of certain variables for the pitcher to roll a ground ball

**Quality At Bat %**
* A report which defines a "Quality At Bat" & uses Statcast data to figure out if having a QAB is one of the more relevant statistics in baseball

**Stock Market API Shiny App**
* A college project Shiny App which uses an API to look at Fortune 500 companies & graph their trends (minimums, maximums, open, close, & trading volume) over the last 100 days

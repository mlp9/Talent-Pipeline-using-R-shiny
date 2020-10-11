# Talent-Pipeline-using-R-shiny
A talent pipeline is defined as a ready pool of potential candidates who are qualified and prepared to step up and fill relevant key roles within the organization as soon as they fall vacant.  This on-hold talent pool can include internal employees who show promise and can be promoted from within the organization as well as candidates from external sources like referrals, online job portals, career web-pages.  A pipeline of both active and passive candidates helps in perceptive and proactive workforce planning. With a ready pool of right talent, the cost and time to hire can be reduced considerably. Organizations today, in spite of operating in a largely candidate-led market, do not have the luxury to wait for candidates to take the lead and apply. They need to have prospective candidates prepared – the machinery all wound up and ready to be set into motion - before the need arises for them to fill in a role. 

## Application Link: https://mlp9csk.shinyapps.io/TALENT_PIPELINE_FINAL/

## Problem:
Creating a Talent Pipeline for Ashish Chandra who is in Level L17 and finding best Candidates from the talent pool:

## Steps: 
o	From the dataset we are fetching the competency of a particular employee or some benchmark competency values.
o	Using Euclidean distance method, we are calculating the distance between the selected employee’s competency and rest of the employee’s competency. 
o	Based on the distance we find the most matching candidates. 
o	Visualization of the matching candidate using charts such as radar chart, lollipop chart and datatable.

## Using the application:

1) Upload the dataset, "talent_pipeline data". 
2) Select from the dropdown list what kind of comparison you want to make that is with employee or benchmark competency values for the position.
    •	Comparing with current candidates: 
      o	User inputs the employee id and selects the desired competency.
      o	Dynamically select the desired department of candidates to match.
      o	Dynamically select the experience level of candidates to match.
      o	Dynamically select the no of employees to display.
      o	Type of visualization.
  
    •	Comparing with benchmark values:
      o	User selects the desired competency and enters its value.
      o	Dynamically select the desired department of candidates to match.
      o	Dynamically select the experience level of candidates to match.
      o	Dynamically select the no of employees to display.
      o	Type of visualization.

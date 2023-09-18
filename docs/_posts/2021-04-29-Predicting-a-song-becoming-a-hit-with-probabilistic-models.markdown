---
layout: post
title: "Predicting a song becoming a hit with probabilistic models - applied econometrics essay"
date: 2021-04-29 12:00:00 -0000
categories: R music econometrics LaTeX
---
Below I have attached a file containing an essay that I wrote as a final project for my undergraduate course in econometrics. For this task I used the [__Spotify Hit Predictor Dataset__](https://www.kaggle.com/datasets/theoverman/the-spotify-hit-predictor-dataset) and estimated logit, probit, and linear probability models (LPM) using R. First I found the best model specification within each type of model, and then compared the error rates. The figures for LPM, logit and probit were 0.266, 0.279 and 0.328 respectively. The result is quite unusual because normally we would not expect the LPM to outperform non-linear probability models due to its numerous flaws. However, LPM tends to perform quite well when the data is homogenous, which I believe was the case here. 

<object data="{{ site.url }}{{ site.baseurl }}/assets/econometrics_essay.pdf" width="1000" height="1000" type="application/pdf"></object>



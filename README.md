# Counties-by-percentage-case-rate-and-age
Counties by Percentage of Fully Vaccinated and 7-Day Case Rates for Ages 0 – 17 Years –   based on Jenks Natural Breaks for Vaccine Coverage

Created the R code that uses cluster analysis. Two types of cluster analysis are used in order to classify levels of vaccine percentage coverage so that you can identify what should be the cut off points for each level among counties for those ages 0-17 years. For example, in your data, if 5% of those ages 0-17 is the highest vaccinated for any county, then, what should be the range (cut off points) for that level, and the next highest level (eg medium level of vaccination). Either quantile clustering method or Jenks clustering method can determine the cut-off points if you tell R the number of bins (number or levels) you need.

-Jenk’s natural breaks
-Quantiles

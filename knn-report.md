# Knn Report

## Knn1

First, I\'ve implemented simple knn with only parameter k.
For sampling I\'ve chosen t-k-fold cross-validation.

Results are following:

In most cases k was 1 or 2 (scanning range from 1 to 20). These values were repeatedly achieved with different t and k (for tk-fold CV).

Average error value was 0.3, which is rather stable, though varying from 0.26 to 0.36.

Error value states for relative amount of points, which class was determined wrong. So, value 0.3 means that on test dataset class is correctly determined for 70% of points.

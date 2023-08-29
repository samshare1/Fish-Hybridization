#Python script to run a Random Forest analysis on the data collected in the download_env_data.R script

import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.metrics import mean_squared_error, r2_score, explained_variance_score
from sklearn.model_selection import GridSearchCV
from sklearn.feature_selection import RFE
import matplotlib.pyplot as plt
from sklearn.tree import plot_tree
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import cross_val_score

from sklearn.linear_model import Lasso
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import learning_curve
import numpy as np

#import file into python script
file = "genetics_out_21July.csv"
genetic_data = pd.read_csv(file)


#do same thing to upload the env data (this script is in R)
env_file = "env_data.csv"
env_data = pd.read_csv(env_file)


#remove leading/trailing spaces from column names
genetic_data.columns = genetic_data.columns.str.strip()


#check for missing values in the genID column
missing_values = genetic_data['genID'].isna().sum()
#print("Number of missing values in the genID column:", missing_values)

#convert all values in the genID column to string type
genetic_data['genID'] = genetic_data['genID'].astype(str)


#filter the genetic_data DataFrame for rows with exact match "fw" in the "genID" column
fw_genetic_data = genetic_data[genetic_data['genID'] == 'fw'].copy()

#check if "fms_ci_UB" is below 1 and "fms_ci_LB" is above zero
condition = (fw_genetic_data['fms_ci_UB'] < 1) & (fw_genetic_data['fms_ci_LB'] > 0)

#filter the DataFrame using the condition
filtered_genetic_data = fw_genetic_data[condition].copy()

#count the occurrences of each unique river entry in "fw_genetic_data"
river_counts = fw_genetic_data['river'].value_counts()

#filter out the river entries with count less than 10
valid_rivers = river_counts[river_counts >= 10].index.tolist()

# Filter the "fw_genetic_data" DataFrame to keep only rows with valid river entries
fw_genetic_data = fw_genetic_data[fw_genetic_data['river'].isin(valid_rivers)].copy()

# Select only the "river" and "fms" columns from the filtered data
filtered_data = fw_genetic_data[['river', 'fms']].copy()

#Print the filtered data if needed
#print(filtered_data)

#this is a check - count the occurrences of each unique river entry in "fw_genetic_data" again
river_counts2 = filtered_data['river'].value_counts()

#this is a check - print the unique river entries and their respective counts
#print("River\tCount")
#for river, count in river_counts2.items():
#    print(f"{river}\t{count}")

#create a new DataFrame with only the "river" and "fms" columns
subset_df = filtered_data.copy()

#perform an inner join on "river" column to add all columns from "env_data" to "subset_df" if they match
merged_df = subset_df.merge(env_data, on='river', how='inner')

#remove the "total_oil_gas_count" and "GNIS_NAME" columns from the DataFrame, remove cumdrainag because of correlation
merged_df.drop(columns=['total_oil_gas_count', 'ELEV', 'closest_city', 'GNIS_NAME'], inplace=True)

#make a copy for analysis
dataframe = merged_df.copy()
dataframe.drop(columns=['site_no','river'], inplace=True)

#get the target variable (fms_quantile_range)
target_variable = 'fms'

#extract the features (X) and target variable (y)
X = dataframe.drop(columns=[target_variable])
y = dataframe[target_variable]

#split the data into training and testing sets (80% training, 20% testing)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

#define the steps for the pipeline (scaling and Random Forest model)
pipeline = Pipeline([
    ('scaler', StandardScaler()),               # Standardize the features
    ('rf', RandomForestRegressor())             # Random Forest model
])

#define the hyperparameters and their ranges for tuning
param_grid = {
    'rf__n_estimators': [50, 100, 150, 800],         # Number of trees in the forest
    'rf__max_depth': [None, 10, 20],            # Maximum depth of the trees
    'rf__min_samples_split': [2, 5, 10],        # Minimum samples required to split a node
    'rf__min_samples_leaf': [1, 2, 4]           # Minimum samples required at each leaf node
}

#create a GridSearchCV object to perform the hyperparameter tuning
grid_search = GridSearchCV(
    estimator=pipeline,                        # Use the defined pipeline
    param_grid=param_grid,                      # Hyperparameters to tune
    scoring='r2',                              # Use R^2 score for evaluation
    cv=10,                                     # 10-fold cross-validation
    n_jobs=-1                                  # Use all available CPU cores
)

#fit the GridSearchCV object to the training data
grid_search.fit(X_train, y_train)

#get the best model from the grid search
best_model = grid_search.best_estimator_

#evaluate the model on both training and testing data
train_pred = best_model.predict(X_train)
test_pred = best_model.predict(X_test)

#calculate the R^2 score and Mean Squared Error for both sets
train_r2 = r2_score(y_train, train_pred)
test_r2 = r2_score(y_test, test_pred)
train_mse = mean_squared_error(y_train, train_pred)
test_mse = mean_squared_error(y_test, test_pred)

#print the R^2 and Mean Squared Error for both training and testing data
print("Training R^2 score:", train_r2)
print("Testing R^2 score:", test_r2)
print("Training Mean Squared Error:", train_mse)
print("Testing Mean Squared Error:", test_mse)



#function to plot the learning curve
def plot_learning_curve(estimator, X, y, title):
    train_sizes, train_scores, test_scores = learning_curve(
        estimator, X, y, cv=10, scoring='r2', train_sizes=np.linspace(0.1, 1.0, 10)
    )

    train_mean = np.mean(train_scores, axis=1)
    train_std = np.std(train_scores, axis=1)
    test_mean = np.mean(test_scores, axis=1)
    test_std = np.std(test_scores, axis=1)

    plt.figure(figsize=(8, 6))
    plt.plot(train_sizes, train_mean, label='Training R^2', color='b')
    plt.fill_between(
        train_sizes,
        train_mean - train_std,
        train_mean + train_std,
        alpha=0.1,
        color='b',
    )
    plt.plot(train_sizes, test_mean, label='Testing R^2', color='g')
    plt.fill_between(
        train_sizes,
        test_mean - test_std,
        test_mean + test_std,
        alpha=0.1,
        color='g',
    )
    plt.xlabel('Number of Training Samples')
    plt.ylabel('R^2 Score')
    plt.title(title)
    plt.legend(loc='best')
    plt.grid()
    plt.show()

#plot the learning curve
plot_learning_curve(best_model, X_train, y_train, 'Learning Curve for Random Forest')



#plot an overfitting curve based on the number of trees (n_estimators)
n_estimators_range = [50, 100, 150, 500, 600, 800, 1000]
train_scores = []
test_scores = []
for n_estimators in n_estimators_range:
    rf_model = RandomForestRegressor(n_estimators=n_estimators, random_state=42)
    rf_model.fit(X_train, y_train)
    train_scores.append(rf_model.score(X_train, y_train))
    test_scores.append(rf_model.score(X_test, y_test))

plt.figure(figsize=(8, 6))
plt.plot(n_estimators_range, train_scores, label="Training R^2")
plt.plot(n_estimators_range, test_scores, label="Testing R^2")
plt.xlabel("Number of Trees (n_estimators)", fontsize=12)  # Increase the font size
plt.ylabel("R^2 Score", fontsize=12)  # Increase the font size
plt.title("Overfitting Plot - Random Forest", fontsize=14)  # Increase the font size
plt.legend()
plt.grid(True)  # Add gridlines to the plot
plt.show()

#plot feature importance
feature_importance = best_model.named_steps['rf'].feature_importances_
sorted_idx = feature_importance.argsort()

plt.figure(figsize=(8, 6))
plt.barh(range(len(sorted_idx)), feature_importance[sorted_idx], align='center')
plt.yticks(range(len(sorted_idx)), X.columns[sorted_idx])
plt.xlabel("Feature Importance", fontsize=12)  # Increase the font size
plt.ylabel("Feature", fontsize=12)  # Increase the font size
plt.title("Feature Importance Plot - Random Forest", fontsize=14)  # Increase the font size
plt.show()


#plot predicted vs. real values for the testing set
plt.figure(figsize=(8, 6))
plt.scatter(y_test, test_pred, alpha=0.7, marker='o', label="Predicted Values")
plt.scatter(y_test, y_test, alpha=0.7, marker='x', label="Real Values")
plt.xlabel("Real Values", fontsize=12)  # Increase the font size
plt.ylabel("Predicted Values", fontsize=12)  # Increase the font size
plt.title("Predicted vs. Real Values - Random Forest", fontsize=14)  # Increase the font size
plt.legend()
plt.grid(True)  # Add gridlines to the plot
plt.show()

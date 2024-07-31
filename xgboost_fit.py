import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.model_selection import train_test_split
from sklearn.metrics import root_mean_squared_error, r2_score
import matplotlib.pyplot as plt

# Load CSV file
data = pd.read_csv('rb_data.csv')

# Preprocessing
categorical_features = []  # Specify any categorical columns here if any
label_encoders = {}

for col in categorical_features:
    label_encoders[col] = LabelEncoder()
    data[col] = label_encoders[col].fit_transform(data[col])

# Split features and target
features = data.drop(columns=['apy_cap_pct'])
target = data['apy_cap_pct']

# Check for NaN, inf, or too large values
valid_indices = np.isfinite(target) & ~np.isnan(target) & (np.abs(target) < np.finfo(np.float32).max)
features = features[valid_indices]
target = target[valid_indices]

# Standardize numerical features
numerical_features = features.select_dtypes(include=['float64', 'int64']).columns
scaler = StandardScaler()
features[numerical_features] = scaler.fit_transform(features[numerical_features])

# Convert all features to float32
features = features.astype('float32')

# Convert to DMatrix
dtrain = xgb.DMatrix(features, label=target)

# Set parameters for XGBoost
params = {
    'objective': 'reg:squarederror',
    'eval_metric': 'rmse',
    'tree_method': 'hist',
    'device': 'cuda', 
    'alpha': 0.1,
    'lambda': 2
}

# Perform 10-fold cross-validation
cv_results = xgb.cv(
    params,
    dtrain,
    num_boost_round=100,
    nfold=10,
    metrics={'rmse'},
    as_pandas=True,
    seed=42
)

# Output the cross-validation results
print(cv_results)
mean_rmse = cv_results['test-rmse-mean'].min()
std_rmse = cv_results['test-rmse-std'].min()
print(f"10-fold CV Mean RMSE: ${255.4 * mean_rmse:.4f}M")
print(f"10-fold CV Std RMSE: ${255.4 * std_rmse:.4f}M")

# Train the model on the full dataset
bst = xgb.train(params, dtrain, num_boost_round=100)

# Load the new data (saquon_data.csv)
saquon_data = pd.read_csv('saquon_data.csv')

# Preprocessing for the new data
# Apply the same label encodings (if any) used in training data
for col in categorical_features:
    if col in saquon_data:
        saquon_data[col] = label_encoders[col].transform(saquon_data[col])

# Ensure the new data has the same feature set (excluding the target variable)
saquon_features = saquon_data.drop(columns=['apy_cap_pct'])  # Assuming 'apy_cap_pct' is the target variable

# Standardize numerical features using the same scaler
saquon_features[numerical_features] = scaler.transform(saquon_features[numerical_features])

# Convert all features to float32
saquon_features = saquon_features.astype('float32')

# Convert to DMatrix
dsaquon = xgb.DMatrix(saquon_features)

# Estimate 'apy_cap_pct' using the trained model
saquon_pred = bst.predict(dsaquon)

# Output the predictions
saquon_data['estimated_apy_cap_pct'] = saquon_pred
saquon_data['estimated_apy_millions'] = np.where(
    saquon_data.index == 0, 
    saquon_pred * 208.2, 
    saquon_pred * 255.4
)

print(saquon_data[['year_signed', 'apy_cap_pct', 'estimated_apy_cap_pct', 'estimated_apy_millions']])


# Assuming 'year_signed' is a feature in the dataset
years = data['year_signed']

# Feature Importance Plot
xgb.plot_importance(bst)
plt.title('Feature Importance')
plt.show()

# Residual Plot
train_pred = bst.predict(dtrain)
residuals = target - train_pred

plt.scatter(train_pred, residuals, c=years, cmap='viridis')
plt.colorbar(label='Year Signed')
plt.axhline(0, color='r', linestyle='--')
plt.xlabel('Predicted Values')
plt.ylabel('Residuals')
plt.title('Residual Plot')
plt.show()

# Predicted vs. Actual Plot
plt.scatter(train_pred, target, c=years, cmap='viridis')
plt.colorbar(label='Year Signed')
plt.xlabel('Predicted Values')
plt.ylabel('Actual Values')
plt.title('Predicted vs. Actual')
plt.plot([min(train_pred), max(train_pred)], [min(target), max(target)], color='red')  # Diagonal line
plt.show()

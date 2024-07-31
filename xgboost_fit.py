import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.model_selection import train_test_split
from sklearn.metrics import root_mean_squared_error, r2_score
import matplotlib.pyplot as plt
import shap

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

# Train-test split
X_train, X_test, y_train, y_test = train_test_split(features, target, test_size=0.2, random_state=42)

# Convert to DMatrix
dtrain = xgb.DMatrix(X_train, label=y_train)
dtest = xgb.DMatrix(X_test, label=y_test)

# Set parameters for XGBoost
params = {
    'objective': 'reg:squarederror',
    'eval_metric': 'rmse',
    'tree_method': 'hist',
    'device': 'cuda', 
    'alpha': 0.1,
    'lambda': 1
}

# Train the model
bst = xgb.train(params, dtrain, num_boost_round=100)

# Predict on test data
y_pred = bst.predict(dtest)

# Test metrics
test_rmse = root_mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print(f"Test RMSE: {test_rmse:.4f}")
print(f"Test R2 Score: {r2:.4f}")

# Diagnostics and Visualizations
# Feature Importance
xgb.plot_importance(bst)
plt.title('Feature Importance')
plt.show()

# Predicted vs. Actual
plt.figure(figsize=(10, 6))
plt.scatter(y_test, y_pred, alpha=0.3)
plt.plot([y_test.min(), y_test.max()], [y_test.min(), y_test.max()], color='red', linewidth=2)
plt.xlabel('Actual Values')
plt.ylabel('Predicted Values')
plt.title('Predicted vs. Actual Values')
plt.grid(True)
plt.show()

# Residual Analysis
residuals = y_test - y_pred
plt.figure(figsize=(10, 6))
plt.scatter(y_pred, residuals, alpha=0.3)
plt.axhline(y=0, color='red', linestyle='--')
plt.xlabel('Predicted Values')
plt.ylabel('Residuals')
plt.title('Residuals vs. Predicted Values')
plt.grid(True)
plt.show()

# SHAP Analysis
explainer = shap.Explainer(bst)
shap_values = explainer(X_test)

# Summary Plot
shap.summary_plot(shap_values, X_test, feature_names=features.columns)

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
saquon_data['estimated_apy'] = saquon_pred

print(saquon_data[['year_signed', 'apy_cap_pct', 'estimated_apy']])
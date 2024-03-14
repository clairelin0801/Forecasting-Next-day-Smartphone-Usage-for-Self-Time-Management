# Forecasting Next-day Smartphone Usage for Self-Time Management

## Business Problem
Many people struggle with overusing their smartphones due to ineffective time management. This repository addresses this issue by proposing a predictive model based on historical data to assist users in setting reasonable time limits, particularly for those interested in self-time management.

## Data
The data source consists of daily phone usage records from our four team members, capturing usage time every day (00:00 ~ 23:59). Additionally, external factors such as holidays, events, class hours, working hours, previous-day social media and entertainment usage, and phone pickup times are considered as they may influence phone usage time.

## Forecasting Solution
Users will receive a predicted time usage report every 00:30, enabling them to make informed self-time management decisions based on the report. This approach brings users closer to achieving their goals of proper phone usage time.

## Approach
1. Data Collection: Gather daily phone usage records from team members and external factors data.
2. Data Preprocessing: Clean and preprocess the data to prepare it for analysis.
3. Feature Engineering: Extract relevant features from the data that may influence smartphone usage.
4. Model Development: Develop a predictive model using R based on historical data and external factors.
5. Model Evaluation: Evaluate the performance of the model using appropriate metrics and techniques.
6. Deployment: Implement a system to generate and deliver predicted time usage reports to users.

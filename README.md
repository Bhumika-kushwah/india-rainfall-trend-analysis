# 🌧️ Analysis and Visualization of Long-Term Rainfall Trends in India
## 📌 Project Overview

This project presents an end-to-end data analysis pipeline for studying long-term rainfall patterns in India using historical data. The analysis focuses on temporal trends, seasonal variability, regional differences, extreme rainfall events, and forecasting, following methodologies commonly used in climate and remote sensing research.

The project is designed in alignment with research practices relevant to
Indian Space Research Organisation (ISRO), particularly in climate and geospatial data analysis.


## 🎯 Objectives

Analyze long-term rainfall trends across India

Study seasonal and monthly rainfall characteristics

Measure rainfall variability using statistical indicators

Identify extreme rainfall events

Forecast future rainfall using time-series models

Visualize spatial rainfall distribution using interactive dashboards


**📂 Dataset**

Name: IMD Subdivision-wise Rainfall Data (1901–2015)

Source: India Meteorological Department (IMD)

Frequency: Monthly

Unit: Millimeters (mm)

Coverage: Indian meteorological subdivisions

Note: Publicly available government data was used for academic and research purposes.

**🛠️ Tools & Technologies**

Python – Data cleaning and preprocessing

R – Statistical analysis and time-series modeling

Power BI – Interactive dashboards and geospatial visualization

**🔄 Project Workflow**
Raw IMD Dataset
      ↓
Python Data Cleaning & Reshaping
      ↓
R Statistical Analysis & Forecasting
      ↓
Power BI Visualization & Dashboard


**📊 Statistical Methods Used**

Descriptive statistics (mean, median, variance, standard deviation)

Skewness analysis for extreme rainfall detection

Coefficient of Variation (CV) for variability assessment

Extreme rainfall threshold (95th percentile)

Trend analysis (linear regression)

Mann–Kendall non-parametric trend test

Time-series modeling using ARIMA

**📈 Key Insights**

Rainfall in India exhibits high inter-annual variability

Monsoon months dominate seasonal rainfall patterns

Significant regional disparities in rainfall distribution

Increasing occurrence of extreme rainfall events in recent decades

Forecasting results indicate continued rainfall variability

**🗺️ Dashboard Highlights (Power BI)**

State-wise average rainfall (Filled Map)

Year-wise rainfall trends

Monthly and seasonal rainfall analysis

Interactive slicers for year, month, and region

**📁 Repository Structure**
📦 Rainfall-Analysis-India
 ┣ 📂 data
 
 ┃ ┗ imd_rainfall_clean.csv
 
 ┣ 📂 python
 
 ┃ ┗ data_cleaning.ipynb
 ┣ 📂 r
 
 ┃ ┗ statistical_analysis.R
 ┣ 📂 powerbi
 
 ┃ ┗ rainfall_dashboard.pbix
 ┣ 📂 report
 
 ┃ ┗ ISRO_Rainfall_Data_Analysis_Project_Report.pdf
 
 ┗ README.md


**🚀 Future Scope**

Integration of satellite-based rainfall datasets (GPM/TRMM)

Machine learning models for improved rainfall prediction

GIS-based high-resolution spatial analysis

Climate change impact assessment

**👩‍💻 Author**

Bhumika Kushwah
B.Tech – Computer Science (AIML)
Aspiring Data Analyst | Climate & Geospatial Data Enthusiast

**📜 License**

This project is for educational and research purposes only.
Dataset credits belong to the India Meteorological Department (IMD).

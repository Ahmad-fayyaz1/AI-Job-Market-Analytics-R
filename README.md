# AI-Job-Market-Analytics-R
Interactive R Shiny dashboard for analyzing global AI job market trends and salary forecasting.
# 📊 Global AI Job Market Intelligence Dashboard

## 🚀 Project Overview
This interactive **R Shiny** dashboard provides a deep-dive analysis of the global AI employment landscape. It allows users to visualize salary distributions, industry trends, and job requirements using a dataset of 15,000+ AI-related roles.

<p align="center">
  <img src="dashboard.png" width="800" title="R Shiny Dashboard Preview">
</p>

## 🛠️ Key Features
* **Market Explorer:** Interactive visualizations of salary trends across different countries and industries.
* **Talent Flow:** Uses **Sankey Diagrams** to show the relationship between experience levels and employment types.
* **Salary Predictor:** A built-in **Decision Tree (rpart)** model that estimates potential salary based on user-inputted job parameters.
* **Job Browser:** A searchable, filtered table for detailed exploration of specific job listings.

## 🧰 Tech Stack
* **Language:** R
* **Framework:** Shiny, Shinydashboard
* **Data Manipulation:** Dplyr, Tidyr
* **Visualization:** Plotly, Ggplot2, DT (DataTables)
* **Modeling:** Rpart (Decision Trees)

## 📂 Repository Structure
* `SP24-BST-002.R`: Main application file containing UI and Server logic.
* `ai_job_dataset1.csv`: Dataset used for the analysis.

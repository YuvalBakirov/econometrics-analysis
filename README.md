# **Sweden Education Reform - Econometric Analysis**

## **Overview**  
This project investigates the impact of Sweden’s **1940s educational reform** on schooling and labor market outcomes using econometric techniques. A **difference-in-differences (DiD) approach** is applied to assess how the reform influenced years of education and earnings, following the framework of Meghir & Palme (2005).  

## **Key Features**  
- 📊 **Causal Analysis**: Estimates the effect of the reform using difference-in-differences.  
- 📈 **Regression Models**: Applies multiple OLS regressions, interaction terms, and fixed effects.  
- 🏛️ **Policy Insights**: Examines how the reform affected different socioeconomic groups.  

## **Dataset & Methodology**  
- **Dataset**: Swedish survey data on individuals born in **1948** and **1953**.  
- **Method**: Difference-in-differences regression to isolate reform effects.  
- **Variables**: Years of schooling, earnings, parental education, and ability levels.  
- **Tools**: Implemented in **R**, using packages like `tidyverse`, `lmtest`, `stargazer`, and `plm`.  

## **Installation & Usage**  
### **1️⃣ Clone the repository**  
```bash
git clone https://github.com/your-username/your-repo-name.git
cd your-repo-name
````

### **2️⃣ Install required R packages**
````bash
install.packages(c("tidyverse", "lmtest", "stargazer", "plm", "sandwich", "car", "lfe", "knitr"))
````

### **3️⃣ Run the analysis script**
````bash
source("project_51.R")
````

## **Results Summary**

✔ Increased education: The reform led to a significant rise in schooling years, particularly for students from lower-income backgrounds.<br>
✔ Higher earnings: The additional education translated into increased wages, especially for those with less-educated parents. <br>
✔ Gender & Socioeconomic Effects: Differences were observed in how the reform impacted males vs. females and students from different family backgrounds.<br>

## **Authors**

👤 **Yuval Bakirov**
👤 **Eitan Bakirov**

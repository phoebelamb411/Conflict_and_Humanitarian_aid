# Conflict_and_Humanitarian_aid
Analysis of conflict deaths vs humanitarian aid (prominent vs underrated conflicts) using UCDP, OCHA, and World Bank data.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

# Conflict and Humanitarian Aid Analysis (2024)

This project explores the relationship between reported conflict deaths and humanitarian aid distribution.  
I compared **prominent conflicts** (Ukraine, Israel, Sudan, Syria, Gaza) with **underrated conflicts** (Ethiopia, Mali, Somalia, DRC, Cameroon).  

The goal is to highlight how **humanitarian responses often mirror global attention**, even when suffering does not align with headlines.  

---

## 📊 Visualizations

### Prominent Conflicts: Impact vs Humanitarian Funding
![Prominent Conflicts](prominent_conflicts.png)

### Underrated Conflicts: Deaths vs Humanitarian Aid
![Underrated Conflicts](underrated_conflicts.png)

---

## 📂 Files in this Repository
- `prominent_conflicts.png` → Chart showing deaths vs aid for most talked-about conflicts  
- `underrated_conflicts.png` → Chart showing deaths vs aid for underrated conflicts  
- `ocha_funding.csv` → Humanitarian funding data from OCHA  
- `ucdp_conflict_deaths.csv` → Conflict death data from UCDP  
- `population_dataset.csv` → World Bank WDI population data  
- Regional aggregated datasets: Africa, Middle East, Europe-Central Asia  

---

## 📂 Data Sources
- **UCDP Battle-Related Deaths (bd_best)** – conflict death estimates  
- **OCHA Financial Tracking Service** – humanitarian aid funding  
- **World Bank WDI** – population data  

---

## 🔎 Key Findings
- Prominent conflicts often receive proportionally higher humanitarian aid.  
- Underrated conflicts, despite high death tolls, receive less visibility and resources.  
- Aid often mirrors *attention*, not necessarily *need*.  

---

## 🛠️ Learning Journey
If you open the repo, you’ll see multiple attempts and warnings in the R script — that’s intentional, it reflects the real trial-and-error process I went through. This project wasn’t built in one clean run.  
I started by just filtering for the largest conflicts (100k+ deaths), then shifted focus to current events, and eventually added the “underrated” group because I wanted to highlight overlooked crises.  

In the end, I produced two charts that highlight how humanitarian responses often mirror global attention more than actual human suffering.

---

## 💡 Reflection
This is my **first international data project** outside of coursework for my MSBA at Georgetown.  
It took me nearly a week of 8+ hour days to get these results — a mix of persistence, trial-and-error, and learning new R tricks on the fly. It reflects my long-term goal of combining **data analytics and international affairs** to highlight both challenges and opportunities in global outreach.  

---

👩🏻‍💻 Author: Phoebe Lamb ([@phoebelamb411](https://github.com/phoebelamb411))  

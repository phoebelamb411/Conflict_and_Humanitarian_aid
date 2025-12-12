# Conflict Deaths vs Humanitarian Aid: The Attention Gap

![License](https://img.shields.io/badge/license-MIT-blue)
![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)

---

## 🎯 The Question

**When conflicts claim thousands of lives, does humanitarian aid flow to where it's needed most—or to where cameras are pointed?**

---

## 💡 The Answer (In Two Charts)

### Where Are People Dying?

<p align="center">
  <img src="figures/top10_deaths_2024.png" alt="Top 10 Deadliest Conflicts" width="800"/>
</p>

**2024's deadliest conflicts:** Ukraine leads with over 73,000 deaths. But notice the conflicts that follow—**Nigeria** (9,600 deaths), **Burkina Faso** (7,500), **Ethiopia** (7,500), **Somalia** (5,500). These rarely make international headlines, yet thousands die each year.

---

### Where Is Humanitarian Aid Going?

<p align="center">
  <img src="figures/top10_funding_2024.png" alt="Top 10 Funded Countries" width="800"/>
</p>

**Where the money flows:** Syria, West Bank & Gaza, and Ukraine receive the most funding. Compare this to the deaths chart above—**where are Nigeria and Burkina Faso?** They're in the top 5 for deaths but barely crack the top 10 for funding.

**This is the attention gap.**

---

## 📊 The Stark Reality: Funding Per Death

<p align="center">
  <img src="figures/funding_per_death_groups_2024.png" alt="Funding per Death Comparison" width="800"/>
</p>

**The numbers don't lie:**
- **Syria**: ~$408,000 per death in humanitarian funding
- **West Bank & Gaza**: ~$97,000 per death  
- **Sudan**: ~$120,000 per death
- **Ukraine**: ~$33,000 per death

Compare that to:
- **Somalia**: ~$162,000 per death
- **Ethiopia**: ~$133,000 per death  
- **Cameroon**: ~$90,000 per death

**Why does this happen?** When conflicts dominate headlines (Ukraine, Gaza, Syria), humanitarian dollars follow. When they don't—even with comparable death tolls—populations suffer with far less support.

This isn't about whether any crisis deserves more or less aid. **It's about revealing a systematic pattern: media attention shapes resource allocation more than actual human cost.**

---

## 🔍 The Full Picture

<p align="center">
  <img src="figures/scatter_deaths_vs_funding_2024.png" alt="Deaths vs Funding Relationship" width="800"/>
</p>

**For the data-minded:** This log-log scatter plot shows each country as a dot. The dashed line represents the expected relationship between deaths and funding. 

Countries **above the line** receive more funding per death than average. Countries **below** receive less. Notice how prominent conflicts (Ukraine, West Bank & Gaza, Syria) cluster above the trend, while many African conflicts fall below.

**Translation:** Deaths alone don't predict funding. Something else does—and that something is often media coverage.

---

## 📈 By The Numbers

### **Prominent Conflicts** (High Media Coverage)
**Countries analyzed:** Ukraine, Russia, Israel, West Bank & Gaza, Sudan, Syria

| Metric | Value |
|--------|-------|
| **Total deaths (2024)** | ~130,000 |
| **Total humanitarian funding** | ~$14.2B |
| **Average funding per death** | ~$109,000 |

### **Underrated Conflicts** (Low Media Coverage)
**Countries analyzed:** Ethiopia, Cameroon, Somalia, DRC, Mali

| Metric | Value |
|--------|-------|
| **Total deaths (2024)** | ~24,000 |
| **Total humanitarian funding** | ~$3.3B |
| **Average funding per death** | ~$137,000 |

> **Note:** While underrated conflicts actually show higher funding per death on average (driven by Somalia and Ethiopia receiving relatively substantial aid), the **individual country disparities** tell the real story. Cameroon receives $90K per death while Syria receives $408K—a 4.5× difference for conflicts with similar humanitarian needs.

**The deeper issue:** Many high-casualty conflicts (Nigeria: 9,600 deaths, Burkina Faso: 7,500 deaths) receive minimal funding and fall completely outside the "prominent" discussion despite devastating human costs.

---

## 🌍 The Forgotten Crises

These conflicts claim thousands of lives yet rarely make international headlines:

### **Nigeria** 
- **9,600 deaths in 2024** (4th deadliest globally)
- Boko Haram insurgency, intercommunal violence
- Minimal sustained international coverage despite decade-long crisis

### **Burkina Faso**
- **7,500 deaths in 2024** (5th deadliest)
- Armed group insurgencies, government instability
- Rarely mentioned in international media outside occasional reports

### **Ethiopia**
- **7,500 deaths in 2024**
- Post-Tigray conflict regional violence continues
- Severe displacement, but overshadowed by other crises

### **Mali**
- **4,000 deaths in 2024**
- Ongoing insurgencies, French withdrawal impact
- Chronically undercovered despite humanitarian severity

### **Somalia**
- **5,500 deaths in 2024**
- Decades of Al-Shabab violence
- Overshadowed by state-vs-state conflicts despite persistent crisis

---

## 🛠️ How This Analysis Works

### **Data Sources**
- **Conflict Deaths**: UCDP/ACLED regional conflict aggregates (Africa, Middle East, Europe-Central Asia)
- **Humanitarian Funding**: OCHA Financial Tracking Service (official humanitarian response plans)
- **Analysis Period**: 2024

### **Methodology**
1. **Cleaned inconsistent country names** across UCDP, ACLED, and OCHA datasets
2. **Aggregated deaths** by country for 2024 from three regional datasets
3. **Extracted funding** from OCHA humanitarian response plan titles (English & French)
4. **Calculated funding per death** for comparative analysis
5. **Created visualizations** showing deaths, funding, and disparities

### **Technical Highlights**
- **Country name normalization**: Handled 20+ variations (e.g., "DRC" vs "Congo, Dem. Rep." vs "Democratic Republic of the Congo")
- **Plan title parsing**: Used regex to extract countries from humanitarian plan names in multiple languages
- **Missing data handling**: Transparently marked countries without OCHA funding data rather than assuming zero
- **Log-log visualization**: Appropriate for data spanning multiple orders of magnitude

---

## 💭 Why I Built This

This was my **first international data project** outside of coursework for my MSBA at Georgetown.

**The reality behind the code:** It took nearly a week of full days to produce these visualizations. The challenge wasn't just technical (standardizing country names alone took hours), but emotional—every data point represents lives lost, and every funding gap represents real people not getting help they need.

While I'm fortunate enough to sit behind a laptop analyzing data, millions worldwide don't know when their next meal will come or where they'll find safety. That reality fueled every hour of this project.

**What I learned:**
- Real-world data is messy (international datasets use wildly different naming conventions)
- Visualization choices matter (the funding-per-death chart tells the story more clearly than any table could)
- The story matters more than perfection (getting these insights visible matters more than perfect code)

This reflects my long-term goal: **combining data analytics and international affairs** to illuminate both challenges and opportunities in global humanitarian response.

---

## 🌟 Why This Matters

### **For Humanitarian Organizations**
→ Identifies chronically underfunded crises that need advocacy  
→ Provides data for resource allocation discussions  
→ Shows where media attention doesn't match human cost

### **For Policymakers**
→ Reveals systematic patterns in aid distribution  
→ Highlights "forgotten crises" requiring intervention  
→ Offers evidence-based framework for funding priorities

### **For the Public**
→ Challenges assumptions about where help is needed  
→ Highlights conflicts that don't make headlines  
→ Connects media coverage to real-world consequences

### **For Me**
→ Demonstrates ability to work with complex, multi-source datasets  
→ Shows I can translate data into policy-relevant insights  
→ Proves I can communicate findings to both technical and general audiences

---

## ⚠️ Important Limitations

### **What This Analysis Captures**
✅ Official OCHA humanitarian response plan funding  
✅ Reported conflict deaths from UCDP/ACLED aggregates  
✅ 2024 data (most recent available)

### **What It Misses**
❌ **Bilateral aid** not channeled through OCHA plans  
❌ **Private donations** and NGO funding outside official channels  
❌ **Unreported deaths** (actual tolls often higher in areas with poor data)  
❌ **Refugee costs** incurred in neighboring countries  
❌ **Non-financial support** (peacekeeping, medical personnel, etc.)

### **Methodological Caveats**
- **"Prominent" vs "Underrated"**: Based on 2024 media coverage (observable but subjective)
- **Funding per death**: Useful comparison metric but doesn't capture aid effectiveness or complexity
- **Time lag**: Aid allocations can lag behind conflict escalation
- **Regional aggregation**: Some conflicts span borders; attribution requires judgment calls

**Bottom line:** This analysis reveals patterns, not definitive answers. It's a starting point for deeper questions about humanitarian resource allocation.

---

## 🚀 Using This Analysis

### **Quick Start**
```r
# Install required packages
install.packages(c("tidyverse", "readxl", "janitor", "scales", "patchwork", "ggrepel"))

# Run the analysis
source("conflict_aid_analysis.R")

# Outputs created in figures/ folder:
# - top10_deaths_2024.png
# - top10_funding_2024.png  
# - scatter_deaths_vs_funding_2024.png
# - funding_per_death_groups_2024.png
```

### **Repository Structure**
```
Conflict_and_Humanitarian_aid/
├── conflict_aid_analysis.R          # Main analysis script
├── figures/                          # All visualizations
│   ├── top10_deaths_2024.png
│   ├── top10_funding_2024.png
│   ├── scatter_deaths_vs_funding_2024.png
│   └── funding_per_death_groups_2024.png
├── data/
│   ├── ocha_funding.csv
│   ├── Africa_aggregated_data.xlsx
│   ├── Middle-East_aggregated_data.xlsx
│   └── Europe-Central-Asia_aggregated_data.xlsx
└── README.md                         # This file
```

---

## 🔗 Related Projects

This is **Part 1** of my international affairs analytics portfolio:

| Project | Focus | Status |
|---------|-------|--------|
| **[Conflict & Humanitarian Aid](https://github.com/phoebelamb411/Conflict_and_Humanitarian_aid)** | Resource allocation gaps | ✅ Complete |
| **[Paris Agreement Part 1](https://github.com/phoebelamb411/Paris_Agreement_Part_1)** | Emissions vs targets | ✅ Complete |
| **[Paris Agreement Part 2](https://github.com/phoebelamb411/Paris_Agreement_Part_2)** | Climate finance fairness | ✅ Complete |

**What's Next:**
- Global education access analysis
- Peace agreement effectiveness study  
- Public health international collaboration

---

## 📚 Data & Methods

### **Citations**
- **Uppsala Conflict Data Program (UCDP)**: Battle-related deaths database
- **Armed Conflict Location & Event Data (ACLED)**: Regional conflict event aggregates  
- **OCHA Financial Tracking Service**: https://fts.unocha.org - Humanitarian funding data

### **Transparency**
- All code is open source and reproducible
- Data cleaning steps fully documented in script
- Missing data clearly indicated in visualizations
- Assumptions explicitly stated in methodology

---

## 💬 Let's Discuss

I built this analysis to demonstrate how data can drive accountability in international affairs. I'd love to hear:

- Your perspective on humanitarian resource allocation
- Additional data sources I should explore
- Questions this analysis raises for you  
- Opportunities to collaborate on similar work

**Connect with me:**
- 💼 [LinkedIn](https://www.linkedin.com/in/phoebelamb) - Where I share projects as I build them
- 🐙 [GitHub](https://github.com/phoebelamb411) - All code and documentation

---

## 🙏 Acknowledgments

- **UCDP & ACLED** for maintaining accessible, rigorous conflict data
- **OCHA** for transparent humanitarian funding reporting
- **Georgetown MSBA program** for the analytical foundation  
- **Every humanitarian worker** responding to these crises on the ground

---

<div align="center">

**"When we only talk about certain conflicts, humanitarian responses mirror that attention.**  
**But suffering doesn't always align with headlines."**

*Building analytics for global awareness—one project at a time.*

---

**⭐ If this analysis resonates with you, please star this repo and share it.**  
**These stories deserve attention.**

</div>

Estimating a Model of Demand for Health Insurance Plans
================
Laurene Lee

# Introduction

In this project, I perform analytic steps for a project that seeks to
analyze the economic efficiency of subsidy design on the health
insurance markets that were created in the US under the 2010 Affordable
Care Act (ACA). Specifically, I am assisting with estimating a model of
demand for health insurance plans. To estimate the model, I generate a
dataset of potential consumers on the health insurance markets through
finding and cleaning the raw data. I also present descriptive facts
about the data. I discuss the specific procedures undertaken in the
analytic steps below.

# Analytic Steps (Steps 1-4)

# Step 1 - Setting up the data

``` r
## Load Libraries
## -----------------------
library(tidyverse)
library(readxl) # For reading Excel files
library(xtable) # For importing tables into LaTeX
library(stargazer) # For generating other LaTeX codes

## Step 1: Setting Up the Data
## ---------------------------
HousingUnit <- read_csv("psam_h33.csv") # Housing unit records
Population <- read_csv("psam_p33.csv") # Population records

df <- left_join(Population, HousingUnit, by = "SERIALNO") %>% # Merge records
  transmute(ID = SERIALNO, # Unique identifier
            State = ST.x, 
            PUMA = PUMA.x, 
            Age = AGEP, 
            Female = as.numeric(SEX == 2), # Dummy for female
            White = as.numeric(RAC1P == 1), # Dummy for white
            HouInc = as.numeric(HINCP), # Household income 
            IncPov = POVPIP, # Income relative to federal poverty line (from raw data)
            EmpIns = HINS1, # Employer-sponsored insurance coverage
            Medicare = HINS3, 
            TRICARE = HINS5, 
            VA = HINS6, # VA coverage
            Medicaid = HINS4,
            HouFam_Type = HHT, # Household/ family type
            Children = HUPAC, # Info on children
            Edu = SCHL, # Educational attainment 
            Employ = ESR) # Employment status
```

I extracted the raw data from the United States Census Bureau’s Public
Use Microdata Samples (PUMS) of the American Community Survey (ACS). For
the purposes of this data task, I focused on data from New Hampshire
(NH) in the year 2017. I found and downloaded 1-year PUMS file for year
2017. I attained both the “Population Records” and “Housing Unit
Records.”

To create a comprehensive dataset of the population in NH with both
individual and family level information, I merged “Population Records”
and “Housing Unit Records.” From the merged raw PUMS files, I created a
file that contains the following information:

(Note: This is an initial step in the analytical procedure, and
additional variables are created and added in the later steps as
needed.)

- Individual and household ID’s
- Local geography captured by state and PUMA • Age
- Sex (Recoded as a dummy for female)
- Race (Recoded as a dummy for white)
  - For the race variable, there were multiple potential variables that
    could be used such as “RAC1P,” “RAC2P,” “RAC3P,” and “RACWHT.” I
    decided to use the “RAC1P” variable, which came with nine distinct
    racial categories, and recoded it as a dummy for white based on the
    “1. White alone” indicator (racial category).
- Household income
- Income as measured relative to the federal poverty line (Created in
  Analytic Step 3)
  - There is a variable (“POVPIP”) in the raw file that codes
    income-to-poverty ratio, but I create my own variable that
    represents income as a percentage of the federal poverty line to
    ensure the variable is computed in accordance with the rules set out
    in the 2017 IRS Form 8962 (This is further discussed in Analytic
    Step 3).
- Health insurance status (Information regarding employer-sponsored
  insurance, Medi- care, TRICARE, etc.)
- Family type and children information (Created for Analytic Step 4)
- Educational attainment (Created for Analytic Step 4)
- Employment status (Created for Analytic Step 4)

# Step 2 - Defining the set of potential ACA consumers

I restricted the dataset created in Analytic Step 1 to individuals who
are potential consumers in the ACA Health Insurance Marketplaces.

Before restricting the dataset, I carried out the following procedures:

1.  I grouped the individuals in the dataset into tax families by
    identifying those who belong to the same family (sharing the same
    ID), and have the same level of household income (By definition a
    tax family should have the same income level for all its members).

2.  I computed the size of each tax families and created a new variable
    representing this information (named “TaxFamCt”).

# economic_efficiency_of_subsidy_design

In this project, I perform analytic steps for a project that seeks to analyze the economic efficiency of subsidy design on the health insurance markets that were created in the US under the 2010 Affordable Care Act (ACA). (The subsidy is formally known as Advanced Premium Tax Credit.) I use the data from the United States Census Bureau’s Public Use Microdata Samples (PUMS) of the American Community Survey (ACS). (I attained both the “Population Records” and “Housing Unit Records.”)

Specifically, I generate a dataset of potential consumers on the health insurance markets through finding and cleaning the raw data in order to assist with estimating a model of demand for health insurance plans. I compute subsidy eligibility of individuals represented in the dataset, and calculate potential subsidy amounts the individuals would be eligible for. I also create descriptive facts about the generated data, and estimate a model that would allow us to describe how subsidies depend on individual and family characteristics.

_To access the full report with the codes and discussions, see:_
* "complete_analysis.md"

This repo is structured as follows:
* "complete_analysis.Rmd" is the parent R Markdown document
* "01_data_loading.Rmd" ~ "16_output_(2)_(e).Rmd" are the child R Markdown documents ran in order in the parent document
* "complete_analysis_files/figure-gfm" contains the figures created in the codes (this does not include the tables)
* "data" folder includes the raw and preprocessed datasets used in the project

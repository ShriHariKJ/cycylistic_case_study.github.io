###### cycylistic_case_study.github.io
# Case study (Cyclistic) : How does a bike-share navigate speedy success?
![Cyclistic](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/c367afcd6ab42c22b980cfd780055503511b526b/images/Designer.png)
Image courtesy: Copilot

## Introduction

This repository contains a case study analysis of Cyclistic, a fictional bike-share company in Chicago. The analysis aims to understand how different customer segments, such as casual riders and annual members, utilize Cyclistic bikes and to provide insights for designing a new marketing strategy.

## Scenario

You’re a junior data analyst in the marketing team at Cyclistic, a bike-share company in Chicago. The company’s future success is believed to hinge on maximizing the number of annual memberships. Therefore, your team aims to understand how casual riders and annual members use Cyclistic bikes differently. The insights gained from this analysis will inform the design of a new marketing strategy to convert casual riders into annual members. However, before implementation, Cyclistic executives must approve your recommendations, which need to be supported by compelling data insights and professional data visualizations.

## About the company

Cyclistic launched a successful bike-share offering in 2016. Since then, the program has grown to a fleet of 5,824 geotracked bicycles locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. Cyclistic’s marketing strategy has relied on building general awareness and appealing to broad consumer segments. One approach that has facilitated this is the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships

## Stakeholders

The stakeholders include Lily Moreno, the director of marketing; the Cyclistic marketing analytics team, which is responsible for collecting, analyzing, and reporting data that guides Cyclistic’s marketing strategy; and the Cyclistic executive team, which will decide whether to approve the recommended marketing program

## The Business Task

The goal of this case study is to design marketing strategies aimed at converting casual riders into annual members. Moreno and her team are interested in analyzing Cyclistic’s historical bike trip data to identify trends. The key questions to answer are:

* How do annual members and casual riders use Cyclistic bikes differently?
* Why would casual riders buy Cyclistic annual memberships?
* How can Cyclistic use digital media to influence casual riders to become members?

Moreno has assigned you the first question to answer: How do annual members and casual riders use Cyclistic bikes differently? To answer this question, we’ll need to analyze Cyclistic’s historical bike trip data, identify patterns and trends, and present our findings in a clear and compelling manner.

## Data Preparation

In this phase, I will be using Cyclistic’s historical trip data to analyze and identify trends. The data for the previous 12 months (January 2023 to December 2023) can be downloaded from the provided source. Although the datasets have a different name because Cyclistic is a fictional company, they are appropriate for this case study and will enable me to answer the business questions.

The data has been made available by Motivate International Inc. under a specific license. This is public data that I can use to explore how different customer types are using Cyclistic bikes. However, it’s important to note that data-privacy issues prohibit me from using riders’ personally identifiable information. This means that I won’t be able to connect pass purchases to credit card numbers to determine if casual riders live in the Cyclistic service area or if they have purchased multiple single passes.

The chosen data encompasses a full year, from January 2023 to December 2023. Each month’s data is stored in a distinct dataset. These datasets are structured in a tabular format, each containing 13 identical columns. When combined, these datasets comprise a total of 5,71,9877 rows. The ‘member_casual’ column in these datasets will enable me to categorize, consolidate, and contrast the trends between casual riders and member riders.

## Data Processing: From Raw to Refined

### Tools Utilized 
R Studio was the preferred tool for this data processing task, primarily due to its robustness and efficiency in managing large datasets. Furthermore, R Studio’s comprehensive support from open-source libraries like dplyr and ggplot2 made it a perfect fit for the task. R Studio is particularly favored for its advanced data visualization capabilities and its ease of use in statistical analysis. This makes it an excellent tool for data cleaning, transformation, and analysis.

### Data Cleaning
The initial step after consolidating the 12 datasets into a single dataframe was to identify columns and rows with missing data. The total number of rows was 5719877 rows. It was found that 6 out of 13 columns and 1388170 rows had missing data.

Further investigation into the first and last row with missing data revealed null values across multiple columns. The percentage of rows with missing values was calculated to be 24.27 %. After examining the columns with missing data, it was determined that imputation would not be an appropriate strategy due to the nature of the missing data.

Given that rows with missing values represented a small fraction of the data and had missing values across multiple columns, the decision was made to eliminate them.

The “docked_bike” category under the “rideable_type” represents bicycles that Cyclistic has removed from service for quality control assessments. Additionally, there are several instances where the “ride_length” yields a negative value. These records need to be excluded from our dataframe. By doing so, the total number of rows will decrease from 4,331,707 to 4,255,583. This corresponds to a reduction in the row count by approximately 1.76%.

After removing rows with missing data, a check was conducted for any remaining duplicate observations. No duplicates were found, indicating that the data was ready for further processing and analysis.

### Data Transformation 
A ride_length column was then created by calculating the difference in time between the ended_at and started_at columns and stored in minutes unit.

Additional columns: day_of_week, month, date, day and year were created to represent the accurate period in which a bike was hired.

After these steps, a final summary of the data was obtained, confirming that the data was now ready for analysis.

## Data Analysis to Derive Insights

In this phase, I delved into the refined data to discern the distinct usage patterns of Cyclistic bikes between annual members and casual riders.

Initially, I determined the total count of bike hires and delineated how they were distributed between casual riders and member riders. Subsequently, I scrutinized how the total bike hires were dispersed across each month and then each day. This exploration unveiled intriguing patterns that I will elaborate on in the subsequent discussion.

Following that, I investigated how bike hires among the two categories of riders contrasted during a specific month of the year and day of the week. The objective at this juncture was to ascertain if casual riders exhibited a preference for certain days or months in comparison to member riders.

Subsequently, I aimed to contrast the disparity in average ride duration between casual riders and member riders. I found that casual riders tend to engage in longer rides compared to member riders. This piqued my curiosity, prompting me to delve into how the average ride duration compares for both rider categories on a daily and monthly basis.

Additionally, I also compared the top 10 routes for each membership type and analyzed the top 10 start and end stations for each membership type. This additional analysis provided further insights into the preferences and behaviors of the two rider categories.

Lastly, I compared how the selection of bike type varied between the two rider categories. This comprehensive analysis provided valuable insights into the distinct usage patterns of Cyclistic bikes between annual members and casual riders.

## Communicating Findings Through Visualizations

In this phase, I utilized R Studio to craft insightful visualizations that effectively communicate the outcomes of my analysis. The visualizations, along with the derived insights, can be accessed at the provided location. R Studio, with its powerful graphics capabilities, was instrumental in this process. Please note that the specific libraries used for visualization may vary based on the requirements of the analysis. The presentation explaining the visualization is attached here. [Presentation_of_findings](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/b186ebb038294fe8175ae312ec9fe7ed24e19f35/Presentation/Cyclistic_Data_Analysis.pptx)

## Principal Discoveries and Insights

- **Membership Types**: Members, who likely are residents of Chicago, make up the majority of cycle hires (65.8%). Casual users, possibly tourists or occasional riders, account for a significant portion (34.2%).
![Cycle Hires per membership type](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/3417528813fb7e6d4c3146061d9bb6ab0932fa87/Visualization_v2/Total_Cycle_Hires_per_Membership_Type.jpg)

- **Ride Lengths**: Casual users generally have longer ride lengths than members throughout all months. The most significant difference occurs in July and August, which aligns with the summer season in Chicago. This could suggest that casual users, potentially tourists, are using bikes for leisure or sightseeing purposes during the warm summer months.
![Ride length per Membership type by month](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/3417528813fb7e6d4c3146061d9bb6ab0932fa87/Visualization_v2/Average_Ride_Length_per_Membership_Type_by_Month.jpg)

- **Popular Stations**: Certain stations like *"Streeter Dr & Grand Ave"* and *"Clinton St & Washigton Blvd"* are more popular than others for both starting and ending bike rides for casual and members respectively. The popularity of start and end stations varies between casual riders and members, indicating different usage patterns. Some stations are popular among both casual riders and members, suggesting they might be located in convenient or popular areas, such as tourist attractions or major transit hubs.
![Top Stations per Membership Type](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/3417528813fb7e6d4c3146061d9bb6ab0932fa87/Visualization_v2/Top%2010%20Start%20Stations%20vs%20Top%2010%20End%20Stations%20for%20each%20membership%20type.jpg)

  
- **Popular Routes**: Certain routes are more popular than others for both casual riders and members. The popularity of routes varies between casual riders and members, indicating different usage patterns. Some routes are popular among both casual riders and members, suggesting they might be scenic or convenient routes.
![Top 10 Routes per Membership Type](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/3417528813fb7e6d4c3146061d9bb6ab0932fa87/Visualization_v2/Top_10_Routes_for_Members_and_Casual_Riders.jpg)

- **Ridership Patterns**: The total number of riders varies significantly across different days of the week and months of the year. The highest number of riders is observed on Thursday and August and the lowest on Sunday and Janurary and heatmap shows Tuesday and Wednesday consistently exhibit higher ridership throughout most months and June stands out as a month with significantly increased ridership across all weekdays. This could suggest that usage patterns are influenced by factors such as work schedules, weather conditions, and tourist seasons.
  <div style="text-align: center; margin: auto;">
    <img src="https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/3417528813fb7e6d4c3146061d9bb6ab0932fa87/Visualization_v2/Total_Riders_per_Month.jpg" width="450" />
    <img src="https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/d8d8bd51b14334019534581de6f100287eb3c880/Visualization_v2/Total_Riders_per_Day_of_Week.jpg" width="450" />
  </div>
<p align="center">
  <img src="https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/d8d8bd51b14334019534581de6f100287eb3c880/Visualization_v2/Total_Number_of_Riders_Across_Days_of_the_Week_and_Months_of_the_Year.jpg" width="450" />
</p>

 
These insights provide a comprehensive understanding of how annual members and casual riders use Cyclistic bikes differently in Chicago. They can inform strategies to increase usage, improve services, and potentially convert casual riders to members. 

## Recommendations

- **Casual User Conversion** : Given that casual users account for a significant portion of total cycle hires and tend to have longer ride lengths, there may be an opportunity to convert these users into members. This could be achieved through targeted marketing strategies or offering trial memberships.
- **Targeted Station and Route Improvements**: For stations and routes that are popular among both casual riders and members, consider improvements or expansions to accommodate the high usage. For stations and routes that are popular among casual riders but not members (or vice versa), consider targeted marketing strategies to attract the other group.
- **Seasonal Adjustments**: Given the variations in ridership across different months, consider seasonal adjustments such as offering promotions during low-ridership months like January and February or enhancing bike availability during high-ridership months like July and August.

## Conclusion

The analysis provides valuable insights into Cyclistic bike usage and lays the groundwork for developing effective marketing strategies to drive growth and conversion.

---
*Note: These insights and recommendations are based on visualizations created using R Studio
For more details, please refer to the [full analysis](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/blob/6ad06e4413ef9b85c0fcb8679f7ed4f9884c1d95/Scripts/Cyclist_202301_to_202312.R) and [visualizations](https://github.com/ShriHariKJ/cycylistic_case_study.github.io/tree/6ad06e4413ef9b85c0fcb8679f7ed4f9884c1d95/Visualization_v2) in this repository.*


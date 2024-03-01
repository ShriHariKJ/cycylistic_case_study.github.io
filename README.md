###### cycylistic_case_study.github.io
# Case study (Cyclistic) : How does a bike-share navigate speedy success?
![Image Alt Text](images/Designer.png)

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

In this phase, I will be using Cyclistic’s historical trip data to analyze and identify trends. The data for the previous 12 months can be downloaded from the provided source. Although the datasets have a different name because Cyclistic is a fictional company, they are appropriate for this case study and will enable me to answer the business questions.

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

In this phase, I utilized R Studio to craft insightful visualizations that effectively communicate the outcomes of my analysis. The visualizations, along with the derived insights, can be accessed at the provided location. R Studio, with its powerful graphics capabilities, was instrumental in this process. Please note that the specific libraries used for visualization may vary based on the requirements of the analysis.

## Principal Discoveries and Insights

- **Membership Types**: Members, who likely are residents of Chicago, make up the majority of cycle hires (65.8%). Casual users, possibly tourists or occasional riders, account for a significant portion (34.2%).
- **Ride Lengths**: Casual users generally have longer ride lengths than members throughout all months. The most significant difference occurs in July and August, which aligns with the summer season in Chicago. This could suggest that casual users, potentially tourists, are using bikes for leisure or sightseeing purposes during the warm summer months.
- **Popular Stations**: Certain stations are more popular than others for both starting and ending bike rides. The popularity of start and end stations varies between casual riders and members, indicating different usage patterns. Some stations are popular among both casual riders and members, suggesting they might be located in convenient or popular areas, such as tourist attractions or major transit hubs.
- **Popular Routes**: Certain routes are more popular than others for both casual riders and members. The popularity of routes varies between casual riders and members, indicating different usage patterns. Some routes are popular among both casual riders and members, suggesting they might be scenic or convenient routes.
- **Ridership Patterns**: The total number of riders varies significantly across different days of the week and months of the year. The highest number of riders is observed on Monday and January, and the lowest on Wednesday and February. This could suggest that usage patterns are influenced by factors such as work schedules, weather conditions, and tourist seasons.

These insights provide a comprehensive understanding of how annual members and casual riders use Cyclistic bikes differently in Chicago. They can inform strategies to increase usage, improve services, and potentially convert casual riders to members. 

## Recommendations:

- **Casual User Conversion** : Given that casual users account for a significant portion of total cycle hires and tend to have longer ride lengths, there may be an opportunity to convert these users into members. This could be achieved through targeted marketing strategies or offering trial memberships.
- **Targeted Station and Route Improvements**: For stations and routes that are popular among both casual riders and members, consider improvements or expansions to accommodate the high usage. For stations and routes that are popular among casual riders but not members (or vice versa), consider targeted marketing strategies to attract the other group.
- **Seasonal Adjustments**: Given the variations in ridership across different months, consider seasonal adjustments such as offering promotions during low-ridership months like February or enhancing bike availability during high-ridership months like July.

Note: These insights and recommendations are based on visualizations created using R Studio. Please find the visualizations here [Link](Scripts/Cyclist_202301_to_202312.R)

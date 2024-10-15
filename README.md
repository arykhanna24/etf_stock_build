**Purpose**
- To collate information on stock and ETF returns for the past 20 years
- Try to understand what sectors/individual firms have returned consistently high returns over the past 5, 10, 15, 20 years.
- What sectors/firms did well in 2000s but plummeted during the 2010s, which refused to abate? Which new sectors/firms have done well in the past 5/10 years?

**Data (Imports)** 
- NASDAQ ETF Screener -- a comprehensive dataset comprising of descriptive information for ETFs
- NASDAQ Stock Screener-- a comprehensive dataset comprising of descriptive information for ETFs
  
**Code**
- Leveraging the tidyquant package to pull yearly returns and calculate cumulative returns for the past 5, 10, 15, 20 years.
- Repeating the above for both stocks and ETFs -- calculating cumulative geometric average returns accounting for compounding.
- Playing around with ranking flags based on time-period and recency of high performance.

**Outputs**
- See the builds folder.

# rvest_degree_curriculum
R webscraper with rvest: extracts University's courses curriculum

This sample project shows how to use rvest library to do webscraping in R.

The target website is Brazilian private university Mackenzie, from which we will extract a list of all degrees available and their curriculum, i.e., all the courses required to complete each degree.

Besides rvest, other support libraries are also used in the project:
- dplyr: to store the extracted data;
- tidyr: to organize the data;
- gt: to present the data in a well-formatted table;
- purrr: to get rid of several loops that were part of the first version of the code. Webscraping usually relies on loops, as we need to repeatedly navigate through pages, tabs, tables and lines.
# EVRTT - Emerging Viruses Research Tracking Tool

EVRTT ("Everett") is an R Shiny web application that is designed to track research publications 
both temporally and geographically. Users are given the ability to choose an NCBI database that 
a virus (or virus subfamily) against. Hits are returned and parsedVIRAL for pubMed accession 
numbers and year of publication. A line chart of publications by year is generated and displayed 
to the user. The pubMed accession numbers are used to access pubMed webpages and the HTML is 
parsed via regex to identify the geographical location of the author(s). These geographical 
locations are then passed to an R implementation of the Google Maps API, which uses latitude and 
longitude to overlay points on a world map. The darker red the location is, the more publications 
originated from this location. 

EVRTT is capable of running on a web server and provides an easy-to-use GUI, while also supporting 
multithreading for processing large datasets. 

![Line-chart](/Mononegairales_trends_over_time)
Format: ![](url)

![Map](/Mononegairales_date)
Format: ![](url)

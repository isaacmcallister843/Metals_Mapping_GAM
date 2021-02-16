# Metals_Mapping_GAM
Predictive metals mapping using generalized additive modeling

## Motivation 
A large concern for mining projects is heavy metal contimnination. In the enviromental perminiting procces mining companies must do a detailed survey of heavy metal levels on their future mine site. Historically this procces was very surface level, scientists would idenitfy areas of concern and field teams would sample at those locations. However, with modern data tools detailed maps of metal concenrations are possible. With this project our team developed several methods of predicting heavy metal concentrations. Generalized additive modeling is showcased here. This method involves using polynomerials smoother functions to identify predictors that are relevant to metal concentrations. This was one of the first large scale data science projects I worked to develop. 

## Intellectual Property
Since I worked in a team to develop these products for large clients I do not have permission to distribute the raw data used in these projects. I have also remnamed any identifing files and removed the location data from the showcased images. The final products were geo TIFF files, however they are been run through GIS to create viewable images. 

## Data and Modeling
The data was collected by external teams and was presented as a csv and after proccesing was in the following columns: 

|sample_id|Easting|Northing|Au|Cu|Pb|Zn|grid|Subsurface Material|Elevation|Slope|Aspect| 
|---------|-------|--------|--|--|--|--|----|-------------------|---------|-----|------|  

The GAM model used a base function to perform modeling: 
```Math
Metal ~ s(Easting, Northing) + s(Aspect) + te(Easting, Northing, k=3)
```

## Features 
- Detailed anaylis of geospatial data. Including automatic correction for spatial auto correlation and outlier filtering. 
- 



# Metals_Mapping_GAM
Predictive metals mapping using generalized additive modeling

![Raster Plot](https://user-images.githubusercontent.com/78721353/108017788-9d6a5300-6fca-11eb-94b4-5ff47191b014.PNG)
#### Figure 1: Lead Concentration Predictions, Scales Removed


## Motivation 
A large concern for mining projects is heavy metal contamination. In the environmental permitting process mining companies must do a detailed survey of heavy metal levels on their future mine site. Historically this process was very surface level, scientists would identify areas of concern and field teams would sample at those locations. However, with modern data tools detailed maps of metal concentrations are possible. With this project our team developed several methods of predicting heavy metal concentrations. Generalized additive modeling is showcased here. This method involves using polynomials smoother functions to identify predictors that are relevant to metal concentrations. This was one of the first large scale data science projects I worked to develop. 

## Intellectual Property
Since I worked in a team to develop these products for large clients I do not have permission to distribute the raw data used in these projects. I have also renamed any identifying files and removed the location data from the showcased images. The final products were geo TIFF files, however they have been run through GIS to create viewable images. 

## Data and Modeling
The data was collected by external teams and was presented as a csv and after processing was in the following columns: 

|sample_id|Easting|Northing|Au|Cu|Pb|Zn|grid|Subsurface Material|Elevation|Slope|Aspect| 
|---------|-------|--------|--|--|--|--|----|-------------------|---------|-----|------|  

The GAM model used a base function like the one below to perform modeling.

```Math
Metal ~ s(Easting, Northing) + s(Aspect) + te(Easting, Northing, k=3)
```
The function ana() automatically compares various functions and picks the one with the highest AIC value. Tensor effects were often stopped at k=3 since the smallest grid had around 23 data points and the tensor predictor needs a min of 30. 

In some cases the models work best with a spatial transform to put the data points in a grid, this is done manually by the grid 2 function. 

## Features 
- Detailed analysis of geospatial data. Including automatic correction for spatial auto correlation and outlier filtering. 
- Automatic selection of best fit models 
- Advanced error handeling and redudancy features 
- Map generation and raster output
- Bubble map creation 


![Bubble Maps](https://user-images.githubusercontent.com/78721353/108017948-27b2b700-6fcb-11eb-830d-38a198f4f0c5.PNG)
### Figure 2: Bubble Map for Various Metals

## Code Examples

First a grid needs to be created using the grid2 function. 

```R
xy_coords <- grid2(dataframe,rownum = 6, sens = 5, theta = 1)
```
The new grid is now pasted with the dataframe into the ana function. This function performs all needed analysis and outputs a raster

```R
output_list <- ana(dataframe,xy_coords)
```

## Final Products
Can be seen in the Final Images folder. 









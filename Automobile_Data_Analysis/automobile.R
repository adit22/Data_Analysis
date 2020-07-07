library('readxl')
#to read excel data
car_data<-read_excel("C:\\Users\\Mark 1\\Desktop\\Lockdown\\R\\Automobile_Data_Analysis\\auto.xlsx",col_names = c('symboling','norm_loss','make','fuel_type','aspirations','num_doors','body_style','drive_wheel','engine_loc','wheel_base','length','width','height','curb_weight','engine_type','num_cylinders','engine_size','fuel_sys','bore','stroke','compression_ratio','horsepower','peak_rpm','city_mpg','highway_mpg','price'))


#to see colname
c(colnames(car_data))


#to see dimension of dataset
dim(car_data)


#to see type of column
str(car_data)
library('dplyr')
glimpse(car_data)


#to see summary of dataset
summary(car_data)


#to see na values
names(which(sapply(car_data, anyNA)))
is.na(car_data)


#to see if vector contains a specific value or not
t<-c(is.na(car_data$symboling))
for (i in t){
  if(i!=FALSE){
    print("no true values")
  }
}
TRUE %in% t
FALSE %in% t
"?" %in% car_data$symboling


#looping over column names in dataset
for (i in colnames(car_data)){
  print(i)
}



#to see whether a column contain "?" 
vec<-vector()
for (i in colnames(car_data)){
  
  quescol<-c("?" == car_data[,i])
  if(TRUE %in% quescol){
    vec<-c(vec,i)
  }
}
vec



#a function to do such task of finding desired character, further such function can be made for multiple symbols
symb_finder<- function(ds,symb){
  vec<-vector()
  for (i in colnames(ds)){
    symbcol<-c(symb == ds[,i])
    if(TRUE %in% symbcol){
      vec<-c(vec,i)
    }
  }
  return(vec)
}
symb_finder(car_data,"?")
symb_finder(car_data,NA)
symb_finder(car_data,NaN)



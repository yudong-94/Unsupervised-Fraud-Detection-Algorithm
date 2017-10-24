load("ny_cleaned.rda")

library(ggplot2)
library(dplyr)

# Change lot area to numeric
ny_cleaned$LOT_AREA = as.double(ny_cleaned$LOT_AREA)

# creating temporary variable...preserving original dataset
ny_temp = ny_cleaned

str(ny_temp)

library(dataQualityR)

num.file <- paste(getwd(), "/dq_num.csv", sep= "")
cat.file <- paste(getwd(), "/dq_cat.csv", sep= "")
checkDataQuality(data= ny_cleaned, out.file.num= num.file, out.file.cat= cat.file)

num.file
cat.file

### HYGIENCE CHECKS
### change numerators containing value 1 to value 0
ny_temp = ny_cleaned

ny_temp$FULLVAL = ifelse(ny_temp$FULLVAL == 0,1, ny_temp$FULLVAL)
summary(ny_temp$FULLVAL)

ny_temp$AVLAND = ifelse(ny_temp$AVLAND == 0,1, ny_temp$AVLAND)
summary(ny_temp$AVLAND)

ny_temp$AVTOT = ifelse(ny_temp$AVTOT == 0,1, ny_temp$AVTOT)
summary(ny_temp$AVTOT)

ny_temp$EXLAND = ifelse(ny_temp$EXLAND == 0,1, ny_temp$EXLAND)
summary(ny_temp$EXLAND)

ny_temp$EXTOT = ifelse(ny_temp$EXTOT == 0,1, ny_temp$EXTOT)
summary(ny_temp$EXTOT)

ny_temp$BLD_VOLUME = ifelse(ny_temp$BLD_VOLUME == 0,1, ny_temp$BLD_VOLUME)
summary(ny_temp$BLD_VOLUME)

ny_temp$LOT_AREA = ifelse(ny_temp$LOT_AREA == 0,1, ny_temp$LOT_AREA)
summary(ny_temp$LOT_AREA)

ny_cleaned = ny_temp

### Creating new variables
### if the denominator is 0, then denominator <- 1
### FULLVAL/AVTOT
ny_cleaned$FV_AT = ifelse(ny_cleaned$AVTOT == 0, 
                         ny_cleaned$FULLVAL/1, 
                         ny_cleaned$FULLVAL/ny_cleaned$AVTOT)

sum(is.nan(ny_cleaned$FV_AT)) # All values are valid
summary(ny_cleaned$FV_AT)

ny_cleaned$FV_AT = ifelse(ny_cleaned$FV_AT == 0,1, ny_cleaned$FV_AT)
summary(ny_cleaned$FV_AT)


### AVTOT/EXTOT
summary(ny_cleaned$EXTOT)
ny_cleaned$AT_ET = ifelse(ny_cleaned$EXTOT == 0, 
                          ny_cleaned$AVTOT/1, 
                          ny_cleaned$AVTOT/ny_cleaned$EXTOT)

sum(is.nan(ny_cleaned$AT_ET)) # All values are valid
summary(ny_cleaned$AT_ET)
ny_cleaned$AT_ET = ifelse(ny_cleaned$AT_ET == 0,1, ny_cleaned$AT_ET)
summary(ny_cleaned$AT_ET)


### FULLVAL/EXTOT
ny_cleaned$FV_ET = ifelse(ny_cleaned$EXTOT == 0, 
                         ny_cleaned$FULLVAL/1, 
                         ny_cleaned$FULLVAL/ny_cleaned$EXTOT)

sum(is.nan(ny_cleaned$FV_ET)) # All values are valid

ny_cleaned$FV_ET = ifelse(ny_cleaned$FV_ET == 0,1, ny_cleaned$FV_ET)
summary(ny_cleaned$FV_ET)


### AVLAND/EXLAND
ny_cleaned$AL_EL = ifelse(ny_cleaned$EXLAND == 0, 
                          ny_cleaned$AVLAND/1, 
                          ny_cleaned$AVLAND/ny_cleaned$EXLAND)

sum(is.nan(ny_cleaned$AL_EL)) # All values are valid
ny_cleaned$AL_EL = ifelse(ny_cleaned$AL_EL == 0,1, ny_cleaned$AL_EL)
summary(ny_cleaned$AL_EL)


ny_temp = ny_cleaned


### Change the order
ny_temp = ny_temp[ , names(ny_temp)[c(1,7:13,2:6,14:17)]]
ny_cleaned = ny_temp


###  9 numerators & 7 denominators
numerator = c(names(ny_cleaned)[-1:-8])  # doing -1 for removing RECORD column
str(numerator)
denominator = c(names(ny_cleaned)[2:8])
str(denominator)


## HYGIENE CHECK .. no zeros/ no NaN's anywhere
j=1
names(ny_cleaned)

for (j in seq(names(ny_cleaned))){
  print(colnames(ny_cleaned[j]))
  print(sum(ny_cleaned[j] == 0))  
  print(sum(is.nan(ny_cleaned[,j])))
}


# CREATE VARIABLES FOR PCA
i=1
j=1

for (i in seq(numerator)) {
  for(j in seq(denominator)){
          
     name = paste0(numerator[i],"_",denominator[j])
     average = paste0("avg_",denominator[j])
     
# CREATES average variables     

       mutate_call = lazyeval::interp(~median(a), a = as.name(numerator[i]))
        ny_cleaned= ny_cleaned %>% 
              group_by_(.dots = denominator[j]) %>%
              mutate_(.dots = setNames(list(mutate_call), average))


#CREATES MAIN variables        
mutate_call = lazyeval::interp(~a/b, a = as.name(numerator[i]), b = as.name(average))
ny_cleaned = ny_cleaned %>% mutate_(.dots = setNames(list(mutate_call), name))      

    }
}

## V.V. IMP
ny_cleaned = as.data.frame(ny_cleaned)

## THE FINAL HYGIENE CHECK
j=1
names(ny_cleaned)

for (j in seq(names(ny_cleaned))){
  print(colnames(ny_cleaned[j]))
  print(sum(ny_cleaned[j] == 0))  
  print(sum(is.nan(ny_cleaned[,j])))
}


ny_cleaned$avg_BORO = NULL
ny_cleaned$avg_EASEMENT = NULL
ny_cleaned$avg_BLDGCL = NULL
ny_cleaned$avg_TAXCLASS = NULL
ny_cleaned$avg_ZIP = NULL
ny_cleaned$avg_LOT_AREA = NULL
ny_cleaned$avg_BLD_VOLUME = NULL


ny_subset_100000 = ny_cleaned[1:100000,]
save(ny_cleaned, file = "ny_ready.rda")
save(ny_subset_100000, file = "ny_ready_100000.rda")

### Plot distributions of first 95% data
ny_cleaned %>%
  filter(FULLVAL_BORO < quantile(ny_cleaned$FULLVAL_BORO, .95)) %>%
  ggplot(aes(FULLVAL_BORO)) +
  geom_histogram()


# NY_property data project

##############################################

library(stringr)
library(dplyr)
# library(HotDeckImputation)

#setwd("~/Desktop/DSO 562/Project1")
#load("ny_property.RDA")   #the Rdata of the original excel file
#load("ny_cleaned.RDA") 
ny_cleaned = ny_property

# extract the BORO code
ny_cleaned$BORO = str_extract(ny_cleaned$BBLE, "[0-9]")

# extract the 1st digit of BLDGCL
ny_cleaned$BLDGCL = str_extract(ny_cleaned$BLDGCL, "[A-Z]")

# add "NO" to all missing values in EASEMENT
ny_cleaned$EASEMENT = str_replace(ny_cleaned$EASEMENT, "^$", "NO")
## double-check: 
sum(ny_property$EASEMENT == "") == sum(ny_cleaned$EASEMENT == "NO")

# fill missing values in STORIES
# ## explore the relationship between stories and ZIP/BLDGCL/TAXCLASS
# zip_story = ny_cleaned %>%
#     filter(!is.na(STORIES)) %>%
#     group_by(ZIP) %>%
#     summarise(avg = mean(STORIES),
#              count = n())
# ## the average of stories in each ZIP ranging from 1 to 66
# bldgcl_story = ny_cleaned %>%
#     filter(!is.na(STORIES)) %>%
#     group_by(BLDGCL) %>%
#     summarise(avg = mean(STORIES),
#               count = n())
# # the average of stories in each bldgcl ranging from 1 to 36
# taxcl_story = ny_cleaned %>%
#     filter(!is.na(STORIES)) %>%
#     group_by(TAXCLASS) %>%
#     summarise(avg = mean(STORIES),
#               count = n())
# ## the average of stories in each tax level ranging from 1 to 16 (not very extreme)

# fill in missing values in STORIES with the average in its TAXCLASS
# ny_cleaned = mutate(ny_cleaned, STORIES = impute.mean(TAXCLASS))  ##dataset is too large, causing R crush
tax_story = ny_cleaned %>%
    filter(!is.na(STORIES)) %>%
    group_by(TAXCLASS) %>%
    summarise(avg = mean(STORIES))

for (i in seq(nrow(ny_cleaned))) {
    if (is.na(ny_cleaned[i, "STORIES"])) {
        tc = as.character(ny_cleaned[i, "TAXCLASS"])
        ny_cleaned[i, "STORIES"] = tax_story[tax_story$TAXCLASS == tc,]$avg
    }
    if (i %% 10000 == 0) {
        print(paste0(i," completed"))
    }
}
# take about 10 minutes to run the loop

# # check relationship between FULLVAL and EASEMENT
# ny_cleaned %>%
#     group_by(EASEMENT) %>%
#     summarise(avg = mean(FULLVAL),
#               count = n())
# ## some EASEMENT types do have some patterns: 
# ## All records in J, L, M, or U Easement type, have FULLVAL 0  (but in total 13 records only)

# ny_cleaned %>%
#     group_by(TAXCLASS) %>%
#     summarise(avg = mean(FULLVAL),
#               count = n())
## Buildings in TAXCLASS 1D (BUNGALOW COLONIES) has relatively higher full value on average
## Building in TAXCLASS 3 (UTILITIES (EXCEPT CEILING RR)) has relatively lower full value on average

# # check relationship between EXCD1 and EASEMENT
# ny_cleaned %>%
#     group_by(EXCD1, EASEMENT) %>%
#     summarise(count = n()) %>%
#     group_by(EASEMENT) %>%
#     summarise(types_of_EXCD1 = n())
# 
# ny_cleaned %>%
#     group_by(EASEMENT) %>%
#     summarise(count = n())
# ## Some EASEMENT types only have one type of EXCD1, but those EASEMENT types have only a few records

# fill in missing values in EXCD1 with "N"
ny_cleaned[is.na(ny_cleaned$EXCD1),]$EXCD1 = "N"
## double-check: 
sum(is.na(ny_property$EXCD1)) == sum(ny_cleaned$EXCD1 == "N")

# fill in missing values in ZIP with 00000
ny_cleaned$ZIP = as.character(ny_cleaned$ZIP)
ny_cleaned[is.na(ny_cleaned$ZIP),]$ZIP = "00000"
## double-check: 
sum(is.na(ny_property$ZIP)) == sum(ny_cleaned$ZIP == "00000")

# remove STADDR, OWNER, BBLE, BLOCK, LOT
ny_cleaned$STADDR = NULL
ny_cleaned$OWNER = NULL
ny_cleaned$BBLE = NULL
ny_cleaned$BLOCK = NULL
ny_cleaned$LOT = NULL

# remove AVLAND2, AVTOT2, EXLAND2, EXTOT2, EXCD2
ny_cleaned$AVLAND2 = NULL
ny_cleaned$AVTOT2 = NULL
ny_cleaned$EXLAND2 = NULL
ny_cleaned$EXTOT2 = NULL
ny_cleaned$EXCD2 = NULL

# remove EXCD1, EXMPTL
ny_cleaned$EXCD1 = NULL
ny_cleaned$EXMPTCL = NULL

# remove last 3 columns
ny_cleaned$PERIOD = NULL
ny_cleaned$YEAR = NULL
ny_cleaned$VALTYPE = NULL

# create LOT_AREA
ny_cleaned$LOT_AREA = ny_cleaned$LTFRONT * ny_cleaned$LTDEPTH
ny_cleaned$LOT_AREA = as.numeric(ny_cleaned$LOT_AREA)
ny_cleaned$LTFRONT = NULL
ny_cleaned$LTDEPTH = NULL

# create BLD_VOLUME
ny_cleaned$BLD_VOLUME = ny_cleaned$BLDFRONT * ny_cleaned$BLDDEPTH * ny_cleaned$STORIES
ny_cleaned$BLDFRONT = NULL
ny_cleaned$BLDDEPTH = NULL
ny_cleaned$STORIES = NULL

# adjust the column orders (reference number + numerators + denomorators )
ny_cleaned = ny_cleaned[,c(1,5:9,11,2:4,10,12:13)]

#save(ny_property, file = "ny_property.rda")
#save(ny_cleaned, file = "ny_cleaned.rda")


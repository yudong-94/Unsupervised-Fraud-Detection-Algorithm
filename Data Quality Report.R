library(ggplot2)
library(stringr)
library(dplyr)
setwd("~/Desktop/DSO 562/HW2")

load("NY property data.RData")


### DQR
summary(ny_property)


### BBLE

length(unique(ny_property$BBLE))

# BLOCK
length(unique(ny_property$BLOCK))
sum(is.na(ny_property$BLOCK))

ggplot(ny_property, aes(x = BLOCK)) +
    geom_histogram()

block_order = ny_property %>%
    mutate(BLOCK=as.character(BLOCK)) %>%
    group_by(BLOCK) %>%
    summarise(count = n(),
              percentage = count / nrow(ny_property)*100) %>%
    arrange(-count)

ggplot(block_order[1:10,], aes(x = reorder(BLOCK, -count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18)) +
    xlab("BLOCK")

# LOT
length(unique(ny_property$LOT))
sum(is.na(ny_property$LOT))

lot_unique = ny_property %>%
    mutate(boro_block = str_extract(BBLE, "(^[0-9]{6})")) %>%
    select(boro_block, LOT) %>%
    group_by(boro_block, LOT) %>%
    summarise(count = n())

lot_order = ny_property %>%
    mutate(LOT = as.factor(LOT)) %>%
    group_by(LOT) %>%
    summarise(count = n(),
              percentage = count / nrow(ny_property)*100) %>%
    arrange(-count)

ggplot(lot_order[1:10,], aes(x = reorder(LOT, -count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18)) +
    xlab("LOT")

# EASEMENT

data.frame(summary(ny_property$EASEMENT))
sum(is.na(ny_property$EASEMENT))

ny_property %>%
    group_by(EASEMENT) %>%
    summarise(count = n(), 
              percentage = 100 * count / nrow(ny_property)) %>%
    arrange(-count)

ny_property %>%
    group_by(EASEMENT) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = reorder(EASEMENT, -count), y = count)) +
    geom_bar(stat = "identity") + 
    xlab("EASEMENT") +
    ylab("Log Tranformed Count") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18)) +
    scale_y_log10()

# OWNER

length(unique(ny_property$OWNER))
sum(is.na(ny_property$OWNER))

owner_order = ny_property %>%
    group_by(OWNER) %>%
    summarise(count = n(),
              percentage = count / nrow(ny_property) * 100) %>%
    arrange(-count) 

ggplot(owner_order[1:10,], aes(x = reorder(OWNER, - count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("OWNER") +
    scale_y_log10()

# BLDGCL

length(unique(ny_property$BLDGCL))
summary(ny_property$BLDGCL)
sum(is.na(ny_property$BLDGCL))

bldgcl_order = ny_property %>%
    group_by(BLDGCL) %>%
    summarise(count = n(),
              percentage = count / nrow(ny_property)*100) %>%
    arrange(-count) 

ggplot(bldgcl_order[1:10,], aes(x = reorder(BLDGCL, - count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18)) +
    xlab("BLDGCL")

# TAXCLASS

length(unique(ny_property$TAXCLASS))
data.frame(summary(ny_property$TAXCLASS))
sum(is.na(ny_property$TAXCLASS))

tax_order = ny_property %>%
    group_by(TAXCLASS) %>%
    summarise(count = n(),
              percentage = count / nrow(ny_property)*100) %>%
    arrange(-count) 

ggplot(tax_order, aes(x = reorder(TAXCLASS, - count), y = count)) +
    geom_bar(stat = "identity") +
    xlab("TAXCLASS") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18)) +
    ylab("Log Transformed Count") +
    scale_y_log10()

# LTFRONT

length(unique(ny_property$LTFRONT))
summary(ny_property$LTFRONT)
summary(factor(ny_property$LTFRONT))[1:10]
sum(is.na(ny_property$LTFRONT))
sum(ny_property$LTFRONT == 0)

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

ny_property %>%
    filter(LTFRONT != 0) %>%
    summarise(min = min(LTFRONT),
              max = max(LTFRONT),
              median = median(LTFRONT),
              mean = mean(LTFRONT),
              mode = Mode(LTFRONT),
              sd = sd(LTFRONT))

summary(filter(ny_property, LTFRONT != 0)$LTFRONT)

ny_property %>%
    filter(LTFRONT != 0) %>%
ggplot(aes(x = LTFRONT)) +
    geom_histogram(bins = 20) +
    ylab("Log Transformed Count") +
    ggtitle("Distribution of LTFRONT Records Exluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18),
         plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_y_log10()

ny_property %>%
    filter(LTFRONT != 0) %>%
    ggplot(aes(x = LTFRONT)) +
    geom_histogram(bins = 20) +
    ylab("Count") +
    ggtitle("Distribution of Middle 50% LTFRONT Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    xlim(20, 40)
    
# LTDEPTH

length(unique(ny_property$LTDEPTH))
summary(ny_property$LTDEPTH)
summary(factor(ny_property$LTDEPTH))[1:10]
sum(is.na(ny_property$LTDEPTH))
sum(ny_property$LTDEPTH == 0)

ny_property %>%
    filter(LTDEPTH != 0) %>%
    summarise(min = min(LTDEPTH),
              max = max(LTDEPTH),
              median = median(LTDEPTH),
              mean = mean(LTDEPTH),
              mode = Mode(LTDEPTH),
              sd = sd(LTDEPTH))

summary(filter(ny_property, LTDEPTH != 0)$LTDEPTH)

ny_property %>%
    filter(LTDEPTH != 0) %>%
ggplot(aes(x = LTDEPTH)) +
    geom_histogram(bins = 20) +
    ylab("Log Transformed Count") +
    ggtitle("Distribution of LTDEPTH Records Excluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_y_log10()



ny_property %>%
    filter(LTDEPTH != 0) %>%
    ggplot(aes(x = LTDEPTH)) +
    geom_histogram(bins = 20) +
    ylab("Log Transformed Count") +
    ggtitle("Distribution of Middle 50% LTDEPTH Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_y_log10() +
    xlim(97, 100)

# STORIES

length(unique(ny_property$STORIES))
summary(ny_property$STORIES)
sum(is.na(ny_property$STORIES))

ny_property %>%
    filter(!is.na(STORIES)) %>%
    summarise(min = min(STORIES),
              max = max(STORIES),
              median = median(STORIES),
              mean = mean(STORIES),
              mode = Mode(STORIES),
              sd = sd(STORIES))

summary(filter(ny_property, STORIES != 0)$STORIES)

ggplot(ny_property, aes(x = STORIES)) +
    geom_histogram(bins = 20) +
    ylab("Log Transformed Count") +
    ggtitle("Distribution of All STORIES Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_y_log10()

ggplot(ny_property, aes(x = STORIES)) +
    geom_histogram(bins = 20) +
    ylab("Log Transformed Count") +
    ggtitle("Distribution of Middle 50% STORIES Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_y_log10() +
    xlim(2, 3)

# FULLVAL

length(unique(ny_property$FULLVAL))
summary(ny_property$FULLVAL)
summary(factor(ny_property$FULLVAL))[1:10]
sum(is.na(ny_property$FULLVAL))

ny_property %>%
    filter(FULLVAL!= 0) %>%
    summarise(min = min(FULLVAL),
              max = max(FULLVAL),
              median = median(FULLVAL),
              mean = mean(FULLVAL),
              mode = Mode(FULLVAL),
              sd = sd(FULLVAL))

summary(filter(ny_property, FULLVAL != 0)$FULLVAL)

ny_property %>%
    filter(FULLVAL!= 0) %>%
    ggplot(aes(x = FULLVAL)) +
        geom_histogram(bins = 100) +
        scale_y_log10() +
        ggtitle("Distribution of FULLVAL Records Excluding 0") +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              plot.title = element_text(size = 20, hjust = 0.5)) +
        ylab("Log Transformed Count") +
        coord_cartesian(xlim=c(0,1e+09)) 

ny_property %>%
    filter(FULLVAL!= 0) %>%
    ggplot(aes(x = FULLVAL)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% FULLVAL Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Count")
    

# AVLAND

length(unique(ny_property$AVLAND))
summary(ny_property$AVLAND)
summary(factor(ny_property$AVLAND))[1:10]
sum(is.na(ny_property$AVLAND))

ny_property %>%
    filter(AVLAND != 0) %>%
    summarise(min = min(AVLAND),
              max = max(AVLAND),
              median = median(AVLAND),
              mean = mean(AVLAND),
              mode = Mode(AVLAND),
              sd = sd(AVLAND))

summary(filter(ny_property, AVLAND != 0)$AVLAND)

ny_property %>%
    filter(AVLAND != 0) %>%
ggplot(aes(x = AVLAND)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of AVLAND Records Excluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,5e+08)) 

ny_property %>%
    filter(AVLAND != 0) %>%
    ggplot(aes(x = AVLAND)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% AVLAND") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Count") +
    xlim(9425, 19830)


# AVTOT

length(unique(ny_property$AVTOT))
summary(ny_property$AVTOT)
summary(factor(ny_property$AVTOT))[1:10]
sum(is.na(ny_property$AVTOT))

ny_property %>%
    filter(AVTOT != 0) %>%
    summarise(min = min(AVTOT),
              max = max(AVTOT),
              median = median(AVTOT),
              mean = mean(AVTOT),
              mode = Mode(AVTOT),
              sd = sd(AVTOT))

summary(filter(ny_property, AVTOT != 0)$AVTOT)

ny_property %>%
    filter(AVTOT != 0) %>%
ggplot(aes(x = AVTOT)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of AVTOT Records Excluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,1e+09)) 

ny_property %>%
    filter(AVTOT != 0) %>%
    ggplot(aes(x = AVTOT)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% AVTOT Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Count") +
    xlim(18660, 46880) +
    scale_y_log10()

# EXLAND

length(unique(ny_property$EXLAND))
summary(ny_property$EXLAND)
summary(factor(ny_property$EXLAND))[1:10]
sum(is.na(ny_property$EXLAND))

summary(filter(ny_property, EXLAND != 0)$EXLAND)

ny_property %>%
    filter(EXLAND != 0) %>%
    summarise(min = min(EXLAND),
              max = max(EXLAND),
              median = median(EXLAND),
              mean = mean(EXLAND),
              mode = Mode(EXLAND),
              sd = sd(EXLAND))

ggplot(ny_property, aes(x = EXLAND)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of EXLAND Records Excluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,5e+08)) 

ggplot(ny_property, aes(x = EXLAND)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% EXLAND Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    xlim(1620, 3240) +
    scale_y_log10()

sum(ny_property$AVLAND < ny_property$EXLAND)

# EXTOT

length(unique(ny_property$EXTOT))
summary(ny_property$EXTOT)
summary(factor(ny_property$EXTOT))[1:10]
sum(is.na(ny_property$EXTOT))

ny_property %>%
    filter(EXTOT != 0) %>%
    summarise(min = min(EXTOT),
              max = max(EXTOT),
              median = median(EXTOT),
              mean = mean(EXTOT),
              mode = Mode(EXTOT),
              sd = sd(EXTOT))

summary(filter(ny_property, EXTOT != 0)$EXTOT)

ggplot(ny_property, aes(x = EXTOT)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of EXTOT Records Excluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,1e+09)) 

ggplot(ny_property, aes(x = EXTOT)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% EXTOT Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    xlim(1620, 9786) +
    scale_y_log10()

sum(ny_property$AVTOT < ny_property$EXTOT)

# EXCD1

length(unique(ny_property$EXCD1))
summary(ny_property$EXCD1)
sum(is.na(ny_property$EXCD1))

EX = ny_property %>%
    filter(is.na(EXCD1)) %>%
    select(EXLAND, EXTOT, EXCD1)

excd_order = ny_property %>%
    group_by(EXCD1) %>%
    filter(!is.na(EXCD1)) %>%
    summarise(count = n(),
              percentage = 100 * count / nrow(ny_property)) %>%
    arrange(-count) 

ggplot(excd_order[1:10,], aes(x = reorder(EXCD1, - count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    xlab("EXCD1") +
    ylab("Log Transformed Count") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    scale_y_log10()


# STADDR

length(unique(ny_property$STADDR))
summary(ny_property$STADDR)
sum(is.na(ny_property$STADDR))

add_order = ny_property %>%
    group_by(STADDR) %>%
    filter(!is.na(STADDR)) %>%
    summarise(count = n()) %>%
    arrange(-count) 

ggplot(add_order[1:10,], aes(x = reorder(STADDR, - count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    xlab("STADDR") 


# ZIP

length(unique(ny_property$ZIP))
summary(ny_property$ZIP)
sum(is.na(ny_property$ZIP))

zip_order = ny_property %>%
    group_by(ZIP) %>%
    filter(!is.na(ZIP)) %>%
    summarise(count = n(),
              percentage = 100 * count / nrow(ny_property)) %>%
    arrange(-count) 

ggplot(zip_order[1:10,], aes(x = reorder(ZIP, - count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    xlab("ZIP") 


# EXMPTCL

length(unique(ny_property$EXMPTCL))
summary(ny_property$EXMPTCL)
sum(is.na(ny_property$EXMPTCL))

n = nrow(filter(ny_property, EXMPTCL != ""))

EXMPTCL_order = ny_property %>%
    group_by(EXMPTCL) %>%
    filter(EXMPTCL!= "") %>%
    summarise(count = n(),
              percentage = 100 * count / n) %>%
    arrange(-count) 

ggplot(EXMPTCL_order, aes(x = reorder(EXMPTCL, - count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    xlab("EXMPTCL") +
    scale_y_log10()


# BLDFRONT

length(unique(ny_property$BLDFRONT))
summary(ny_property$BLDFRONT)
summary(factor(ny_property$BLDFRONT))[1:10]
sum(is.na(ny_property$BLDFRONT))
sum(ny_property$BLDFRONT == 0)

ny_property %>%
    filter(BLDFRONT != 0) %>%
    summarise(min = min(BLDFRONT),
              max = max(BLDFRONT),
              median = median(BLDFRONT),
              mean = mean(BLDFRONT),
              mode = Mode(BLDFRONT),
              sd = sd(BLDFRONT))

summary(filter(ny_property, BLDFRONT != 0)$BLDFRONT)

ny_property %>%
    filter(BLDFRONT != 0) %>%
ggplot(aes(x = BLDFRONT)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of BLDFRONT Records Excluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10()

ny_property %>%
    filter(BLDFRONT != 0) %>%
    ggplot(aes(x = BLDFRONT)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% BLDFRONT Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    xlim(18, 25) +
    scale_y_log10()


# BLDDEPTH

length(unique(ny_property$BLDDEPTH))
summary(ny_property$BLDDEPTH)
summary(factor(ny_property$BLDDEPTH))[1:10]
sum(is.na(ny_property$BLDDEPTH))
sum(ny_property$BLDDEPTH == 0)

ny_property %>%
    filter(BLDDEPTH != 0) %>%
    summarise(min = min(BLDDEPTH),
              max = max(BLDDEPTH),
              median = median(BLDDEPTH),
              mean = mean(BLDDEPTH),
              mode = Mode(BLDDEPTH),
              sd = sd(BLDDEPTH))

summary(filter(ny_property, BLDDEPTH != 0)$BLDDEPTH)

ny_property %>%
    filter(BLDDEPTH != 0) %>%
ggplot(aes(x = BLDDEPTH)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of BLDDEPTH Records Excluding 0") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10()


ny_property %>%
    filter(BLDDEPTH != 0) %>%
    ggplot(aes(x = BLDDEPTH)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% BLDDEPTH Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Count") +
    xlim(35, 55)

# AVLAND2

length(unique(ny_property$AVLAND2))
summary(ny_property$AVLAND2)
summary(factor(ny_property$AVLAND2))[1:10]
sum(is.na(ny_property$AVLAND2))

avland = ny_property %>%
    select(AVLAND, AVLAND2) %>%
    filter(!is.na(AVLAND2))
sum(avland$AVLAND < avland$AVLAND2)

ny_property %>%
    filter(!is.na(AVLAND2)) %>%
    summarise(min = min(AVLAND2),
              max = max(AVLAND2),
              median = median(AVLAND2),
              mean = mean(AVLAND2),
              mode = Mode(AVLAND2),
              sd = sd(AVLAND2))

ggplot(ny_property, aes(x = AVLAND2)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of All AVLAND2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,5e+08))

ggplot(ny_property, aes(x = AVLAND2)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% AVLAND2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    xlim(5705, 62340)
    scale_y_log10()


# AVTOT2

length(unique(ny_property$AVTOT2))
summary(ny_property$AVTOT2)
summary(factor(ny_property$AVTOT2))[1:10]
sum(is.na(ny_property$AVTOT2))

avtot = ny_property %>%
    select(AVTOT, AVTOT2) %>%
    filter(!is.na(AVTOT2))
sum(avtot$AVTOT < avtot$AVTOT2)

ny_property %>%
    filter(!is.na(AVTOT2)) %>%
    summarise(min = min(AVTOT2),
              max = max(AVTOT2),
              median = median(AVTOT2),
              mean = mean(AVTOT2),
              mode = Mode(AVTOT2),
              sd = sd(AVTOT2))

ggplot(ny_property, aes(x = AVTOT2)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of All AVTOT2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,1e+09))

ggplot(ny_property, aes(x = AVTOT2)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% AVTOT2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Count") +
    xlim(34010, 240800) +
    scale_y_log10()

# EXLAND2

length(unique(ny_property$EXLAND2))
summary(ny_property$EXLAND2)
summary(factor(ny_property$EXLAND2))[1:10]
sum(is.na(ny_property$EXLAND2))

exland = ny_property %>%
    select(EXLAND, EXLAND2) %>%
    filter(!is.na(EXLAND2))
sum(exland$EXLAND < exland$EXLAND2)

ny_property %>%
    filter(!is.na(EXLAND2)) %>%
    summarise(min = min(EXLAND2),
              max = max(EXLAND2),
              median = median(EXLAND2),
              mean = mean(EXLAND2),
              mode = Mode(EXLAND2),
              sd = sd(EXLAND2))

ggplot(ny_property, aes(x = EXLAND2)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of All EXLAND2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,5e+08))

ggplot(ny_property, aes(x = EXLAND2)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% EXLAND2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    xlim(2090, 31420) +
    scale_y_log10()

# EXTOT2

length(unique(ny_property$EXTOT2))
summary(ny_property$EXTOT2)
summary(factor(ny_property$EXTOT2))[1:10]
sum(is.na(ny_property$EXTOT2))

extot = ny_property %>%
    select(EXTOT, EXTOT2) %>%
    filter(!is.na(EXTOT2))
sum(extot$EXTOT < extot$EXTOT2)

ny_property %>%
    filter(!is.na(EXTOT2)) %>%
    summarise(min = min(EXTOT2),
              max = max(EXTOT2),
              median = median(EXTOT2),
              mean = mean(EXTOT2),
              mode = Mode(EXTOT2),
              sd = sd(EXTOT2))

ggplot(ny_property, aes(x = EXTOT2)) +
    geom_histogram(bins = 100) +
    ggtitle("Distribution of All EXTOT2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10() +
    coord_cartesian(xlim=c(0,1e+09))

ggplot(ny_property, aes(x = EXTOT2)) +
    geom_histogram(bins = 20) +
    ggtitle("Distribution of Middle 50% EXTOT2 Records") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    xlim(2889, 106600) +
    scale_y_log10()

# EXCD2

length(unique(ny_property$EXCD2))
summary(ny_property$EXCD2)
sum(is.na(ny_property$EXCD2))

EX2 = ny_property %>%
    filter(is.na(EXCD2)) %>%
    select(EXLAND2, EXTOT2, EXCD2)

n = nrow(filter(ny_property, !is.na(EXCD2)))

excd_order = ny_property %>%
    group_by(EXCD2) %>%
    filter(!is.na(EXCD2)) %>%
    summarise(count = n(),
              percentage = count / n * 100) %>%
    arrange(-count) 

ggplot(excd_order[1:10,], aes(x = reorder(EXCD2, - count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    xlab("EXCD2") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5)) +
    ylab("Log Transformed Count") +
    scale_y_log10()


# PERIOD

summary(ny_property$PERIOD)

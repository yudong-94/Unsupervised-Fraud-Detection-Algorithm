### overlap in top 1%

library(dplyr)
library(ggplot2)
load("ny_property.rda")
load("ny_cleaned.rda")
load("overlap_1percent.rda")
load("overlap_top10.rda")
    

overlap_original = ny_property %>%
    filter(RECORD %in% overlap$RECORD)

# BORO
boro_all = ny_cleaned %>%
    group_by(BORO) %>%
    summarise(prop = n()/nrow(ny_cleaned))
boro_all = data.frame(boro_all, dataset = "all")
boro_overlap = overlap %>%
    group_by(BORO) %>%
    summarise(prop = n()/nrow(overlap))
boro_overlap = data.frame(boro_overlap, dataset = "high score")
boro_comp = rbind(boro_all, boro_overlap)

ggplot(data = boro_comp, aes(x = BORO, y = prop, fill = dataset)) +
    geom_bar(position = position_dodge(), 
             stat = "identity") +
    ylab("Proportion") +
    geom_text(aes(label = round(prop,2)), 
              hjust = 0.5, vjust = 0, size = 5,
              position = position_dodge(width=1)) +
    scale_fill_manual(values=c("black", "grey")) +
    ggtitle("BORO Distribution Comparison") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
    

# TAXCLASS
tax_all = ny_cleaned %>%
    group_by(TAXCLASS) %>%
    summarise(prop = n()/nrow(ny_cleaned))
tax_all = data.frame(tax_all, dataset = "all")
tax_overlap = overlap %>%
    group_by(TAXCLASS) %>%
    summarise(prop = n()/nrow(overlap))
tax_overlap = data.frame(tax_overlap, dataset = "highscore")
tax_comp = rbind(tax_all, tax_overlap)

ggplot(data = tax_comp, aes(x = TAXCLASS, y = prop, fill = dataset)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    ylab("Proportion") +
    geom_text(aes(label = round(prop,2)), 
              hjust = 0.5, vjust = 0, size = 5,
              position = position_dodge(width=1)) +
    scale_fill_manual(values=c("black", "grey")) +
    ggtitle("TAXCLASS Distribution Comparison") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))

# BLDGCL
bld_all = ny_cleaned %>%
    group_by(BLDGCL) %>%
    summarise(prop = n()/nrow(ny_cleaned))
bld_all = data.frame(bld_all, dataset = "all")
bld_overlap = overlap %>%
    group_by(BLDGCL) %>%
    summarise(prop = n()/nrow(overlap))
bld_overlap = data.frame(bld_overlap, dataset = "high score")
bld_comp = rbind(bld_all, bld_overlap)

ggplot(data = bld_comp, aes(x = BLDGCL, y = prop, fill = dataset)) +
    geom_bar(position = position_dodge(width = 0.5), stat = "identity") +
    ylab("Proportion") +
    geom_text(aes(label = round(prop,2)), 
              hjust = 0, vjust = 0.5, size = 5,
              position = position_dodge(width=1)) +
    scale_fill_manual(values=c("black", "grey")) +
    ggtitle("BLDGCL Distribution Comparison") +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)) +
    coord_flip()

# FULLVAL
median(ny_cleaned$FULLVAL)
median(overlap$FULLVAL)

# NET VALUE
ny_cleaned$NET_VALUE = ny_cleaned$AVTOT - ny_cleaned$EXTOT
overlap$NET_VALUE = overlap$AVTOT - overlap$EXTOT
sum(ny_cleaned$NET_VALUE == 0)/nrow(ny_cleaned)
sum(overlap$NET_VALUE == 0)/nrow(overlap)

# LOT_AREA
sum(ny_cleaned$LOT_AREA == 0)/nrow(ny_cleaned)
sum(overlap$LOT_AREA == 0)/nrow(overlap)

# BLD_VOLUME
sum(ny_cleaned$BLD_VOLUME == 0)/nrow(ny_cleaned)
sum(overlap$BLD_VOLUME == 0)/nrow(overlap)

# AVTOT/BLDVOLUME
median(ny_cleaned$AVTOT / ny_cleaned$BLD_VOLUME)
overlap_top10$AVTOT / overlap_top10$BLD_VOLUME

# AVTOT/LOT_AREA
median(ny_cleaned$AVTOT / ny_cleaned$LOT_AREA)
overlap_top10$AVTOT / overlap_top10$LOT_AREA
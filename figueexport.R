## Exporting graphs script!

#Load packages if need be
library(gridExtra)

# generic print
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "chla_v_lt_bymonth.png", units = "in", width = 8, height = 6, res=300)
grid.arrange(lt_22_plot, chla_lt_22_plot, ncol=1)
dev.off()


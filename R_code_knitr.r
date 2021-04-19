# creiamo un report utilizzando un codice già scritto

setwd("C:/lab/")
library(knitr)
library(tinytex)
# tinytex::install_tinytex()
    
tinytex::tlmgr_update()

stitch("R_code_Greenland.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))

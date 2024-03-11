here::i_am("data-raw/sdtm/prepareData.R")
library(here)
library(usethis)

# Copy csv files in /data-raw to /data and create roxygen documentation in /R
paths<-list.files(
    here::here("data-raw","sdtm"),
    pattern = "\\.xpt$",
    ignore.case = TRUE,
    full.names = FALSE,
    recursive = TRUE
)

for(path in paths){
    # remove file extension
    file <- paste0(
        "sdtm_",
        substr(path, 1, nchar(path)-4)
    )

    # Read xpt to data frame
    assign(
        file,
        haven::read_xpt(here('data-raw','sdtm',path))
    )

    #save to .RDA to /data
    do.call(
        'use_data',
        list(as.name(file), overwrite = TRUE)
    )
}

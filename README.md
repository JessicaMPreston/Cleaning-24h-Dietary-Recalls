Clean 24h Recalls
This project takes the data collected from the field of 24h dietary recalls, 
and transforms it into a format appropriate for downstream work.

# Brief description of folder and file contents

The following folders contain:
-   `data-raw/`: tb24Hour and 24HR Recall FINAL CRF
-   `data/`: metadata, all recalls, combined recall
-   `docs/`: quarto file
-   `R/`: Targets pipeline, Functions

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the
`Clean 24h Recalls.Rproj` file and running this command in the console:

```         
# install.packages("pak")
pak::pak()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.

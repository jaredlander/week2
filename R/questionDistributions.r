## @knitr riceDist
ricePerc <- build.dist(data=pak, lhs="New_ID", group="Province", question="RiceLost")
ricePerc$Size <- "All"

## @knitr waterDist
water <- build.dist(data=pak, lhs="New_ID", group="Province", question="StagnantWater")
water$Size <- "All"

## @knitr accomodationDist
accommodation <- build.dist(data=pak, lhs="New_ID", group="Province", question="Accommodation")
accommodation$Size <- "All"
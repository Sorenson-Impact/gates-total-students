library(tidycensus)
library(siverse)
library(sorensonimpact)
library(tidyverse)


thing <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B15001_006", "B15001_007", "B15001_008", 
                                             "B15001_009", "B15001_010"), 
          state = x)
})
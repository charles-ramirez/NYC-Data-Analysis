library(readxl)
RAW <- readxl::read_xlsx("~/Downloads/sf1_dp_cncld_demoprofile.xlsx", 
                         col_names=FALSE, 
                         .name_repair="minimal")
colnames(RAW) <- paste0("V", seq(NCOL(RAW)))

# From Visual inspection, the file is divided into 52 chunks
# (51 districts + totals) with the same structure, so let's split
# the raw data into 52 components and process them separately

RAW %>% mutate(
  admin_unit = case_when(
    is.na(V1) ~ NA_character_, 
    str_detect(V1, "SF1-DP") ~ NA_character_,
    str_detect(V1, "Council District") ~ V1, 
    str_detect(V1, "New York City") ~ "Total")
) %>%
  tidyr::fill(admin_unit) %>% 
  filter(!is.na(admin_unit)) %>%
  group_by(admin_unit) %>%
  group_split -> SPLIT

TBL <- SPLIT[[1]]

## Now we have a _list_ each element of which is a data frame
## with the same structure. From here, we write a function
## to process each unit separately and we'll then recombine

process_district <- function(TBL){
  TBL %>% 
    ## We know we don't need G, I, J, or K
    ## since that info is redundant with the raw populations
    ## so we can drop it
    select(-V7, -V9, -V10, -V11) %>%
    ## Next, we can drop the first few rows since they don't really 
    ## add anything for this analysis
    tail(-3) %>%
    ## From here, let's drop the blank rows, which we can identify
    ## as those without a number in the V6 column (Y2000 counts)
    filter(!is.na(V6)) %>%
    ## Now that we have dropped all labels, 
    ## we can convert the two numeric columns to proper numbers
    mutate(Y2000 = as.double(V6), 
           Y2010 = as.double(V8)) %>%
    select(-V6, -V8) %>%
    ## Let's 'fill forward' the first column since tells us
    ## what section of the table we're in
    tidyr::fill(V1) %>% 
    ## Next, let's the variously indented columns into a single 
    ## usable column
    tidyr::unite(field, V2:V5, na.rm=TRUE) %>%
    ## And drop the sub-total rows since they're still redundant
    ## but we have to be careful here to keep "Persons Per Family/Household"
    filter((str_length(field) != 0) | (str_detect(V1, "Persons Per"))) %>%
    ## Finally, let's break this up by something we can use
    mutate(variable = case_when(
      V1 == "Total Population" ~ "Population", 
      V1 == "Total Households" ~ "Households",
      V1 == "Occupied Housing Units" ~ "Housing Units", 
      TRUE ~ V1
    )) %>%
    # Next, let's clarify what the age field does for the housing
    # units part of the data
    mutate(field = ifelse((variable == "Housing Units") & str_detect(field, "years"), 
                           paste("Householder Age -", field), 
                           field)) %>%
    # Finally, let's rearrange columns into a more natural order
    select(admin_unit, variable, field, Y2000, Y2010)
}

DATA <- SPLIT %>% lapply(process_district) %>% bind_rows

DATA <- DATA %>% 
  filter(admin_unit != "Total") %>%
  mutate(district = as.integer(str_replace(admin_unit, "Council District", ""))) %>%
  select(-admin_unit)

readr::write_csv(DATA, "nyc_demos.csv")
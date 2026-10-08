
## Functions for the OROV dataset 

# 1. Data dictionary functions -------------------------------------------
# the function in this section are not needed, used dataMaid to generate codebook

# function to extract site data from the CRF
site_data_extract <- function(file) {
  df <- read_xlsx(file,
                  sheet = 2, range = "B3:C16", col_names = c("cols", "var"))
  df_clean <- df %>% 
    filter(!is.na(cols)) %>% #removes the blank spaces in the excel 
    pivot_wider(id_cols = everything(), # switch to a wide format
                names_from = cols, 
                values_from=var) 
  
  
  return(df_clean)
} 

# function to clean problematic dates 
parse_mixed_date <- function(x){
  
  # remove whitespace
  x <- trimws(x)
  
  # detect numeric strings (Excel serials)
  is_num <- grepl("^[0-9]+$", x)
  
  out <- as.Date(rep(NA_real_, length(x)), origin = "1899-12-30")
  
  # convert Excel serials
  out[is_num] <- as.Date(as.numeric(x[is_num]), origin = "1899-12-30")
  
  # parse real dates
  out[!is_num] <- suppressWarnings(
    as.Date(lubridate::parse_date_time(
      x[!is_num],
      orders = c("ymd","dmy","mdy")
    ))
  )
  
  return(out)
}



# function to extract column names from the CRF
extract_colnames <- function(file){
  col_key <- read_xlsx(file,  
                       sheet = 3, n_max = 3) 
  
  # rename columns with the code
  colnames(col_key) <- col_key[3,]
  
  colnames(col_key) <-  make.unique(colnames(col_key))  
  v_col_key <- colnames(col_key)
  return(v_col_key)
}

# function to extract the data code (type of data)
extract_codes <- function(file, v_columns){
  code_key <- read_xlsx(file,  
                       sheet = 3, n_max = 3) 

  df_code <- code_key[2:3,]
  colnames(df_code) <- v_columns  
  df_linker <-  df_code %>%
    select(-matches("\\bNA\\b")) 
  return(df_linker)
}

# function to create data linker (needed for the dictionary building)
extract_linker <- function(file, v_colnames){
  df<- read_xlsx(file,  
                       skip =1, sheet = 3, n_max = 3, col_names = v_colnames) 
  

  
  df_linker <-  df %>%
    select(-matches("\\bNA\\b")) %>% # remove blank columns wiht NA in their names
    rename(p_id = CODE)
  

  var_desc <- df_linker %>%
    slice(1) %>%
    unlist(.,use.names = F) # remove the column names

  var_type <- df_linker %>%
    mutate(across(everything(), ~ case_when(
      # Exclude columns matching 'temperature' or '_lab', then check value match
      !str_detect(cur_column(), "(?i)temperature|_lab") & 
        . %in% c("dropdown", "freetext", "free text", "Free text") ~ 1,
      
      TRUE ~ 0 # All other cases
    ))) %>%
    slice(2) %>%
    unlist(use.names = FALSE)

  linker <- build_linker(df_linker, var_desc, var_type)
  
  linker_clean <- linker %>% # ensuring that each variable has a unique description
    mutate(var_desc=
             case_when(str_detect(var_name, "co_inf") ~ paste("coinfection", var_desc),
                       str_detect(var_name, "blood_") ~ paste("blood test", var_desc),
                       str_detect(var_name, "urine_") ~ paste("urine test", var_desc),
                       str_detect(var_name, "semen_") ~ paste("semen test", var_desc),
                       str_detect(var_name, "serum_") ~ paste("serum test", var_desc),
                       str_detect(var_name, "csf_") ~ paste("CSF test", var_desc),
                       str_detect(var_name, "ethnicity") ~ paste("ethnicity", var_desc),
                       str_detect(var_name, "education") ~ paste("education", var_desc),
                       T ~ var_desc
             )) %>%
    mutate(var_desc=
             case_when(
               str_detect(var_name, "_fu") ~ paste(var_desc, "during follow up"),
               T ~ var_desc)) # has to be a different mutate call or else R only executes the first true case so does not rename the variable when it contains both fu and blood, etc.
  if(length(unique(linker_clean$var_desc)) != length(linker_clean$var_desc)) {
    print("Error: some descriptions are not unique")
  }
  

  return(linker_clean)
  
}

# extract data from the main CRF

extract_dataset <- function(file, v_colnames){
  # extract column types from column names 
  v_col_read <- case_when(v_colnames %in% "age" | grepl("_ct|_lab", v_colnames) ~ "numeric",
                          #grepl("_date", v_colnames) ~ "date", # This would be to directly turn all dates into characters but the problem is that dates are not read properly by the dataMeta function 
                          grepl("_date", v_colnames) ~ "text",
                           grepl("\\bNA\\b", v_colnames) ~ "skip",
                          T~"guess")
  
  # introduce a check in the function
  if(sum(v_col_read=="skip") == 10){ # there should 10 blank columns
    
    # there should be 10 blank columns
    sum(v_col_read=="skip") == 10
    
    
    
    df <- read_xlsx(file, 
                    sheet = 3, skip = 3, 
                    na = c("", "NA"), col_types = v_col_read) #make sure all NAs are coded as such. It will introduce errors in dates but we can fix this in post-processing
    # use the column names without the NA columns 
    colnames(df) <- v_colnames[!grepl("\\bNA\\b", v_colnames)]
    
    df_clean <- df %>%
      rename(p_id = CODE) %>%
      mutate(p_id = sapply(str_extract_all(p_id, "\\d+"), paste, collapse = " ")) # this is to ensure that the p_id data will be read as a range
    
    date_cols <- grep("_date", names(df_clean), value = TRUE)
    
    df_clean <- df_clean %>%
      mutate(across(all_of(date_cols), parse_mixed_date))
    return(df_clean)
    
  } else {
    print("Error: wrong columns skipped")
  }
  
  
} 

# extract the column names of the dates
get_date_cols <- function(df) {
  # logical vector: TRUE if column is Date/POSIXct or name ends with "date"/"Date"
  is_date <- sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
  ends_with_date <- grepl("date$|end$", names(df), ignore.case = TRUE)
  
  # return column names
  names(df)[is_date | ends_with_date]
}




# extract dictionary, uses the previous functions 

extract_dict <- function(df, linker) {

 

# v_dates <-  df[ , sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))] %>% # across all columns, check whether x is a date or POSIXct object
#   colnames()
  
v_dates<- get_date_cols(df)

df_dict <- df %>%
  mutate(
    across(all_of(v_dates), as.numeric), # transform the dates to numerical factors so that the build_dict function works
    p_id = as.numeric(p_id)
  )


# Now build the dictionary
dict <- build_dict(df_dict, linker, 
                   option_description = NULL, 
                   prompt_varopts = FALSE)


# extract min and max of the date variables into a matrix 

# Initialize variables
date_min <- vector("character", length(v_dates))
date_max <- vector("character", length(v_dates)) 
v_range <- vector("character", length(v_dates))
m_dates <- matrix(ncol = 2, nrow = length(v_dates))
colnames(m_dates) <- c("var_name", "range")

# Loop to calculate min, max, and range for each date column
for (i in 1:length(v_dates)) {
  v_coldates <- v_dates[i]
  
  # Convert the column to Date format if it's character, and handle errors
  tryCatch({
    df[[v_coldates]] <- as.Date(df[[v_coldates]], format = "%Y-%m-%d")  # Adjust format if needed
  }, error = function(e) {
    message(paste("Error in converting column", v_coldates, "to Date:", e))
  })
  
  # Calculate min and max, checking for infinite values
  min_val <- min(df[[v_coldates]], na.rm = TRUE)
  max_val <- max(df[[v_coldates]], na.rm = TRUE)
  
  # If min or max is infinite, replace with NA
  if (!is.finite(min_val)) {
    date_min[i] <- NA
  } else {
    date_min[i] <- as.character(min_val)
  }
  
  if (!is.finite(max_val)) {
    date_max[i] <- NA
  } else {
    date_max[i] <- as.character(max_val)
  }
  
  # Create range string
  v_range[i] <- paste(date_min[i], "to", date_max[i], sep = " ")
}

# Optional: Assign to the matrix
m_dates[, 1] <- v_dates  # Store the variable names in the first column
m_dates[, 2] <- v_range    # Store the ranges in the second column

# View result (optional)
df_dates<- data.frame(m_dates)

# # replace the variable_option columns of dates only if variable_name is the same as var_name in our dataframe of dates
dict$variable_options <- ifelse(
  dict$variable_name %in% df_dates$var_name,
  df_dates$range[match(dict$variable_name, df_dates$var_name)],
  dict$variable_options
)
return(dict)
}




# Function to capitalize the first letter only
capitalize_first <- function(x) {
  str_to_title(str_to_lower(str_trim(x)))  # Lowercase then capitalize first letter
}



# function to safely bind datasets
safe_bind_rows <- function(...) {
  dfs <- list(...)
  
  # Get all column names
  all_cols <- unique(unlist(lapply(dfs, names)))
  
  # Standardize columns: add missing ones
  dfs_fixed <- lapply(dfs, function(df) {
    for (col in all_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA
      }
    }
    df <- df[, all_cols]
    df
  })
  
  # Align types across datasets
  for (col in all_cols) {
    # Find first non-NA column to use as reference type
    ref_class <- NULL
    for (df in dfs_fixed) {
      if (!all(is.na(df[[col]]))) {
        ref_class <- class(df[[col]])[1]
        break
      }
    }
    if (is.null(ref_class)) ref_class <- "character"  # fallback
    
    dfs_fixed <- lapply(dfs_fixed, function(df) {
      if (!inherits(df[[col]], ref_class)) {
        # Handle Date
        if (ref_class == "Date") {
          df[[col]] <- as.Date(df[[col]], origin = "1970-01-01")
        }
        # Handle POSIXct
        else if (ref_class == "POSIXct") {
          df[[col]] <- as.POSIXct(df[[col]], origin = "1970-01-01", tz = "UTC")
        }
        # Handle numeric/integer
        else if (ref_class %in% c("numeric", "integer")) {
          df[[col]] <- as.numeric(df[[col]])
        }
        # Otherwise, coerce to character
        else {
          df[[col]] <- as.character(df[[col]])
        }
      }
      df
    })
  }
  
  bind_rows(dfs_fixed)
}


# 2. Descriptive analysis functions ---------------------------------------
# function to pass onto repair name, to be used with read_excel()
my_custom_name_repair <- function(nms) tolower(gsub(" ", "_", nms))

# function to extract lab values 
extract_lab_values <- function(df, v_lab_cat){
  l_lab <- list()
  
  for(lab in v_lab_cat) {
    idx <- !is.na(df[[lab]]) & grepl("[A-Za-z]", df[[lab]])
    
    v_lab <- df[idx, lab]
    names(v_lab) <- df[idx, "p_id"]
    
    df[[lab]] <- NA
    l_lab[[lab]] <- v_lab 
    l_lab[["idx"]] <- idx
  }
  
  return(l_lab)
}



extract_missing <- function(df, l_attributes) {
  var_missing <- df %>% 
    select(where(~ sum(is.na(.)) == nrow(df))) %>% #sums up the number of NA and if it is same as total rows, they are all missing
    names() # extract the names of the variables 
  
  # extract the original variable name from the dictionary
  
  dict<- l_attributes$dict

  l_missing <- list()
  for(i in seq_along(var_missing)){
    # check if var_missing[i] exists in dict$variable_name
    if(var_missing[i] %in% dict$variable_name){
      l_missing[[i]] <- dict[dict$variable_name == var_missing[i], "variable_description"]
    } else {
      l_missing[[i]] <- "fetal outcomes"  # new name
    }
  }
  
  cleaned_list <- str_replace_all(l_missing, "\\s+", " ")  # replaces all whitespace (tabs, newlines, etc.) with single space

  return(cleaned_list)
}



# function to check the lab values range by sex
range_check <- function(value, df1, df2) {
  
  # extract the required columns from lab
  cols<- df2[, names(df2) %in% c(value, "sex")]
  
  #extract range from normal_values_adults 
  range <- df1[df1$variable_name==value,"range"]
  
  # If range is NA, use sex-stratified range instead
  if (is.na(range)) {
    range_f <- df1[df1$variable_name == value, "range_f"]
    range_m <- df1[df1$variable_name == value, "range_m"]
    
    bounds_f <- as.numeric(str_split(range_f, "[-–]", simplify = TRUE))
    bounds_m <- as.numeric(str_split(range_m, "[-–]", simplify = TRUE))
    
    # Row-wise classification using mapply
    result <- mapply(function(val, sex) {
      if (is.na(val)) return(NA)
      if (sex == "F") {
        if (val > bounds_f[2]) return("above")
        if (val < bounds_f[1]) return("below")
      } else if (sex == "M") {
        if (val > bounds_m[2]) return("above")
        if (val < bounds_m[1]) return("below")
      }
      return("within")
    }, cols[[value]], cols[["sex"]])
    
    return(as.factor(result))
  } else {
    
    # Extract numeric bounds from range
    bounds <- as.numeric(str_split(range, "[-–]", simplify = TRUE))
    
    # check if values inside vector are above, below or within the normal range
    result <- as.factor(case_when(
      is.na(cols[[value]]) ~ NA,
      cols[[value]] >bounds[2] ~ "above",
      cols[[value]] < bounds[1]~"below", 
      T ~ "within"))
    
  }
  
  # Return combined range string
  result
}

# function to multiply both parts of a range by the same factor
multiply_ranges <- function(r1, r2) {
  # Ensure inputs are matrices (or vectors)
  r1 <- as.matrix(r1)
  r2 <- as.matrix(r2)
  
  # Check dimensions match
  if(!all(dim(r1) == dim(r2))) stop("Dimensions of r1 and r2 must match")
  
  # Element-wise multiplication
  result <- matrix(NA, nrow = nrow(r1), ncol = ncol(r1))
  
  for(i in 1:nrow(r1)) {
    for(j in 1:ncol(r1)) {
      # Extract numeric bounds from each range
      nums1 <- as.numeric(str_split(r1[i,j], "[-–]", simplify = TRUE))
      nums2 <- as.numeric(str_split(r2[i,j], "[-–]", simplify = TRUE))
      
      # Multiply lower and upper bounds
      lower <- nums1[1] * nums2[1]
      upper <- nums1[2] * nums2[2]
      
      # Store combined range
      result[i,j] <- paste0(lower, "-", upper)
    }
  }
  
  return(result)
}



# function to create a clean table from lab values tab
clean_tab <- function(tab) {
  colname <- names(tab)[1]
  
  # extract unit from parentheses in first column
  unit <- str_extract(colname, "\\((.*?)\\)") %>% 
    str_remove_all("[()]")
  
  tab_clean <- tab %>%
    slice(-1)  # remove header row
  
  n <- ncol(tab_clean)
  
  # Always split first column into Age and Male
  first_col <- names(tab_clean)[1]
  
  tab_clean <- tab_clean %>%
    # collapse "≥ 18 years", "≤ 14 days", etc.
    mutate(!!first_col := str_replace(!!sym(first_col), "([≥≤])\\s*([0-9]+\\s*(?:d|days|months|years))", "\\1\\2")) %>%
    separate(!!sym(first_col), into = c("Age", "Male", "Female"),
             sep = "\\s+(?=\\d|<|≥|≤)", extra = "merge", fill = "right")
  
  # Rename remaining columns to keep original names
  if (n > 1) {
    tab_clean$Female <- tab_clean$...2
    tab_clean <- tab_clean[,-4]
  }
  
  # Add unit column
  tab_clean <- tab_clean %>%
    mutate(unit = unit)
  
  return(tab_clean)
}





# create a clean summary table from the gt_summary package, with all our customization for the manuscript (earlier version)
# must be set at the beginning of the script in option()
create_summary_table <- function(data, by_var, v_strat, type_list = NULL, stat_list = NULL, var_yes = NULL) {
  
  exclude_var <- setdiff(v_strat, c({{ by_var }}))
  
  include_vars <- setdiff(colnames(data), exclude_var)
  
  nc_values <- c( # values for each column when all are missing
    stat_1 = "487",
    stat_2 = "47",
    stat_3 = "1,289",
    stat_4 = "439",
    stat_5 = "231",
    stat_6 = "61",
    stat_7 = "56",
    stat_8 = "106"
  )
  
 
  #this function transforms missing value to an actual label with always the last position 
  data <- data %>%
    mutate(across(where(~ is.character(.) | is.factor(.)),
                  ~ {
                    # Save original levels if it's already a factor
                    orig_levels <- if (is.factor(.)) levels(.) else NULL
                    
                    # Convert to character to safely handle NAs
                    x <- as.character(.)
                    
                    if (any(is.na(x))) {
                      x[is.na(x)] <- "Missing"
                      
                      # If original levels exist, append "Missing" at the end
                      if (!is.null(orig_levels)) {
                        factor(x, levels = c(orig_levels, "Missing"))
                      } else {
                        # Preserve the order of appearance for character vars
                        factor(x, levels = c(unique(x[x != "Missing"]), "Missing"))
                      }
                    } else {
                      # If no NAs, just restore original order if available
                      if (!is.null(orig_levels)) {
                        factor(x, levels = orig_levels)
                      } else {
                        factor(x)
                      }
                    }
                  }))
  
  
  
  
  
  tbl <- data %>%
    tbl_summary(
      by = {{ by_var }},
      include = all_of(include_vars),
      type = type_list,
      statistic = stat_list,
      missing = "ifany",        # include rows for missing values
      missing_text = "Missing",  # label for missing values
      digits = all_categorical() ~ c(0, 1) # only adds digits to %
    ) %>%
    add_overall() |>
    add_n() |> # add n available data column
    remove_row_type(variables = var_yes ,type = "level", level_value = "No") |> # remove rows that are not needed
    remove_row_type(variables = var_yes ,type = "level", level_value = "Not applicable") |>
    remove_row_type(variables = var_yes ,type = "level", level_value = "Negative") |> 
    remove_row_type(variables = var_yes ,type = "level", level_value = "Not done") |>
    modify_table_body( # copy info of "Yes" value directly in the variable label row
      ~ .x %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(
          dplyr::across(
            starts_with("stat_"),
            ~ ifelse(
              row_type == "label" & any(row_type == "level" & label %in% c("Yes","Positive")),
              .[row_type == "level" & label %in% c("Yes","Positive")],
              .
            )
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!(row_type == "level" & label %in% c("Yes","Positive"))) # remove Yes row
    ) |>
    modify_table_body( # replace impossible value with "-"
      ~ .x %>%
        mutate(
          across(
            starts_with("stat_"),
            ~ ifelse(. %in% c("0 (NA%)", "NA (NA, NA)", "0" , "Inf days, -Inf days"), "—", .)
          )
        )
    )|>
    modify_table_body( # add "nc" in the label row for columns in which data is 100% missing (variable not collected)
      ~ .x %>%
        group_by(variable) %>%
        mutate(
          across(
            .cols = -stat_0 & starts_with("stat_"),
            .fns = ~ {

              # detect pattern in Missing row
              col_name <- cur_column()
              flag_nc <- any(
                label == "Missing" &
                  stringr::str_detect(
                    .,
                    paste0("\\b", stringr::fixed(nc_values[[col_name]]), "\\b")
                  )
              )

              # replace label row if detected
              ifelse(
                row_type == "label" & flag_nc,
                "nc",
                .
              )
            }
          )
        ) %>%
        ungroup()
    )  |>
    modify_table_body(
      ~ .x %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(
          missing_n = dplyr::case_when(
            label == "Missing" ~ stringr::str_extract(stat_0, "^[0-9,]+")
          ),
          missing_n = as.numeric(gsub(",", "", missing_n)),
          missing_n = ifelse(all(is.na(missing_n)), 0, missing_n[!is.na(missing_n)][1]),
          # convert n to numeric
          n = as.numeric(gsub(",", "",
                              n)),
          n = dplyr::if_else(
            var_type %in% c("categorical", "dichotomous") & row_type == "label",
            n - missing_n,
            n
          ),
          n = format(n, big.mark = ",", scientific = FALSE) # add back the comma for >3 digits
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-missing_n)
    )|>
  remove_row_type(type = "level", level_value = "Missing") |> # remove missing row
  remove_row_type(type = "missing")

  
  return(tbl)

}


##function to create summary table with missing data row
# create a clean summary table from the gt_summary package, with all our customization for the manuscript
# must be set at the beginning of the script in option()
create_summary_table_miss <- function(data, by_var, v_strat, type_list = NULL, stat_list = NULL, var_yes = NULL,
                                      set_na_option = "ifany") {
  
  exclude_var <- setdiff(v_strat, c({{ by_var }}))
  
  include_vars <- setdiff(colnames(data), exclude_var)
  
  nc_values <- c( # values for each column when all are missing
    stat_1 = "487",
    stat_2 = "47",
    stat_3 = "1,289",
    stat_4 = "439",
    stat_5 = "231",
    stat_6 = "61",
    stat_7 = "56",
    stat_8 = "106"
  )
  
  
  #this function transforms missing value to an actual label with always the last position 
  data <- data %>%
    mutate(across(where(~ is.character(.) | is.factor(.)),
                  ~ {
                    # Save original levels if it's already a factor
                    orig_levels <- if (is.factor(.)) levels(.) else NULL
                    
                    # Convert to character to safely handle NAs
                    x <- as.character(.)
                    
                    if (any(is.na(x))) {
                      x[is.na(x)] <- "Unknown"
                      
                      # If original levels exist, append "Unknown" at the end
                      if (!is.null(orig_levels)) {
                        factor(x, levels = c(orig_levels, "Unknown"))
                      } else {
                        # Preserve the order of appearance for character vars
                        factor(x, levels = c(unique(x[x != "Unknown"]), "Unknown"))
                      }
                    } else {
                      # If no NAs, just restore original order if available
                      if (!is.null(orig_levels)) {
                        factor(x, levels = orig_levels)
                      } else {
                        factor(x)
                      }
                    }
                  }))
  
  
  
  
  
  tbl <- data %>%
    tbl_summary(
      by = {{ by_var }},
      include = all_of(include_vars),
      type = type_list,
      statistic = stat_list,
      missing = set_na_option,        # decide what rows to include for missing value
      missing_text = "Unknown",  # label for missing values
      digits = list(all_categorical() ~ c(0, 1),
                    # Overrides formatting for variables starting with "sero_" (1 decimal places for %)
                    starts_with("sero_") ~ c(0, 0, 1), # only adds digits to %
      ends_with("_outcomes") ~ c(0, 0, 1))
    ) %>%
    add_overall() |>
    #add_n(missing=T) |> # add n available data column
    remove_row_type(variables = var_yes ,type = "level", level_value = "No") |> # remove rows that are not needed
    remove_row_type(variables = var_yes ,type = "level", level_value = "Not applicable") |>
    remove_row_type(variables = var_yes ,type = "level", level_value = "Negative") |> 
    remove_row_type(variables = var_yes ,type = "level", level_value = "Not done") |>
    modify_table_body( # copy info of "Yes" value directly in the variable label row
      ~ .x %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(
          dplyr::across(
            starts_with("stat_"),
            ~ ifelse(
              row_type == "label" & any(row_type == "level" & label %in% c("Yes","Positive")),
              .[row_type == "level" & label %in% c("Yes","Positive")],
              .
            )
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!(row_type == "level" & label %in% c("Yes","Positive"))) # remove Yes row
    ) |>
    modify_table_body( # replace impossible value with "-"
      ~ .x %>%
        mutate(
          across(
            starts_with("stat_"),
            ~ ifelse(. %in% c("0 (NA%)", "NA (NA, NA)", "0" , "Inf days, -Inf days", "0/0 (NA%)"), "—", .)
          )
        )
    )|>
    modify_table_body(
      ~ .x %>%
        group_by(variable) %>%
        mutate(
          across(
            .cols = -stat_0 & starts_with("stat_"),
            .fns = ~ {
              col_name <- cur_column()
              target_nc <- nc_values[[col_name]]
              
              # Extract ONLY the leading missing count 'n' from the string
              missing_val <- .[label %in% c("Missing", "Unknown")]
              missing_n <- stringr::str_extract(missing_val, "^[0-9,]+")
              
              # True ONLY if the extracted leading missing count equals the column total
              flag_nc <- length(missing_n) > 0 && 
                !is.na(missing_n) && 
                missing_n == target_nc
              
              # Replace label row if 100% missing
              ifelse(
                row_type == "label" & flag_nc,
                "nc",
                .
              )
            }
          )
        ) %>%
        ungroup()
    )
  
  return(tbl)
  
}


## create summary table for lab tab, where the denominator shows up as n/N in the cell 
create_summary_table_lab <- function(data, by_var, v_strat, type_list = NULL, stat_list = NULL, var_yes = NULL) {
  
  exclude_var <- setdiff(v_strat, c({{ by_var }}))
  
  include_vars <- setdiff(colnames(data), exclude_var)
  
  nc_values <- c( # values for each column when all are missing
    stat_1 = "487",
    stat_2 = "47",
    stat_3 = "1,289",
    stat_4 = "439",
    stat_5 = "231",
    stat_6 = "61",
    stat_7 = "56",
    stat_8 = "106"
  )
  
  
  
  
  tbl <- data %>%
    tbl_summary(
      by = {{ by_var }},
      include = all_of(include_vars),
      type = type_list,
      statistic = stat_list,
      missing = "no",        # we do not want missing values as we already include n/N
      #missing_text = "Missing",  # label for missing values
      digits = all_categorical() ~ c(0,0, 1) # only adds digits to % (p is the third value in the list)
    ) %>%
    add_overall() |>
    #add_n() |> # add n available data column
    remove_row_type(variables = var_yes ,type = "level", level_value = "No") |> # remove rows that are not needed
    remove_row_type(variables = var_yes ,type = "level", level_value = "Not applicable") |>
    remove_row_type(variables = var_yes ,type = "level", level_value = "Negative") |> 
    remove_row_type(variables = var_yes ,type = "level", level_value = "Not done") |>
    modify_table_body( # copy info of "Yes" value directly in the variable label row
      ~ .x %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(
          dplyr::across(
            starts_with("stat_"),
            ~ ifelse(
              row_type == "label" & any(row_type == "level" & label %in% c("Yes","Positive")),
              .[row_type == "level" & label %in% c("Yes","Positive")],
              .
            )
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!(row_type == "level" & label %in% c("Yes","Positive"))) # remove Yes row
    ) |>
    modify_table_body( # replace impossible value with "-"
      ~ .x %>%
        mutate(
          across(
            starts_with("stat_"),
            ~ ifelse(. %in% c("0 (NA%)", "NA (NA, NA)", "0" , "Inf days, -Inf days", "0/0 (NA%)"), "—", .)
          )
        )
    )|>
    modify_table_body( # add "nc" in the label row for columns in which data is 100% missing (variable not collected)
      ~ .x %>%
        group_by(variable) %>%
        mutate(
          across(
            .cols = -stat_0 & starts_with("stat_"),
            .fns = ~ {
              
              # detect pattern in Missing row
              col_name <- cur_column()
              flag_nc <- any(
                label == "Missing" &
                  stringr::str_detect(
                    .,
                    paste0("\\b", stringr::fixed(nc_values[[col_name]]), "\\b")
                  )
              )
              
              # replace label row if detected
              ifelse(
                row_type == "label" & flag_nc,
                "nc",
                .
              )
            }
          )
        ) %>%
        ungroup()
    )  
  
  return(tbl)
  
}




# 3. Plots ----------------------------------------------------------------
# create a map from shapefiles 
map_continent <- function(data, 
                          section, 
                          fill_var, 
                          fill_scale = scale_fill_distiller(palette = "YlOrRd",
                                                            guide = guide_legend(order = 1)),
                          set_xmin = -170, 
                          set_xmax = 0,
                          set_ymin = -60,  
                          set_ymax =  85,
                          legend = "Reported OROV cases 2024-2025",
                          show_adm1 = TRUE){
  
  americas_bbox <- st_bbox(c( # to keep only the Americas landmass and zoom into LATAM
    xmin = set_xmin, xmax = set_xmax,
    ymin = set_ymin,  ymax =  set_ymax
  ), crs = st_crs(data))
  
  
  plot <- data |>
    st_collection_extract("POLYGON") |>
    st_crop(americas_bbox) |>
    ggplot()+
    geom_sf(aes(fill = .data[[fill_var]]),
            color = NA)
  
  if(show_adm1){
    plot <- plot+
      geom_sf(data = df_shapefiles |> 
                st_collection_extract("POLYGON") |>
                st_crop(americas_bbox) |>
                filter(shapeType == "ADM1"), fill = NA,
              color = "lightgrey"
      )
  }
  plot <- plot +
    geom_sf(data = df_shapefiles |> 
              st_collection_extract("POLYGON") |>
              st_crop(americas_bbox) |>
              filter(shapeType != "ADM1"), fill = NA,
            color = "grey50",
            size = 0.5
    )+
    fill_scale+
    coord_sf(expand = TRUE) + # ensure that the bounds include all objects added to the map
    theme_void()+
    labs(fill = legend)+
    theme(legend.title = element_text(face = "bold"),
          legend.position = "bottom")
  
  return(plot)
}

## apply layout customization to the map 
apply_theme <- function(p, 
                        offset_tab = df_bubble_offsets,
                        label_data = df_map,
                        set_size_lab = 2.5,
                        set_size_legend = 8
){
  p + 
    theme(
      legend.text = element_text(size = set_size_legend),
      legend.title = element_text(size = set_size_legend),
      legend.key.size = unit(0.4, "cm"),
      legend.position = "left"
    )+
    #add sites
    geom_segment(data = label_data |>
                   left_join(offset_tab, by = "shapeName") |>
                   mutate(x    = st_centroid(geometry) %>% st_coordinates(.) %>% .[,1],
                          y    = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2],
                          xend = x_pos,
                          yend = y + y_offset),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 0.4, color = "grey40")+
    geom_point(data =label_data |>
                 mutate(x = st_centroid(geometry) %>% st_coordinates(.) %>% .[,1],
                        y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2]),
               aes(x=x, y = y),
               shape = 21, stroke = 0.5, size = 1, color ="#08306B", fill="white")+
    geom_text(data = label_data |>
                left_join(offset_tab, by = "shapeName") |>
                mutate(x = x_pos + label_x_offset,
                       y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset + label_y_offset),
              aes(x = x, y = y, label = label, hjust = hjust),
              size = set_size_lab, fontface = "italic")
} 


# function to make a simple barplot
make_barplot <- function(data, 
                         vars, 
                         replacement_vector = NULL,
                         names_to_col = "symptom",
                         col_fill = "lightblue",
                         set_alpha = 0.7,
                         set_width = 0.75
                         ){
 
   names_sym <- sym(names_to_col)  # convert string to symbol for tidy evaluation
  
  # Prepare data
  df <- data %>%
        select(all_of(vars)) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to  = names_to_col,
      values_to = "response"
    ) %>%
    # Apply replacement_vector if provided
    mutate(!!names_sym := if (!is.null(replacement_vector)) {
      case_when(
        .data[[names_to_col]] %in% names(replacement_vector) ~ replacement_vector[.data[[names_to_col]]],
        TRUE ~ .data[[names_to_col]]
      )
    } else {
      .data[[names_to_col]]
    }) %>%
    group_by(!!names_sym) %>%
    summarise(
      count_yes = sum(response == "Yes", na.rm = TRUE),
      count_total = n(),                # <-- includes NAs
      .groups   = "drop"
    ) %>%
    mutate(
      pct_yes = (count_yes / count_total) * 100   # denominator includes NAs
    ) %>%
    # Total Yes per symptom for ordering
    group_by(!!names_sym) %>%
    mutate(
      total_yes = sum(count_yes)
    ) %>%
    ungroup() %>%
    arrange((total_yes)) %>%
    mutate(!!names_sym := factor(.data[[names_to_col]], levels = unique(.data[[names_to_col]])))
  
  # Plot
  ggplot(df, aes(x = .data[[names_to_col]], y = pct_yes)) +
    geom_bar(stat = "identity", 
             fill  = scales::alpha(col_fill, set_alpha),  # fill alpha
             color = scales::alpha(col_fill, set_alpha + 0.2),    # border alpha darker, 
             width = set_width) +
    ylab("Percentage of patients") +
    coord_flip() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8))+
        scale_y_continuous(limits = c(0, 102), expand = c(0, 0)) # get rid of the white space under the bar
}


#function to make a grouped barplot
grouped_barplot <-  function(data, 
                             xvar, 
                             strat_var, 
                             set_width=0.7,
                             set_alpha = 0.7,
                             xlab = NULL, 
                             v_replacement = replacement_vector,
                             v_palette){

    x_sym  <- sym(xvar) # creating symbols to be able to mutate them within the function
    strat_sym <- sym(strat_var)
    legend_string <- unname(v_replacement[strat_var])
    # Save original factor levels for the legend
    original_levels <- levels(data[[strat_var]])
    
    df <- data %>%
     filter(!is.na(!!strat_sym)) %>%
      group_by(!!x_sym, !!strat_sym,  .drop = FALSE) %>% # keep the levels 
      summarise(n = n(), .groups = "drop_last") %>%
      mutate(pct_group = n / sum(n) * 100) %>%
      ungroup()  %>%
    group_by(!!x_sym) %>%
    mutate(total_prop = sum(pct_group)) %>%
    ungroup() |>
      mutate(
      !!strat_sym := fct_rev(factor(.data[[strat_var]], levels = original_levels))
    )
    
    
    plot <- ggplot(
      df,
      aes(
        x = .data[[xvar]],
        y = pct_group,
        fill = .data[[strat_var]],
        color = .data[[strat_var]]
      )
    ) +
      geom_bar(stat = "identity", position = "dodge", width = set_width, alpha = set_alpha) +
      labs(y = "Percentage of patients", x = xlab, fill= legend_string, color = legend_string)+
      scale_fill_manual(
        values = v_palette,
        breaks = original_levels,
        drop = FALSE
      ) +
      scale_color_manual(
        values = alpha(v_palette, set_alpha + 0.2),
        breaks = original_levels,
        drop = FALSE
      )+
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(2.5, "mm"),
        legend.spacing.y = unit(1, "mm")
      )+
      scale_y_continuous(limits = c(0, 102), expand = c(0, 0))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
      coord_flip()

    
    return(plot)
}


# function to make a faceted and grouped barplot
grouped_barplot_facet <- function(data, vars, strat_var = "site", facet_var = NULL, replacement_vector = NULL, names_to_col = "symptom", set_width = 0.75) {
  
  names_sym <- sym(names_to_col)  # convert string to symbol for tidy evaluation
  strat_sym <- sym(strat_var)
  facet_sym <- sym(facet_var)
  # Prepare data
  df <- data %>%
    filter(!is.na(.data[[strat_var]])) |> # ensure that there are no NAs in the stratifying variable
    add_count(.data[[strat_var]], name = "strat_var_n") |>
    mutate({{ strat_var }} := paste0(.data[[strat_var]], " (n=", strat_var_n, ")"))|> # add the n of each strata in the legend 
    select(all_of(vars), all_of(strat_var),
           if (!is.null(facet_var)) all_of(facet_var)) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to  = names_to_col,
      values_to = "response"
    ) %>%
    # Apply replacement_vector if provided
    mutate(!!names_sym := if (!is.null(replacement_vector)) {
      case_when(
        .data[[names_to_col]] %in% names(replacement_vector) ~ replacement_vector[.data[[names_to_col]]],
        TRUE ~ .data[[names_to_col]]
      )
    } else {
      .data[[names_to_col]]
    }) %>%
    group_by(!!names_sym, .data[[strat_var]], !!facet_sym) %>%
    summarise(
      count_yes = sum(response == "Yes", na.rm = TRUE),
      count_total = n(),                # <-- includes NAs
      .groups   = "drop"
    ) %>%
    mutate(
      pct_yes = (count_yes / count_total) * 100   # denominator includes NAs
    ) %>%
    # Total Yes per symptom for ordering
    group_by(!!names_sym) %>%
    mutate(
      total_yes = sum(count_yes)
    ) %>%
    ungroup() %>%
    arrange(total_yes) %>%
    mutate(
      !!names_sym := factor(
        .data[[names_to_col]],
        levels = unique(.data[[names_to_col]])
      ),
      !!strat_sym := factor(
        .data[[strat_var]],
        levels = rev(unique(.data[[strat_var]])) # reverse the order in which the bars appear
      )
    )

  p <- ggplot(df, aes(x = .data[[names_to_col]],
                      y = pct_yes,
                      fill = .data[[strat_var]])) +
    geom_bar(stat = "identity", position = "dodge", width = set_width) +
    ylab("Percentage of patients") +
    ylim(0, 100) +
    coord_flip() +
    theme_classic() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8)
    )+ 
    guides(fill = guide_legend(reverse = TRUE, 
                               nrow = 2,
                               byrow = TRUE)
                  )
                 # re-reverse the order of the legend only (need to do this because coord are flipped)

  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }

  p

}




# function to make a faceted barplot
barplot_facet <- function(data, 
                          vars, 
                          strat_var = "site", 
                          replacement_vector = NULL, 
                          names_to_col = "symptom", 
                          set_width = 0.75,
                          set_facet_rows=NULL,
                          set_facet_cols = NULL) {
  
  names_sym <- sym(names_to_col)  # convert string to symbol for tidy evaluation
  
  # Prepare data
  df <- data %>%
    filter(!is.na(.data[[strat_var]])) |> # ensure that there are no NAs in the stratifying variable
    add_count(.data[[strat_var]], name = "strat_var_n") |>
    mutate(
      {{ strat_var }} := forcats::fct_relabel(
        .data[[strat_var]],
        ~ paste0(.x, " (n=", strat_var_n[match(.x, .data[[strat_var]])], ")")
      )
    )|> # add the n of each strata in the legend and keep the levels 
    select(all_of(vars), all_of(strat_var)) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to  = names_to_col,
      values_to = "response"
    ) %>%
    # Apply replacement_vector if provided
    mutate(!!names_sym := if (!is.null(replacement_vector)) {
      case_when(
        .data[[names_to_col]] %in% names(replacement_vector) ~ replacement_vector[.data[[names_to_col]]],
        TRUE ~ .data[[names_to_col]]
      )
    } else {
      .data[[names_to_col]]
    }) %>%
    group_by(!!names_sym, .data[[strat_var]]) %>%
    summarise(
      count_yes = sum(response == "Yes", na.rm = TRUE),
      count_total = n(),                # <-- includes NAs
      .groups   = "drop"
    ) %>%
    mutate(
      pct_yes = (count_yes / count_total) * 100   # denominator includes NAs
    ) %>%
    # Total Yes per symptom for ordering
    group_by(!!names_sym) %>%
    mutate(
      total_yes = sum(count_yes)
    ) %>%
    ungroup() %>%
    arrange(desc(total_yes)) %>%
    mutate(
      !!names_sym := forcats::fct_reorder(
        .data[[names_to_col]],
        total_yes,
        .fun = sum,
        .desc = TRUE
      )) |>
    mutate( # keeps the order
      {{ strat_var }} := forcats::fct_relevel(
        .data[[strat_var]],
        levels(.data[[strat_var]])
      )
    )
  
  
  
  # Plot
  ggplot(df, aes(x = .data[[strat_var]], 
                 y = pct_yes, 
                 fill = .data[[strat_var]], 
                 colour = .data[[strat_var]])) +
    geom_col(alpha = 0.7, width = set_width) + # one "bar" per x-axis instead of multiple ones
    facet_wrap(~ .data[[names_to_col]], 
               nrow = set_facet_rows,
               ncol = set_facet_cols,
               strip.position = "bottom") +
    labs(x = NULL, y = "Percentage of patients") +
    ylim(0,100)+
    theme_classic() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 8),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.key.size = unit(2.5, "mm"),
      legend.spacing.y = unit(1, "mm")
      
    )+
    scale_y_continuous(limits = c(0, 102), expand = c(0, 0)) # get rid of the white space under the bar
}



# function to make a histogram faceted or non_faceted
make_hist1 <- function(dat, x, fill, xlab, facet =NULL, colors, set_width = 0.75, set_break =c(10,12), v_break=NULL) {
  # convert bare column names to quosures
  x_var     <- enquo(x)
  fill_var  <- enquo(fill)
  facet_var <- enquo(facet)
  
  if (quo_is_null(facet_var)){
    dat <- dat |>
      filter(!is.na(.data[[x_var]])) |>
      add_count(!!fill_var, name = "fill_var_n") |>
      mutate(
        !!fill_var := forcats::fct_relabel(
          !!fill_var,
          ~ paste0(.x, " (n=", fill_var_n[match(.x, !!fill_var)], ")")
        ))
    
    ggplot(dat, aes(x = !!x_var, fill = !!fill_var, color = !!fill_var)) +
      geom_histogram(
        position = "identity", binwidth = set_width, alpha = 0.7) +
      scale_x_continuous(limits = c(0, NA), expand = c(0, 0))+ # get rid of the white space under the bar
      scale_y_break(set_break, scales = 0.5) +
      xlab(xlab) +
      ylab("Number of patients") +
      labs(fill = "", color = "") +
     theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(2.5, "mm"),
        legend.spacing.y = unit(1, "mm")) +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0), breaks = v_break) # get rid of the white space under the bar

  } else { 
  
  dat <- dat |>
    filter(!is.na(.data[[x_var]])&!is.na(.data[[facet_var]]))
  
  ggplot(dat, aes(x = !!x_var, fill = !!fill_var)) +
    geom_histogram(
                   position = "identity", binwidth = 0.75, alpha = 0.7) +
    xlab(xlab) +
    ylab("Number of patients") +
    labs(fill = "") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10)
    ) +
    facet_wrap(vars(!!facet_var))+
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0))+ # get rid of the white space under the bar
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0))
}
}

# function to make a non-faceted histogram
make_hist2 <- function(dat, x, fill, xlab, set_width = 0.75, set_ylim=100, set_xlim=NA, set_alpha=0.7) {
  # convert bare column names to quosures
  x_var     <- enquo(x)
  fill_var  <- enquo(fill)
  
    dat <- dat |>
      filter(!is.na(.data[[x_var]])) |>
      add_count(!!fill_var, name = "fill_var_n") |>
      mutate(
        !!fill_var := forcats::fct_relabel(
          !!fill_var,
          ~ paste0(.x, " (n=", fill_var_n[match(.x, !!fill_var)], ")")
        ))
    
    ggplot(dat, aes(x = !!x_var, fill = !!fill_var, color = !!fill_var)) +
      geom_histogram(
        position = "identity", binwidth = set_width, alpha = set_alpha) +
      xlab(xlab) +
      ylab("Number of patients") +
      labs(fill = "", color = "") +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(2.5, "mm"),
        legend.spacing.y = unit(1, "mm")
     )+
    scale_y_continuous(limits = c(0, set_ylim), expand = c(0, 0))+ # get rid of the white space under the bar
    scale_x_continuous(limits = c(0, set_xlim), expand = c(0, 0)) # get rid of the white space under the bar
}

#function to make an histogram of timediff data
make_hist3<- function(data = data,
                      set_x,
                      set_color,
                      set_v_colors = v_green_blue,
                      set_width = 1,
                      set_xlab = "Time since symptom onset",
                      set_ylab = "Number of patients",
                      set_ylim_min = 0,
                      set_ylim_max = NA,
                      set_breaks = waiver()){    
  ggplot(data = data , aes(x =.data[[set_x]], fill = .data[[set_color]], color = .data[[set_color]])) +
    geom_histogram(
      position = "identity", binwidth = set_width, alpha = 0.7) +
    scale_x_continuous(
      labels = function(x) ifelse(x == 0, "<24h", paste(x, "days")),
      expand = c(0, 0)
    )+
    xlab(set_xlab) +
    ylab(set_ylab) +
    labs(fill = "", color = "") +
    scale_fill_manual(values = unname(set_v_colors))+
    scale_color_manual(values = unname(set_v_colors))+
    theme_classic() +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(2.5, "mm"),
      legend.spacing.y = unit(1, "mm")) +
    scale_y_continuous(breaks = set_breaks,limits = c(0, NA), expand = c(0, 0))+
    coord_cartesian(ylim = c(set_ylim_min, set_ylim_max)) # this allows to cut the graph 
  
}

# fucntion to create a point range graph of Ct values
point.range <-  function(data,set_x, set_y, set_ymin, set_ymax,
                         set_color = "site2",
                         set_v_color = v_green_blue,
                         set_ylab = "Ct values",
                         set_xlab = "Time since symptoms onset",
                         set_legend = "",
                         set_size ="site2"){
  plot  <- data %>% 
    ggplot(aes(x=.data[[set_x]], y=.data[[set_y]], color = .data[[set_color]],  size=.data[[set_size]]))+ 
    geom_pointrange(aes(
      ymin= .data[[set_ymin]],
      ymax = .data[[set_ymax]]),
      alpha=0.5, # transparency
      stroke = 1, # width of the border
      linewidth=0.75,
      position=position_dodge(width = 0.5) # move points away from each other horizontally
    )+ 
    labs(y = set_ylab, x=set_xlab, color = set_legend)+
    theme_classic()+
    scale_color_manual(values = set_v_color)+
    scale_size_manual(values = c("Espirito Santo" = 2, "Ceará" = 1.5, "Amazonas" = 1,
                                                      "Rio de Janeiro" = 0.5, "Darién" = 0.5),
                         guide = "none"
                       )+
    scale_y_continuous(limits = c(0, max(data[[set_ymax]], na.rm = TRUE)), expand = c(0, 0))+
    scale_x_continuous(
      breaks = 0:15,
      labels = function(x) ifelse(x == 0, "<24h", paste0(x, "d"))
    )+theme(
      axis.text   = element_text(size = 10),
      axis.title  = element_text(size = 10),
      legend.text = element_text(size = 6),
      legend.key.size = unit(1.25, "mm"),
      legend.spacing.y = unit(0.5, "mm"),
      legend.position = "bottom"
    )+
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = F
      )
    )
  return(plot)
}


# 4. Manuscript functions -------------------------------------------------
# function to turn a summary table to a customized flextable for the manuscript
style_table <- function(tbl_summary_obj, v_replacement) {
  
  # rename the sections based on replacement vector
  tab <- tbl_summary_obj %>%
    modify_table_body(
      ~ .x %>%
        mutate(label = case_when(
          #label %in% names(v_replacement) & !is.na(n) ~ paste0(v_replacement[variable], " (n=", n ,")"), # paste the amount of missing values
          label %in% names(v_replacement)  ~ v_replacement[variable],
          T ~label))
    ) |>
   # modify_column_hide(n) |>
    modify_footnote_header(
      footnote = "Data are in median (IQR) or n (%), unless specified otherwise. nc indicates variables that were not collected at specific sites.",
      columns = all_stat_cols(),
      replace = TRUE
    )
  
  # convert to flextable first
  ft <- tab %>% 
    as_flex_table()
  
  # extract the body data to find matching rows ,remove this as we will do it manually
  # ft_data <- ft$body$dataset
  # idx <- which(ft_data$label %in% v_replacement)
  
  # remove the word characteristics 
  ft$header$content$data[[1]]$txt <- ""
  
  ft %>%
    fontsize(size = 8, part = "all") %>%
    flextable::font(fontname = "Times", part = "all") %>%
    theme_box() %>%
    bg(bg = "#EAEAEA", part = "header") %>%
    width(j = 1, width = 1.7) %>%
    #bold(i = idx, j = "label", part = "body") %>%
    #bg(i = idx, bg = "#DDDDDD", part = "body") %>%
    align(align = "left", part = "all") %>%
    set_table_properties(width = 1, layout = "autofit")
}


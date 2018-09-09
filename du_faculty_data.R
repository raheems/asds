
# Sun Sep  9 10:09:02 2018 ------------------------------


fpath = "/Users/enayetraheem/Dropbox/MyBooks/AppliedStatDataScience/"
fname = "data-resource_2018_08_06_Detail Information of Teacher_DU_Working.xlsx"

f = paste(fpath, fname, sep = "")
f

df <- readxl::read_excel(f, sheet = 1, range = "B8:Y30")
dim(df)

for (i in 2:133){
  new_df = readxl::read_excel(f, sheet = i, range = "B8:Y30")
  df = rbind(df, new_df)
}

dim(df)


# Remove all columns with NA
df <- df %>%
  discard(~all(is.na(.x))) %>%
  map_df(~.x)

# Drop X__6
df <- df %>% select(-X__6)

# Remove all rows with all NAs
df <- df[!apply(is.na(df), 1, all),]


# Add  new column to indicate year
df$year = 2017
dim(df)

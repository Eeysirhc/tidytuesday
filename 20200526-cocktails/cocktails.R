
# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2020-05-25

# TIDYTUESDAY: COCKTAILS (WEEK 2020-05-26)
# SOURCE: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-26/readme.md


# LOAD PACKAGES
library(tidyverse)
library(FactoMineR)
library(ggrepel)


# DOWNLOAD DATA
bc_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv') 


# DATA PROCESSING

## STANDARDIZE CASES
bc <- bc_raw %>% 
  mutate(ingredient = str_to_lower(ingredient)) %>% 
  distinct() %>% 
  select(name, ingredient)

## FIX UNTIDY DATA

### CLEANED DATAFRAME
bc_tidy <- bc %>% 
  filter(!str_detect(ingredient, ","))

### EXTRACT UNTIDY DATA THEN CLEAN
bc_untidy <- bc %>% 
  filter(str_detect(ingredient, ",")) %>% 
  mutate(ingredient = str_split(ingredient, ", ")) %>% 
  unnest(ingredient)

### COMBINE BOTH DATAFRAMES
bc_clean <- rbind(bc_tidy, bc_untidy) %>% 
  distinct()

## REDUCE CARDINALITY
n_ingredients <- bc_clean %>% 
  count(ingredient, sort = TRUE) %>% 
  filter(n > 10)

## NORMALIZE INGREDIENTS
df <- bc_clean %>% 
  inner_join(n_ingredients) %>% 
  select(-n) %>% 
  mutate(ingredient = str_replace_all(ingredient, "-", "_"),
         ingredient = str_replace_all(ingredient, " ", "_"),
         ingredient = str_replace_all(ingredient, "old_mr._boston_", ""),
         ingredient = str_replace_all(ingredient, "old_thompson_", "")) 



# MULTIPLE CORRESPONDENCE ANALYSIS (MCA)
## FORMAT DATA FOR MCA
df_mca_processed <- df %>% 
  mutate(value = 1) %>%
  pivot_wider(names_from = ingredient) %>% 
  replace(is.na(.), 0) %>% 
  select(-name) %>%
  mutate_if(is.double, as.factor)

mca_results <- MCA(df_mca_processed, graph = FALSE)


## SHAPE DATA TO TIDY STRUCTURE
mca_df <- data.frame(mca_results$var$coord)


mca_final <- rownames_to_column(mca_df, var = "rowname") %>% 
  as_tibble() %>% 
  filter(str_detect(rowname, "_1")) %>% 
  mutate(variable = str_replace_all(rowname, "_1", "")) %>% 
  select(variable, everything(), -rowname) %>% 
  mutate(highlight = case_when(str_detect(variable, "gin") ~ "gin",
                               str_detect(variable, "rum") ~ "rum",
                               str_detect(variable, "vodka") ~ "vodka",
                               str_detect(variable, "whiskey") ~ "whiskey",
                               str_detect(variable, "brandy") ~ "brandy",
                               str_detect(variable, "bourbon") ~ "bourbon",
                               str_detect(variable, "tequila") ~ "tequila"))


# FINAL PLOT
mca_final %>% 
  ggplot(aes(x = Dim.1, y = Dim.2, label = variable, color = highlight)) +
  geom_density2d(color = "gray90") +
  geom_point(show.legend = FALSE) +
  geom_text_repel(show.legend = FALSE) +
  labs(x = "D1", y = "D2", 
       title = "Multiple correspondence analysis (MCA) on the most common cocktail ingredients",
       subtitle = "Closer points suggest they are typically mixed together",
       caption  = "by: @eeysirhc\nsource: Mr. Boston Bartender's Guide") +
  theme_minimal(base_size = 15) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


library(tidyverse)
library(vroom)
library(lsr)
library(corrr)
library(ggcorrplot)
library(tidygraph)
library(ggraph)
library(sf)
library(reshape2)
library(corrplot)
options(scipen = 999)

# 1. Correlation Testing --------------------------------------------------

## Read in the inputs
db <- vroom("Output Data/3. Postcode Modelling/PCD_inputs_PROP.tsv")

## Remove geographies, adn remove ts055 vars as not interesting 
corrInputs <- db %>%
  select(-c(PCDS, OA21CD)) %>%
  select(-c(130:131))

## Convert variable names to numbers 
colnames(corrInputs) <- seq(ncol(corrInputs))

# 1.1. Correlation Matrix -------------------------------------------------

## Get data format sorted for correlation matrix

## Convert to matrix
sub_c <- round(cor(corrInputs), 2)
sub_p <- Hmisc::rcorr(as.matrix(corrInputs), type = "pearson")
sub_po <- sub_p$P

## COmpute matrix of p values
p_mat <- cor_pmat(corrInputs)

## Reshape to long format - each row is a correlation coefficient/p value
m <- melt(sub_c)
o <- melt(sub_po)

## Bring together 
df <- merge(m, o, by = c("Var1", "Var2"))
df <- df %>%
  setNames(c("Var1", "Var2", "coef", "pval")) %>%
  replace(is.na(.), 1)

## Build correlation matrix in ggplot
ggplot(df, aes(x = Var1, y = Var2, fill=coef, color = pval)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", space = "Lab",
                       name = "Pearson\nCorrelation") +
  guides(fill = guide_colourbar(barwidth = 1,
                                barheight = 30)) +
  theme_minimal() +
  coord_fixed()

## Build matrix using ggcorrplot
ggcorrplot(sub_c, outline.color = "white",
           colors = c("#78a2cc", "white", "#ff6961"),
           p.mat = p_mat, insig = "blank")

# 1.2. Graph Method -------------------------------------------------------

## Calculate correlations
data_corr <- corrInputs %>%
  correlate()

## Filter correlations for 0.6 threshold
g <- data_corr %>%
  shave() %>%
  stretch() %>%
  drop_na() %>%
  filter((r > 0.6)|(r < -0.6)) %>%
  mutate(band = cut(r, breaks = c(-Inf, -0.8, -0.6, 0, 0.6, 0.8, Inf)))

## Build correlation graph
graph_corr <- g %>%
  as_tbl_graph()

## Fix level order
graph_corr <- graph_corr %>%
  activate(edges) %>%
  mutate(band = factor(band, levels = c("(-Inf,-0.8]","(-0.8,-0.6]","(0.6,0.8]","(0.8, Inf]")))

## Plot graph
graph_corr %>% ggraph(layout = "kk") + 
  geom_edge_diagonal(aes(colour = band), edge_width = 0.5) + #alpha = 0.3) +
  geom_node_text(aes(label = name), size = 2, check_overlap = TRUE,
                 col = "black", fontface = "bold") +
  geom_node_point(size = 0.5, colour = "black") +
  scale_edge_colour_manual(values = c("#0000FF", "#ADD8E6",  "#FAA0A0", "#FF0000"), 
                           name = "Variable\nCorrelation") +
  theme_void()

ggsave("Output Data/4. Correlation Testing/Graph-Test.pdf", width = 16, height = 12, dpi = 600)


# 2. Pruning Variables ----------------------------------------------------


# 2.1. Setup --------------------------------------------------------------

# ## Get list of variables for identifying those to prune etc.
# list_df <- as.data.frame(colnames(corrInputs))
# list_df <- list_df %>% setNames(c("var"))
# 
# ## Write out 
# write.csv(list_df, "Output Data/4. Correlation Testing/vars.csv")

# 2.2. Iterative pruning ---------------------------------------------------------

## Read in the list of variables and what to prune
p1 <- read.csv("Output Data/4. Correlation Testing/vars.csv")

## Identify all variables to be removed
toKeep <- p1 %>%
  filter(action != 0 | is.na(action))

## Remove all variables assigned 0s in pruning 
p1_inputs <- corrInputs %>%
  select(toKeep$var)

## Rebuild the graph
## Calculate correlations
data_corr <- p1_inputs %>%
  correlate()

## Filter correlations for 0.6 threshold
g <- data_corr %>%
  shave() %>%
  stretch() %>%
  drop_na() %>%
  filter((r > 0.6)|(r < -0.6)) %>%
  mutate(band = cut(r, breaks = c(-Inf, -0.8, -0.6, 0, 0.6, 0.8, Inf)))

## Build correlation graph
graph_corr <- g %>%
  as_tbl_graph()

## Fix level order
graph_corr <- graph_corr %>%
  activate(edges) %>%
  mutate(band = factor(band, levels = c("(-Inf,-0.8]","(-0.8,-0.6]","(0.6,0.8]","(0.8, Inf]")))

## Plot graph
graph_corr %>% ggraph(layout = "kk") + 
  geom_edge_diagonal(aes(colour = band), edge_width = 0.5) + #alpha = 0.3) +
  geom_node_text(aes(label = name), size = 2, check_overlap = FALSE,
                 col = "black", fontface = "bold") +
  geom_node_point(size = 1, colour = "black") +
  scale_edge_colour_manual(values = c("#0000FF", "#ADD8E6",  "#FAA0A0", "#FF0000"), 
                           name = "Variable\nCorrelation") +
  theme_void()

ggsave("Output Data/Correlation Testing/Graph-Test-Prune4.pdf", width = 16, height = 12, dpi = 600)


# 2.3. Checking final set of variables ------------------------------------

## Get list of final variables
final <- p1_inputs

## Convert to correlation matrix
colnames(final) <- seq(ncol(final))
final_corr <- cor(final)

## Check correlations against eachother
ggcorrplot(final_corr)

## Get table of highest correlations
final_melt <- melt(final_corr)
final_melt <- final_melt %>%
  filter(value != 1) %>%
  arrange(desc(value))

## Appears to be no correlations over 0.6 or below 0.6
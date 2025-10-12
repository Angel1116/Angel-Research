library(causaldata)  # Contains example causal inference datasets
library(tidyverse)   # Data manipulation and visualization
library(vtable)      # Summary tables for descriptive statistics
library(Matching)    # Functions for propensity score matching
library(ggplot2)     # Plotting library
library(dplyr)

# --------------------------------------------------------------------------------
# Load the dataset
# --------------------------------------------------------------------------------
data("nsw_mixtape")   # Load the NSW (National Supported Work) dataset
nsw <- nsw_mixtape %>% dplyr::select(-data_id)
nsw



# --------------------------------------------------------------------------------
# Specify outcome, treatment, and covariates
# --------------------------------------------------------------------------------
Y <- nsw %>% pull(re78)  # Outcome variable: 1978 earnings
D <- nsw %>% pull(treat) # Treatment variable: participation in job training
nsw <- nsw %>% mutate(initial_weight = 1)  # add weight column first
X <- nsw %>%
  dplyr::select(-treat, -re78, -initial_weight) %>% 
  as.matrix()



# --------------------------------------------------------------------------------
# Estimate propensity scores using logistic regression
# --------------------------------------------------------------------------------
ps_model <- glm(treat ~ X,
                data = nsw, family = binomial(link = 'logit'))
nsw <- nsw %>%
  mutate(propensity = predict(ps_model, type = "response"))
summary(nsw$propensity)  # Check that propensity scores are between 0 and 1



# --------------------------------------------------------------------------------
# Trim extreme propensity scores
# --------------------------------------------------------------------------------
# Remove observations with very low (<0.05) or very high (>0.95) propensity scores
nsw <- nsw %>%
  mutate(propensity = ifelse(propensity < 0.05 | propensity > 0.95, 
                             NA_real_, propensity))
summary(nsw$propensity)  # Confirm trimming



# --------------------------------------------------------------------------------
# Plot propensity score distribution before/after Propensity Score Weighting
# --------------------------------------------------------------------------------
## Before Propensity Score Weighting
ggplot(nsw, aes(x = propensity, fill = factor(treat))) +
  geom_density(alpha = 0.4) +  # Overlay density plots
  labs(
    x = "Propensity Score",
    y = "Density",
    fill = "Treatment Status"
  ) +
  scale_fill_manual(values = c("#56B4E9","#DC000099"), 
                    labels = c("Control", "Treated")) +
  theme_minimal() +
  ggtitle("Propensity Score Distribution before Propensity Score Weighting")  # Visualize common support


## After Propensity Score Weighting
ggplot(nsw, aes(x = propensity, fill = factor(treat), weight = w.out$weights)) +
  geom_density(alpha = 0.4) +  # Overlay density plots
  labs(
    x = "Propensity Score",
    y = "Density",
    fill = "Treatment Status"
  ) +
  scale_fill_manual(values = c("#56B4E9","#DC000099"), 
                    labels = c("Control", "Treated")) +
  theme_minimal() + 
  ggtitle("Propensity Score Distribution after Propensity Score Weighting")  # Visualize common support




# --------------------------------------------------------------------------------
# Calculate weight(inverse propensity weight) of unit
# --------------------------------------------------------------------------------
library(WeightIt)
w.out <- weightit(treat ~ X, data = nsw, method = "glm")
summary(w.out)
# w.out$weights = ifelse(treat == 1, 1 / propensity, 1 / (1 - propensity))




# --------------------------------------------------------------------------------
# Years of education Distribution before/after Propensity Score Weighting
# --------------------------------------------------------------------------------
ggplot(nsw, aes(x = educ, fill = factor(treat))) +
  geom_density(alpha = 0.4) +  # Overlay density plots
  labs(
    x = "Years of education",
    y = "Density",
    fill = "Treatment Status"
  ) +
  scale_fill_manual(values = c("#56B4E9","#DC000099"), 
                    labels = c("Control", "Treated")) +
  theme_minimal() +
  ggtitle("Years of education Distribution before Propensity Score Weighting")  


ggplot(nsw, aes(x = educ, fill = factor(treat), weight = w.out$weights)) +
  geom_density(alpha = 0.4) + 
  labs(
    x = "Years of education",
    y = "Weighted Density",
    fill = "Treatment Status"
  ) +
  scale_fill_manual(values = c("#56B4E9","#DC000099"), 
                    labels = c("Control", "Treated")) +
  theme_minimal() +
  ggtitle("Years of education Distribution after Propensity Score Weighting")




# --------------------------------------------------------------------------------
# Treatment effect before/after Propensity Score Weighting
# --------------------------------------------------------------------------------
library(survey)
data <- cbind(nsw, new_weight = w.out$weights)
data

## Before Propensity Score Weighting
design <- svydesign(ids = ~1, weights = ~initial_weight, data = data)
result <- svyglm(re78 ~ treat, design = design)
summary(result)
## The treatment effect(ATE) is 1794.3



## After Propensity Score Weighting
design <- svydesign(ids = ~1, weights = ~new_weight, data = data)
result <- svyglm(re78 ~ treat, design = design)
summary(result)
# The treatment effect(ATE) is 1641.3




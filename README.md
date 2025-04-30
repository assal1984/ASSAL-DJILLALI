#Weighted Probability Modeling for Bid Optimization Across Construction Work Items
# Load required libraries
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tibble)

# Define basic data
projects <- paste("Project", 1:30)
bidders <- c("bidder_1", "bidder_2", "bidder_3", "bidder_4")
work_items <- c("Excavation", "Concrete Work", "Tiling", "Painting", "Site Grading",
                "Wall Construction", "Pipe Installation", "Reinforcement", "Paving", "Carpentry")
units <- c("m³", "m³", "m²", "m²", "m²", "m²", "m", "ton", "m²", "m²")

# Function to generate project data with normal distribution
generate_project_data_normal <- function(project_name) {
  quantities <- sample(c(1000, 1000, 1000, 1000, 1000, 1000, 1100, 1000), 
                      length(work_items), replace = TRUE)
  project_data <- data.frame(
    Project = project_name,
    Work_Item = work_items,
    Unit = units,
    Quantity = quantities,
    stringsAsFactors = FALSE
  )
  
  # Parameters for normal distribution for each item
  item_parameters <- list(
    "Excavation" = list(mean = 1000, sd = 50),
    "Concrete Work" = list(mean = 2000, sd = 100),
    "Tiling" = list(mean = 2500, sd = 100),
    "Painting" = list(mean = 2200, sd = 150),
    "Site Grading" = list(mean = 2300, sd = 200),
    "Wall Construction" = list(mean = 2000, sd = 180),
    "Pipe Installation" = list(mean = 1000, sd = 50),
    "Reinforcement" = list(mean = 3000, sd = 150),
    "Paving" = list(mean = 2000, sd = 80),
    "Carpentry" = list(mean = 3000, sd = 180)
  )
  
  # Generate bids following normal distribution for each item
  for (bidder in bidders) {
    project_data[[bidder]] <- sapply(1:length(work_items), function(i) {
      item <- work_items[i]
      params <- item_parameters[[item]]
      # Generate value from normal distribution ensuring no negative values
      repeat {
        bid <- rnorm(1, mean = params$mean, sd = params$sd)
        if (bid > 0) break
      }
      # Round to 2 decimal places
      round(bid, 2)
    })
  }
  return(project_data)
}

# Generate new data following normal distribution
all_projects <- lapply(projects, generate_project_data_normal) %>% bind_rows()

# Function to create data frame for each work item
create_work_item_df <- function(data, work_item_name) {
  result_df <- data %>%
    filter(Work_Item == work_item_name) %>%
    select(-Work_Item, -Unit, -Quantity) %>%
    t() %>% # Transpose table
    as.data.frame() # Convert to data frame
  
  # Make first row column names
  colnames(result_df) <- result_df[1,]
  result_df <- result_df[-1,]
  
  # Make row names bidder names
  rownames(result_df) <- paste0("bidder_", 1:nrow(result_df))
  
  # Convert values to numeric
  result_df <- result_df %>%
    mutate(across(everything(), as.numeric))
  
  return(result_df)
}

# Function to estimate parameters for all items
estimate_all_parameters <- function(all_projects) {
  # List of all work items
  work_items <- unique(all_projects$Work_Item)
  
  # Internal function to estimate parameters for each item
  estimate_item_parameters <- function(item) {
    # Create logarithmic price matrix
    Y <- log(create_work_item_df(all_projects, item))
    
    # Define initial parameters
    r <- nrow(Y)
    c <- ncol(Y)
    a <- rep(0, r)
    b <- rep(0, c)
    s <- rep(1, r)
    r_j <- rep(1, c)
    d <- !is.na(Y)
    converged <- FALSE
    
    # Iterative estimation process
    max_iter <- 100
    tolerance <- 1e-6
    
    for (iter in 1:max_iter) {
      a_old <- a
      b_old <- b
      s_old <- s
      r_j_old <- r_j
      
      # Update a_i
      for (i in 1:r) {
        valid_indices <- !is.na(Y[i, ])
        a[i] <- sum((Y[i, valid_indices] - b[valid_indices]) * d[i, valid_indices]) / sum(d[i, valid_indices])
      }
      
      # Update b_j
      for (j in 1:c) {
        valid_indices <- !is.na(Y[, j])
        b[j] <- sum((Y[valid_indices, j] - a[valid_indices]) * d[valid_indices, j]) / sum(d[valid_indices, j])
      }
      
      # Update s_i
      for (i in 1:r) {
        valid_indices <- !is.na(Y[i, ])
        s[i] <- sqrt(sum(((Y[i, valid_indices] - a[i] - b[valid_indices]) * d[i, valid_indices])^2) / sum(d[i, valid_indices]))
      }
      
      # Update r_j
      for (j in 1:c) {
        valid_indices <- !is.na(Y[, j])
        r_j[j] <- sqrt(sum(((Y[valid_indices, j] - a[valid_indices] - b[j]) * d[valid_indices, j])^2) / sum(d[valid_indices, j]))
      }
      
      # Check convergence
      if (max(abs(a - a_old), na.rm = TRUE) < tolerance &&
          max(abs(b - b_old), na.rm = TRUE) < tolerance &&
          max(abs(s - s_old), na.rm = TRUE) < tolerance &&
          max(abs(r_j - r_j_old), na.rm = TRUE) < tolerance) {
        converged <- TRUE
        break
      }
    }
    
    # Return results
    list(
      bidders = data.frame(
        Work_Item = item,
        Bidder = rownames(Y),
        a_i = a,
        s_i = s,
        stringsAsFactors = FALSE
      ),
      projects = data.frame(
        Work_Item = item,
        Project = colnames(Y),
        b_j = b,
        r_j = r_j,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Apply function to all work items
  all_results <- map(work_items, safely(estimate_item_parameters))
  
  # Process results
  successful_results <- transpose(all_results)$result
  
  # Aggregate bidder data
  bidders_df <- map_dfr(successful_results, ~.x$bidders)
  
  # Aggregate project data
  projects_df <- map_dfr(successful_results, ~.x$projects)
  
  # Add quantity information for each project and item
  quantities_df <- all_projects %>%
    select(Project, Work_Item, Quantity) %>%
    distinct()
  
  # Merge project data with quantities
  projects_df <- projects_df %>%
    left_join(quantities_df, by = c("Project", "Work_Item"))
  
  # Return results as list
  list(
    bidders = bidders_df,
    projects = projects_df,
    estimation_date = Sys.time(),
    num_items = length(work_items),
    num_bidders = length(unique(bidders_df$Bidder)),
    num_projects = length(unique(projects_df$Project))
  )
}

# Use the function
all_parameters <- estimate_all_parameters(all_projects)

# Define helper functions

calculate_expected_cost <- function(work_item, bidder, all_params) {
  bidder_data <- subset(all_params$bidders, 
                       Work_Item == work_item & Bidder == bidder)
  if(nrow(bidder_data) == 0) return(NA)
  a_i <- bidder_data$a_i
  s_i <- bidder_data$s_i
  exp(a_i + 0.5 * s_i^2)
}

calculate_marginal_win_probability <- function(markup, work_item, target_bidder, all_params) {
  target_data <- subset(all_params$bidders, 
                       Work_Item == work_item & Bidder == target_bidder)
  if(nrow(target_data) == 0) return(0)
  
  a_target <- target_data$a_i
  s_target <- target_data$s_i
  ln_v_target <- log(markup) + log(calculate_expected_cost(work_item, target_bidder, all_params))
  
  competitors <- subset(all_params$bidders, 
                       Work_Item == work_item & Bidder != target_bidder)
  if(nrow(competitors) == 0) return(1)
  
  z_scores <- (ln_v_target - competitors$a_i) / competitors$s_i
  prod(1 - pnorm(z_scores))
}

calculate_expected_profit <- function(markup, work_item, bidder, all_params) {
  cost <- calculate_expected_cost(work_item, bidder, all_params)
  win_prob <- calculate_marginal_win_probability(markup, work_item, bidder, all_params)
  list(
    profit = win_prob * (markup - 1) * cost,
    win_prob = win_prob,
    expected_cost = cost
  )
}

find_optimal_markup <- function(work_item, bidder, all_params) {
  objective <- function(markup) {
    -calculate_expected_profit(markup, work_item, bidder, all_params)$profit
  }
  result <- optimize(objective, interval = c(1.0, 1.2))
  
  optimal_result <- calculate_expected_profit(result$minimum, work_item, bidder, all_params)
  
  list(
    optimal_markup = result$minimum,
    expected_profit = optimal_result$profit,
    marginal_win_prob = optimal_result$win_prob,
    expected_cost = optimal_result$expected_cost
  )
}

# Calculate optimal rates for all bidders
results <- data.frame()
all_work_items <- unique(all_parameters$bidders$Work_Item)
all_bidders <- unique(all_parameters$bidders$Bidder)

for (item in all_work_items) {
  for (bidder in all_bidders) {
    if (nrow(subset(all_parameters$bidders, Work_Item == item & Bidder == bidder)) > 0) {
      optimal <- find_optimal_markup(item, bidder, all_parameters)
      results <- rbind(results, data.frame(
        Work_Item = item,
        Bidder = bidder,
        Optimal_Markup = optimal$optimal_markup,
        Expected_Cost = optimal$expected_cost,
        Unit_price = optimal$expected_cost * optimal$optimal_markup,
        Marginal_Win_Probability = optimal$marginal_win_prob,
        Expected_Profit = optimal$expected_profit,
        stringsAsFactors = FALSE
      ))
    }
  }
}


# weighted_win_probability


calculate_weighted_win_probability <- function(markup_list, bidder, all_params, quantities) {
  # Verify inputs
  if(!bidder %in% all_params$bidders$Bidder) {
    stop("The specified bidder is not in the bid data")
  }
  
  # Standardize item names
  names(quantities) <- gsub("_", " ", names(quantities))
  names(markup_list) <- gsub("_", " ", names(markup_list))
  
  # Check item matching
  missing_items <- setdiff(names(markup_list), names(quantities))
  if(length(missing_items) > 0) {
    stop(paste("The following items are not in the quantities list:", 
               paste(missing_items, collapse = ", ")))
  }
  
  # Calculate costs and weights
  expected_costs <- sapply(names(markup_list), function(item) {
    cost <- calculate_expected_cost(item, bidder, all_params)
    if(is.na(cost)) {
      stop(paste("No data for bidder", bidder, "in item", item))
    }
    cost
  })
  
  weights <- expected_costs * quantities[names(markup_list)]
  total_weight <- sum(weights)
  
  # Calculate win probabilities
  item_probs <- sapply(names(markup_list), function(item) {
    prob <- calculate_marginal_win_probability(
      markup_list[[item]], item, bidder, all_params
    )
    if(is.na(prob)) 0 else prob
  })
  
  Total_cost= sum(expected_costs*quantities)
     Total_Markup=sum(unlist(markup_list)*expected_costs)/ sum(expected_costs)
  
  # Final results
  weighted_prob <- sum(item_probs * weights) / total_weight
  
  list(
    weighted_probability = weighted_prob,
    Total_cost= Total_cost,
       Total_Markup= Total_Markup,
    Total_Bid= Total_Markup * Total_cost,
    Net_Profit= Total_Markup * Total_cost- Total_cost,
    item_details = data.frame(
      Work_Item = names(markup_list),
      Markup = round(unlist(markup_list),3),
      Expected_Cost = round(expected_costs,2),
      Quantity = quantities[names(markup_list)],
      Unit_Price = round(unlist(markup_list)*expected_costs,2),
      Individual_Probability = round(item_probs,3),
      stringsAsFactors = FALSE
    )    
  )
}






#NEW PROJECT DATA



# Define data
#المتعهد
bidder = "bidder_2"
#الكميات 
new_quantities <- c(Excavation = 590,`Concrete Work` = 500,Tiling = 130,Painting = 1600,`Site Grading` = 1790,`Wall Construction` = 3100,`Pipe Installation` = 680,Reinforcement = 480,Paving = 1000,Carpentry = 1070)
#هامش الربح المطبق لكل بند أشغال ( في حالة تطبيق الهامش الأمثل لكل بند)
opt_markup_list <- list(
  `Excavation` = find_optimal_markup("Excavation", bidder, all_parameters)$optimal_markup,
  `Concrete Work` = find_optimal_markup("Concrete Work", bidder, all_parameters)$optimal_markup,
  `Tiling` = find_optimal_markup("Tiling", bidder, all_parameters)$optimal_markup,
  `Painting` = find_optimal_markup("Painting", bidder, all_parameters)$optimal_markup,
  `Site Grading` = find_optimal_markup("Site Grading", bidder, all_parameters)$optimal_markup,
  `Wall Construction` = find_optimal_markup("Wall Construction", bidder, all_parameters)$optimal_markup,
  `Pipe Installation` = find_optimal_markup("Pipe Installation", bidder, all_parameters)$optimal_markup,
  `Reinforcement` = find_optimal_markup("Reinforcement", bidder, all_parameters)$optimal_markup,
  `Paving` = find_optimal_markup("Paving", bidder, all_parameters)$optimal_markup,
  `Carpentry` = find_optimal_markup("Carpentry", bidder, all_parameters)$optimal_markup
)

# Execute and display results
result <- tryCatch({
  calculate_weighted_win_probability(
    markup_list = opt_markup_list,
    bidder = bidder,
    all_params = all_parameters,
    quantities = new_quantities
  )
}, error = function(e) {
  message("Error: ", e$message)
  return(NULL)
})
result





#ELASTICITY AND WEIGHT(quntities-costs) ANALYSIS


# Elasticity analysis
# Prepare data
markup_values <- seq(1, 1.5, 0.01)
items <- names(custom_markup_list)

# Calculate elasticity
calculate_elasticity <- function(bidder, item, markup, all_params) {
  delta <- 0.01
  p1 <- calculate_marginal_win_probability(markup, item, bidder, all_params)
  p2 <- calculate_marginal_win_probability(markup + delta, item, bidder, all_params)
  if (p1 == 0 || is.na(p1)) return(0)
  ((p2 - p1) / p1) / delta
}

# Average elasticity for each item
mean_elasticities <- sapply(items, function(item) {
  elasticities <- sapply(markup_values, function(m) {
    calculate_elasticity(bidder, item, m, all_parameters)
  })
  mean(elasticities, na.rm = TRUE)
})

# Elasticity weights
Elast_Weight<- mean_elasticities/sum(mean_elasticities)

# Calculate weights
expected_costs <- sapply(items, function(item) {
  calculate_expected_cost(item, bidder, all_parameters)
})
weights <- expected_costs * new_quantities[items]
normalized_weights <- weights / sum(weights)

# Get optimal markups
opt_markup <- sapply(items, function(item) {
  find_optimal_markup(item, bidder, all_parameters)$optimal_markup
})

Df<-data.frame(cbind(opt_markup ,Mean_Elasticity = round(mean_elasticities, 4),Elast_Weight,
  Weight_Impact = round(normalized_weights, 4)
))
Df





#Optimistaion with DEoptim



Wei_Ela<- Df$Weight_Impact/Df$Elast_Weight
objective_function<- function(params) {
coef<- params[1:10]
adjusted_markup<-coef*Wei_Ela
   # ضبطالأسماء
  item <- names(new_quantities)
names(adjusted_markup) <- items

result<- calculate_weighted_win_probability(
markup_list = adjusted_markup,
bidder = bidder,
all_params = all_parameters,
quantities = new_quantities
   )

total_expected_profit<- result$weighted_probability * (result$Total_Markup - 1) * result$Total_cost


return(-total_expected_profit)
}
Wei_Ela [Wei_Ela == 0] <- 1e-6
names(Wei_Ela) <- names(new_quantities)

library(DEoptim)
fit_result<- DEoptim(
fn = objective_function,
lower = 0.8/Wei_Ela,
upper = 1.25/Wei_Ela,
  control = list(itermax = 50, trace = FALSE)
)
fit_result$optim$bestmem
coef<-fit_result$optim$bestmem[1:10]


#----------------#الرسومات
Suggested_MarkUp<- coef* Wei_Ela
plot(Wei_Ela, coef, col = "red", lwd = 3,type="p")
abline(v=1,lty=2,lwd=2,col=1)
names(Suggested_MarkUp) <- items 
# حساب النتيجة النهائية
final_result<- calculate_weighted_win_probability(
markup_list = Suggested_MarkUp,
bidder = bidder,
all_params = all_parameters,
quantities = new_quantities
)

# عرض النتيجة
print(final_result)
abline(h=final_result$Total_Markup,lty=2,col= "green",lwd=3)



# البيانات المقدمة
data <- data.frame(Wei_Ela ,coef)
plot(data)
model <- nls(coef ~ a * exp(b * Wei_Ela) + c, 
data = data,
start = list(a = 10, b = -2, c = 5),  # قيم أولية معدلة
            control = nls.control(maxiter = 1000))
# عرض الملخص
summary(model)

# استخراج المعاملات
a <- coef(model)["a"]
b <- coef(model)["b"]
c <- coef(model)["c"]

wei_a<- seq(min(Wei_Ela), max(Wei_Ela), 0.01)
Theoric_coef<- a * exp(b * wei_a) + c
plot(wei_a, Theoric_coef, col = "red", lwd = 3,type="l")
lines(data$Wei_Ela, data$coef, type="p",lwd=4,col=1)

Sug_Theoric_MarkUp<- Theoric_coef *wei_a
plot(wei_a, Sug_Theoric_MarkUp,type="l",lwd=3,col="red")
lines(Wei_Ela, Wei_Ela*coef,type="p",lwd=4,col=1)

sug<- a * exp(b * Wei_Ela) + c
names(sug) <- names(new_quantities)
calculate_weighted_win_probability(
markup_list = sug,
bidder = bidder,
all_params = all_parameters,
quantities = new_quantities
)


sug_clipped <- pmin(pmax(sug, 0.7), 1.3)# Define data
calculate_weighted_win_probability(
markup_list = sug_clipped,
bidder = bidder,
all_params = all_parameters,
quantities = new_quantities
)





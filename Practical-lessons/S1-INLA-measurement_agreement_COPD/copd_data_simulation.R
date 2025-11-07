# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 21  # Subjects
replic <- 3  # Replicates
d <- 2  # Devices
l <- 11  # Activities

# Variance components
sigma_subj <- 4
sigma_act <- 4.5
sigma_inter_subj_act <- 2.5
sigma_inter_subj_dev <- 1
sigma_inter_act_dev <- 2
sigma_epsilon <- 3.5

# Fixed effects
beta0 <- 22  # Intercept
beta <- 1    # Device effect

# Generate random effects
u_subj <- rnorm(n, 0, sigma_subj)
u_act <- rnorm(l, 0, sigma_act)
u_subj_act <- rnorm(n * l, 0, sigma_inter_subj_act)
u_subj_dev <- rnorm(n * d, 0, sigma_inter_subj_dev)
u_act_dev <- rnorm(l * d, 0, sigma_inter_act_dev)

# Create dataset
combinations <- expand.grid(
  replicate = 1:replic,
  act = 1:l,
  subj = 1:n,
  device = 1:d
)

# Assign random effects
combinations$u_subj <- u_subj[combinations$subj]
combinations$u_act <- u_act[combinations$act]
combinations$u_subj_act <- u_subj_act[interaction(combinations$subj, combinations$act)]
combinations$u_subj_dev <- u_subj_dev[interaction(combinations$subj, combinations$device)]
combinations$u_act_dev <- u_act_dev[interaction(combinations$act, combinations$device)]

# Simulate response variable
data <- combinations %>%
  mutate(
    y = beta0 + beta * device + u_subj + u_act + u_subj_act +
      u_subj_dev + u_act_dev + rnorm(nrow(combinations), 0, sigma_epsilon)
  )

# Add interaction identifiers
data <- data %>%
  mutate(
    subj_act = interaction(subj, act),
    subj_device = interaction(subj, device),
    act_device = interaction(act, device)
  )

data$y <- data$y + 12



# Define activities
activities <- c(
  "Sitting", 
  "Lying", 
  "Standing", 
  "Slow walking", 
  "Fast walking", 
  "Sweeping", 
  "Lifting objects", 
  "Standing and walking", 
  "Climbing stairs", 
  "Treadmill (flat walking)", 
  "Treadmill (4% slope)"
)

# Assign activities as a factor
data$act <- factor(data$act, levels = 1:11, labels = activities)

data$act

# Define device
device <- c("oxicon", "chest_band")
data$device <- factor(data$device, levels = 1:2, labels = device)


# Visualize the distribution of the simulated response variable
ggplot(data, aes(x = y)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Simulated Response", x = "Response (y)", y = "Frequency")


data_def <- data %>%
  dplyr::select(subj, y, replicate, act, device)


writexl::write_xlsx(data_def, path = "data_copd.xlsx")

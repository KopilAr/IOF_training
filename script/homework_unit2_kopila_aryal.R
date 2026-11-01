### Part 1: Data Ingestion & Structural Inspection
#1. Load `data/raw/bagmati_raw_inventory.csv` into an object named `inv` using `read.csv()`.
inv <- read.csv("data/raw/bagmati_raw_inventory.csv")
#2. Inspect the dataset:
   #How many total tree rows and how many columns are there? Print exact counts with `nrow()` and `ncol()`.
   nrow(inv)
   ncol(inv)
   # Print all column names with `names()`. Note the exact case-sensitivity of each column.
   names(inv)
   # Inspect the internal structure and storage types of each column using `str()`.
   str(inv)
#3. Check the specific data type and storage mode of `DBH_cm` and `Species` using `class()` and `typeof()`.
class(inv$DBH_cm)
class(inv$Species)
typeof(inv$DBH_cm)
typeof(inv$Species)

### Part 2: Forensic Data Auditing & Missing Values
#Raw forestry survey sheets frequently contain transcription errors and protocol sentinel codes. Write Base R code to audit the following:
#1. **Missing Data (`NA`):** 
   #* How many explicit `NA` values exist in `DBH_cm`? How many in `Height_m`? Print exact counts using `is.na()` and `sum()`.
is.na(inv$DBH_cm)
sum(is.na(inv$DBH_cm))
is.na(inv$Height_m)
sum(is.na(inv$Height_m))
#2. Protocol Missing Codes (`-99` and `999`):
  #  Field crews often record `-99` or `999` when a measurement cannot be taken. How many trees have `DBH_cm == -99`? How many have `DBH_cm == 999`?
  #  How many trees have `Height_m == -99`?
    #  How many trees have `Height_m == 999`? Print exact counts using logical checks and `sum()`.

sum(inv$DBH_cm == -99, na.rm = TRUE)
sum(inv$DBH_cm == 999, na.rm = TRUE)
sum(inv$Height_m == -99, na.rm = TRUE)
sum(inv$Height_m == 999, na.rm = TRUE)

#3. **Biological Boundary Violations:**
   #* Are there any trees with non-positive diameters ($DBH \le 0$)? Use logical comparison and `sum(..., na.rm = TRUE)` to report the exact count.
sum(inv$DBH_cm <= 0, na.rm = TRUE)
#4. **The Plot P17 Field Sheet Transcription Case:**
   #* Query tree `Tree_ID == "T203"` in Plot `"P17"` using bracket row indexing: ```r
     inv[inv$Plot_ID == "P17" & inv$Tree_ID == "T203", ]
     ```
   #* Its recorded DBH is `999 cm`, despite a measured tree height of `21.3 m`. Is a 10-metre diameter tree biologically possible in this forest stand?
   #* The physical field note confirms the scribe recorded `999` as a transcription error instead of the measured value. The audit log confirms the true measured diameter was **`29.9 cm`**. Assign this corrected DBH value to tree T203 in your dataset.
   inv[inv$Plot_ID == "P17" & inv$Tree_ID == "T203", "DBH_cm"] <- 29.9
#5. **Millimetre Entry Errors:**
   #* A common field crew error in Nepal forestry is recording caliper measurements in millimetres instead of centimetres. Identify trees with $DBH > 150\text{ cm}$ and $DBH < 900\text{ cm}$.
   #* Write a relational query to report the exact number of stems affected by this error.
   #* Correct these rows by dividing their DBH values by 10. *(Caution: ensure missing values in your indexing expression do not trigger runtime assignment errors!)*
sum(inv$DBH_cm > 150 & inv$DBH_cm < 900, na.rm = TRUE)
inv[inv$DBH_cm > 150 & inv$DBH_cm < 900, "DBH_cm"] <- inv[inv$DBH_cm > 150 & inv$DBH_cm < 900, "DBH_cm"] / 10


### Part 3: Logical Subsetting & Ecological Queries
#*Create a clean working subset `inv_clean` excluding records where DBH is missing (`NA`), non-positive ($\le 0$), or sentinel codes (-99, 999).*

#1. **Size Classes via `cut()`:**
 #  * Create a new factor column in `inv_clean` named `size_class` categorizing tree diameters into:
  #   * **Sapling:** $(0, 10]\text{ cm}$
   #  * **Pole:** $(10, 30]\text{ cm}$
    # * **Sawtimber:** $> 30\text{ cm}$
   #* Generate a frequency table using `table(inv_clean$size_class)`. Report the exact stem count in each category. Which diameter class dominates the stand?
inv_clean <- inv[!is.na(inv$DBH_cm) & inv$DBH_cm > 0 & !(inv$DBH_cm %in% c(-99, 999)), ]
inv_clean$size_class <- cut(inv_clean$DBH_cm, breaks = c(0, 10, 30, Inf), labels = c("Sapling", "Pole", "Sawtimber"), right = TRUE)
table(inv_clean$size_class)     

# 2. **Finding the Champion Tree (`which.max`):**
   #* Which row number in your clean dataset holds the true biological champion tree (largest valid diameter)?
  # * Print that champion tree's full record (`Plot_ID`, `Tree_ID`, `Species`, `DBH_cm`, `Height_m`).
   #* *(Warning: If your output reports a DBH of 999 cm or 343 cm, you failed to screen sentinel codes or correct millimetre entries before searching!)*
champion_row <- which.max(inv_clean$DBH_cm)
inv_clean[champion_row, ]

# 3. **Target Species Subsetting:**
   #* How many *Shorea robusta* (Sal) trees exist in the clean inventory?
   #* What is the mean DBH of *Shorea robusta* trees? *(Remember `na.rm = TRUE`!)*
sum(inv_clean$Species == "Shorea robusta", na.rm = TRUE)
mean(inv_clean$DBH_cm[inv_clean$Species == "Shorea robusta"], na.rm = TRUE)

### Part 4: Defensive Functions & Stand Variability
# 1. **Basal Area Calculation:**
   #* Write or reuse your defensive `calculate_basal_area(dbh_cm)` function (ensuring it stops on $DBH \le 0$ and warns on $DBH > 150$).
   # * Calculate basal area for all trees in `inv_clean` and store it in a new column `inv_clean$ba_m2`.
   #* What is the **total basal area ($\text{m}^2$)** of all valid trees across the inventory? Report to 4 decimal places.
   
   # Define calculate_basal_area BEFORE calling it
calculate_basal_area <- function(dbh_cm) {
  if (any(dbh_cm <= 0, na.rm = TRUE)) {
    stop("Error: Biological boundary violation. DBH values must be strictly positive (> 0).")
  }
  if (any(dbh_cm > 150, na.rm = TRUE)) {
    warning("Warning: Extreme outlier observed. DBH value exceeds 150 cm threshold.")
  }
  ba_m2 <- (pi * dbh_cm^2) / 40000
  return(ba_m2)
}
inv_clean$ba_m2 <- calculate_basal_area(inv_clean$DBH_cm)
total_basal_area <- sum(inv_clean$ba_m2, na.rm = TRUE)
cat("Total Basal Area:", round(total_basal_area, 4), "m²\n")


# 2. **Coefficient of Variation ($CV\%$):**
   #* Write your custom `calc_cv()` function:
    # $$CV\% = \frac{sd(x, \text{na.rm = TRUE})}{mean(x, \text{na.rm = TRUE})} \times 100$$
   #* Calculate the $CV\%$ of `DBH_cm` across all measured trees. Does the stand show low ($<20\%$) or high ($>30\%$) structural variability?
  calc_cv <- function(x) {
    cv <- (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
    return(round(cv, digits= 2))
    }
    calc_cv(inv_clean$DBH_cm)

# 3. **Testing Model Domain Boundaries:**
   #* Call `calculate_volume_example(dbh_cm, height_m)` on:
     # * **Tree A:** $DBH = 32.4\text{ cm}$, $Height = 22.1\text{ m}$
     # * **Tree B:** $DBH = 6.5\text{ cm}$, $Height = 4.2\text{ m}$
   #* What happens when you run Tree B? Explain in a code comment what warning was triggered and why domain guards are necessary in allometric forestry equations.

calculate_volume_example <- function(DBH_cm, height_m) {
  if (DBH_cm < 10 || height_m < 5) {
    warning("Domain Guard Triggered: Tree parameters fall below reliable allometric boundaries (DBH < 10cm or Height < 5m).")
  }
  # Using matching uppercase 'DBH_cm' everywhere inside the calculation
  vol <- 0.00007854 * (DBH_cm^2) * height_m * 0.45
  return(vol)
}
cat("Tree A Volume:", calculate_volume_example(32.4, 22.1), "\n")
cat("Tree B Volume Execution:\n")
vol_b <- calculate_volume_example(6.5, 4.2)
cat("Tree B Volume:", vol_b, "\n")

### Part 5: Exploratory Visualisation in Base R
#Generate and save a 2-by-2 multi-panel diagnostic graph into your `figures/` directory as `figures/homework_unit2_diagnostics.png`:
```r
#png("figures/homework_unit2_diagnostics.png", width = 1200, height = 1000, res = 150)
#par(mfrow = c(2, 2))
 1. Close any accidentally left-open graphic streams first
graphics.off()

# 2. Open the clean file writing device
png("figures/homework_unit2_diagnostics.png", width = 1200, height = 1000, res = 150)

# 3. Apply the 2x2 grid layout boundaries
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 1.5))

# 1. Histogram of DBH_cm with a dashed red line for the mean
hist(inv_clean$DBH_cm, main = "Histogram of DBH_cm", xlab = "DBH (cm)", col = "lightblue", border = "black")
abline(v = mean(inv_clean$DBH_cm, na.rm = TRUE), col = "red", lty = 2, lwd = 2)  

# 2. Histogram of Height_m with a dashed blue line for the median
hist(inv_clean$Height_m, main = "Histogram of Height_m", xlab = "Height (m)", col = "lightgreen", border = "black")
abline(v = median(inv_clean$Height_m, na.rm = TRUE), col = "blue", lty = 2, lwd = 2)   

# 3. Boxplot of DBH_cm comparing the 4 stands (Stand_ID)
boxplot(DBH_cm ~ as.factor(Stand_ID), data = inv_clean, main = "Boxplot of DBH_cm by Stand_ID", xlab = "Stand ID", ylab = "DBH (cm)", col = "lightyellow", border = "black")  

# 4. Scatterplot of DBH_cm (X-axis) vs Height_m (Y-axis) with a trendline using abline(lm(...))
plot(inv_clean$DBH_cm, inv_clean$Height_m, main = "Scatterplot of DBH_cm vs Height_m", xlab = "DBH (cm)", ylab = "Height (m)", pch = 19, col = rgb(0.2, 0.2, 0.2, 0.5))
abline(lm(Height_m ~ DBH_cm, data = inv_clean), col = "darkgreen", lwd = 2)   

par(mfrow = c(1, 1))
dev.off()

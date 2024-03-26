library(VIM)
library(mice)
library(dplyr)
library(corrplot)
path <- "G:/eicu-crd"
file_name <- "/merged_df.csv"
merged_df <- read.csv(paste0(path, file_name))
merged_df <- merged_df %>% 
  mutate(across(everything(), ~ifelse(. == "", NA, .)))
patientunitstayid <- merged_df$patientunitstayid
merged_df$patientunitstayid <- NULL
# Rename
names(merged_df)[names(merged_df) == "admissionheight"] <- "height"
names(merged_df)[names(merged_df) == "admissionweight"] <- "weight"
names(merged_df)[names(merged_df) == "hospitaladmitsource"] <- "HAS"
names(merged_df)[names(merged_df) == "hospitaldischargelocation"] <- "HDL"
names(merged_df)[names(merged_df) == "hospitaldischargestatus"] <- "HDS"
names(merged_df)[names(merged_df) == "unittype"] <- "UT"
names(merged_df)[names(merged_df) == "unitadmitsource"] <- "UAS"
names(merged_df)[names(merged_df) == "unitdischargestatus"] <- "UDS"
names(merged_df)[names(merged_df) == "hospital_length_of_stay_in_days"] <- "HLoS"
names(merged_df)[names(merged_df) == "unit_length_of_stay_in_days"] <- "ULoS"
names(merged_df)[names(merged_df) == "noninvasivesystolic"] <- "SBP"
names(merged_df)[names(merged_df) == "noninvasivediastolic"] <- "DBP"
names(merged_df)[names(merged_df) == "noninvasivemean"] <- "MeanBP"
names(merged_df)[names(merged_df) == "WBC.x.1000"] <- "WBC"
names(merged_df)[names(merged_df) == "platelets.x.1000"] <- "platelets"
names(merged_df)[names(merged_df) == "anion.gap"] <- "AnionGap"
names(merged_df)[names(merged_df) == "Continuous.infusion.meds"] <- "CIM"
names(merged_df)[names(merged_df) == "bedside.glucose"] <- "BG"
# 1，将merged_df中的分类型变量转为factor形式，方便后续的imputation。
vars_to_convert <- c("gender", "ethnicity", "HAS", "HDL", "HDS", "UT", 
                     "UAS", "UDS", "smoking")
for(var in vars_to_convert) {
  merged_df[[var]] <- as.factor(merged_df[[var]])
}
# 2，根据ICU的特殊情景，确定患者各种指标的正常范围。部分使用范围，例如age、height和weight，部分使用IQR，但是由于是ICU场景，因此将默认的1.5调整为3
# age
merged_df$age[merged_df$age < 0 | merged_df$age > 120] <- NA
# admissionheight和admissionweight（考虑新生儿）
merged_df$height[merged_df$height < 46 | merged_df$height > 220] <- NA
merged_df$weight[merged_df$weight < 2.5 | merged_df$weight > 300] <- NA
# The rest of numerical variables
num_vars <- setdiff(names(merged_df), c("age", "height", "weight", "gcsscore", "apachescore", vars_to_convert))
for (var in num_vars) {
  outliers <- boxplot.stats(merged_df[[var]], coef = 3)$out
  merged_df[[var]][merged_df[[var]] %in% outliers] <- NA
}
# 3，打印merged_df中所有变量的缺失值比例。
missing_percentage <- sapply(merged_df, function(x) mean(is.na(x)) * 100)
formatted_percentage <- sprintf("%.2f%%", missing_percentage)
print(formatted_percentage)
# 4，使用删除法和填补法混合处理缺失值。对于缺失值>80%的变量，直接删除
missing_proportion <- sapply(merged_df, function(col) sum(is.na(col)) / length(col))
filtered_df <- merged_df[, missing_proportion <= 0.8]
# 5，绘制绘制filtered_df中数值型变量之间的相关性热图，确保所有变量之间都是线性独立的
numeric_df <- filtered_df %>% select(-one_of(vars_to_convert))
cor_matrix <- cor(numeric_df, use = "complete.obs")  # 使用完整观测值
png(filename="G:/共病/图片/supplementary_fig3_corr_plot.png", width=8, height=8, units="in", res=600)
corrplot(cor_matrix, method = "color")
dev.off()
# 6，绘制缺失值图
png(filename="G:/共病/图片/supplementary_fig3_missing_value_visualization.png", width=10, height=4, units="in", res=600)
matrixplot(filtered_df)
dev.off()
# 7，使用mice对剩余变量进行缺失值填补，使用pmm (predictive mean matching) 方法填补连续型变量，而分类变量使用多项式回归
meth <- make.method(filtered_df)
meth[vars_to_convert] <- "polyreg"
# imp <- mice(filtered_df, m=5, maxit=50, method=meth, seed=500)
imp <- mice(filtered_df, m=3, maxit=5, method=meth, seed=500)
completed_data <- complete(imp, 1) # 选取第一个填补数据集
completed_data$patientunitstayid <- patientunitstayid
file_name <- "/completed_data.csv"
write.csv(completed_data, file = paste0(path, file_name), row.names = FALSE)
# 8，使用核密度函数进行可视化
jpeg(filename="G:/共病/图片/supplementary_fig3_density_plot.jpeg", width=10, height=6, units="in", res=600)
densityplot(imp)
dev.off()
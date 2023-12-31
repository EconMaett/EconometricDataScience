rm(list=ls())
wage12 <- read.csv("output12_update.csv",header=TRUE)

wage12_age <- lm(LNWAGE~AGE,data=wage12)
summary(wage12_age)
wage12_educ <- lm(LNWAGE~EDUC,data=wage12)
summary(wage12_educ)
wage12_potexp <- lm(LNWAGE~EXPER,data=wage12)
summary(wage12_potexp)

wage12_age_educ <- lm(LNWAGE~AGE+EDUC,data=wage12)
summary(wage12_age_educ)
wage12_educ_exper <- lm(LNWAGE~EDUC+EXPER,data=wage12)
summary(wage12_educ_exper)
wage12_age_exper <- lm(LNWAGE~AGE+EXPER,data=wage12)
summary(wage12_age_exper)

wage12_union <- lm(LNWAGE~UNION,data=wage12)
summary(wage12_union)
wage12_female <- lm(LNWAGE~FEMALE,data=wage12)
summary(wage12_female)
wage12_nonwhite <- lm(LNWAGE~NONWHITE,data=wage12)
summary(wage12_nonwhite)

wage12_union_female <- lm(LNWAGE~UNION+FEMALE,data=wage12)
summary(wage12_union_female)
wage12_union_nonwhite <- lm(LNWAGE~UNION+NONWHITE,data=wage12)
summary(wage12_union_nonwhite)
wage12_female_nonwhite <- lm(LNWAGE~FEMALE+NONWHITE,data=wage12)
summary(wage12_female_nonwhite)

wage12_indicatorvars <- lm(LNWAGE~UNION+FEMALE+NONWHITE,data=wage12)
summary(wage12_indicatorvars)

wage12_indicatorvars_age <- lm(LNWAGE~UNION+FEMALE+NONWHITE+AGE,data=wage12)
summary(wage12_indicatorvars_age)
wage12_indicatorvars_potexp <- lm(LNWAGE~UNION+FEMALE+NONWHITE+EXPER,data=wage12)
summary(wage12_indicatorvars_potexp)
wage12_indicatorvars_educ <- lm(LNWAGE~UNION+FEMALE+NONWHITE+EDUC,data=wage12)
summary(wage12_indicatorvars_educ)

wage12_age_educ_union <- lm(LNWAGE~AGE+EDUC+UNION,data=wage12)
summary(wage12_age_educ_union)
wage12_age_educ_female <- lm(LNWAGE~AGE+EDUC+FEMALE,data=wage12)
summary(wage12_age_educ_female)
wage12_age_educ_nonwhite <- lm(LNWAGE~AGE+EDUC+NONWHITE,data=wage12)
summary(wage12_age_educ_nonwhite)

wage12_age_educ_union_female <- lm(LNWAGE~AGE+EDUC+UNION+FEMALE,data=wage12)
summary(wage12_age_educ_union_female)
wage12_age_educ_union_nonwhite <- lm(LNWAGE~AGE+EDUC+UNION+NONWHITE,data=wage12)
summary(wage12_age_educ_union_nonwhite)
wage12_age_educ_female_nonwhite <- lm(LNWAGE~AGE+EDUC+FEMALE+NONWHITE,data=wage12)
summary(wage12_age_educ_female_nonwhite)

wage12_educ_exper_union <- lm(LNWAGE~EDUC+EXPER+UNION,data=wage12)
summary(wage12_educ_exper_union)
wage12_educ_exper_female <- lm(LNWAGE~EDUC+EXPER+FEMALE,data=wage12)
summary(wage12_educ_exper_female)
wage12_educ_exper_nonwhite <- lm(LNWAGE~EDUC+EXPER+NONWHITE,data=wage12)
summary(wage12_educ_exper_nonwhite)

wage12_educ_exper_union_female <- lm(LNWAGE~EDUC+EXPER+UNION+FEMALE,data=wage12)
summary(wage12_educ_exper_union_female)
wage12_educ_exper_union_nonwhite <- lm(LNWAGE~EDUC+EXPER+UNION+NONWHITE,data=wage12)
summary(wage12_educ_exper_union_nonwhite)
wage12_educ_exper_female_nonwhite <- lm(LNWAGE~EDUC+EXPER+FEMALE+NONWHITE,data=wage12)
summary(wage12_educ_exper_female_nonwhite)

wage12_age_exper_union <- lm(LNWAGE~AGE+EXPER+UNION,data=wage12)
summary(wage12_age_exper_union)
wage12_age_exper_female <- lm(LNWAGE~AGE+EXPER+FEMALE,data=wage12)
summary(wage12_age_exper_female)
wage12_age_exper_nonwhite <- lm(LNWAGE~AGE+EXPER+NONWHITE,data=wage12)
summary(wage12_age_exper_nonwhite)

wage12_age_exper_union_female <- lm(LNWAGE~AGE+EXPER+UNION+FEMALE,data=wage12)
summary(wage12_age_exper_union_female)
wage12_age_exper_union_nonwhite <- lm(LNWAGE~AGE+EXPER+UNION+NONWHITE,data=wage12)
summary(wage12_age_exper_union_nonwhite)
wage12_age_exper_female_nonwhite <- lm(LNWAGE~AGE+EXPER+FEMALE+NONWHITE,data=wage12)
summary(wage12_age_exper_female_nonwhite)

wage12_fullreg <- lm(LNWAGE~AGE+EXPER+UNION+FEMALE+NONWHITE,data=wage12)
summary(wage12_fullreg)
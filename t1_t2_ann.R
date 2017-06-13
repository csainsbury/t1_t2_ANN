library(data.table)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

findTimeToNearestHbA1c <- function(dmList_LinkId, diagnosisDate_unix) {
  
  hb_id_sub <- cleanHbA1cDataDT[LinkId == dmList_LinkId]
  diffFromDiagnosisDate <- hb_id_sub$dateplustime1 - diagnosisDate_unix
  
  findClosest <- sqrt(diffFromDiagnosisDate ^ 2)
  flagClosest <- ifelse(findClosest == min(findClosest), 1, 0)
  
  outputList <- list(findClosest[flagClosest == 1], hb_id_sub$hba1cNumeric[flagClosest == 1])
  
  return(outputList)
  
}

# set index date
index <- "2017-01-01"
paramFromDiagnosisWindowMonths = 12
paramFromDiagnosisWindowSeconds = paramFromDiagnosisWindowMonths * (60*60*24*(365.25/12))

# diagnosisDataset<-read.csv("../GlCoSy/SDsource/diagnosisDateDeathDate.txt")
diagnosisDataset<-read.csv("~/R/GlCoSy/SDsource/demogALL.txt", quote = "", 
                           row.names = NULL, 
                           stringsAsFactors = FALSE)

diagnosisDatasetDT = data.table(diagnosisDataset)

  # cut down dataset
  cut_diagDT <- data.table(diagnosisDatasetDT$LinkId, diagnosisDatasetDT$DateOfDiagnosisDiabetes_Date, diagnosisDatasetDT$Ethnicity_Mapped, diagnosisDatasetDT$CurrentGender_Mapped, diagnosisDatasetDT$DiabetesMellitusType_Mapped, diagnosisDatasetDT$BirthDate)
  colnames(cut_diagDT) <- c("LinkId", "diagnosisDate", "Ethnicity", "Sex", "diabetesType", "DOB")
  cut_diagDT$LinkId <- as.numeric(cut_diagDT$LinkId)
  cut_diagDT$LinkId <- as.numeric(cut_diagDT$LinkId)
  cut_diagDT$DOB_unix <- returnUnixDateTime(cut_diagDT$DOB)
  
  # remove those without a diagnosis date
  cut_diagDT$diagnosisDate_unix <- returnUnixDateTime(cut_diagDT$diagnosisDate)
  cut_diagDT$diagnosisDate_unix[is.na(cut_diagDT$diagnosisDate_unix)] <- 0
  cut_diagDT <- cut_diagDT[diagnosisDate_unix > 0]
  
  # cut to T1 or T2
  cut_diagDT <- cut_diagDT[diabetesType == "Type 1 Diabetes Mellitus" | diabetesType == "Type 2 Diabetes Mellitus"]
  
  # cut to ensure at least 1y data for each
  cut_diagDT <- cut_diagDT[diagnosisDate_unix < (returnUnixDateTime(index) - (60*60*24*365.25))]

# generate node and link files
cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanHbA1cData$timeSeriesDataPoint <- cleanHbA1cData$hba1cNumeric
  cleanHbA1cDataDT <- data.table(cleanHbA1cData)
  hb_DT_forMerge <- data.table(cleanHbA1cDataDT$LinkId, cleanHbA1cDataDT$dateplustime1, cleanHbA1cDataDT$hba1cNumeric)
  colnames(hb_DT_forMerge) <- c("LinkId", "hb_dateplustime1", "hba1cNumeric")

cleanSBPData <- read.csv("~/R/GlCoSy/SD_workingSource/SBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanSBPData$timeSeriesDataPoint <- cleanSBPData$sbpNumeric
  cleanSBPDataDT <- data.table(cleanSBPData)
  sbp_DT_forMerge <- data.table(cleanSBPDataDT$LinkId, cleanSBPDataDT$dateplustime1, cleanSBPDataDT$sbpNumeric)
  colnames(sbp_DT_forMerge) <- c("LinkId", "sbp_dateplustime1", "sbpNumeric")
  
cleanDBPData <- read.csv("~/R/GlCoSy/SD_workingSource/DBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
cleanDBPData$timeSeriesDataPoint <- cleanDBPData$dbpNumeric
  cleanDBPDataDT <- data.table(cleanDBPData)
  dbp_DT_forMerge <- data.table(cleanDBPDataDT$LinkId, cleanDBPDataDT$dateplustime1, cleanDBPDataDT$dbpNumeric)
  colnames(dbp_DT_forMerge) <- c("LinkId", "dbp_dateplustime1", "dbpNumeric")

cleanBMIData <- read.csv("~/R/GlCoSy/SD_workingSource/BMISetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  cleanBMIDataDT <- data.table(cleanBMIData)
  bmi_DT_forMerge <- data.table(cleanBMIDataDT$LinkId, cleanBMIDataDT$dateplustime1, cleanBMIDataDT$bmiNumeric)
  colnames(bmi_DT_forMerge) <- c("LinkId", "bmi_dateplustime1", "bmiNumeric")
  
cleanRenalData <- read.csv("~/R/GlCoSy/SD_workingSource/renalSetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  cleanRenalDataDT <- data.table(cleanRenalData)
  renal_DT_forMerge <- data.table(cleanRenalDataDT$LinkId, cleanRenalDataDT$dateplustime1, cleanRenalDataDT$egfrNumeric)
  colnames(renal_DT_forMerge) <- c("LinkId", "egfr_dateplustime1", "egfrNumeric")


# find closest value to diagnosis date for each parameter - sequential merge

# hba1c
merge_hb <- merge(hb_DT_forMerge, cut_diagDT, by = "LinkId")
merge_hb[, c("hb_diffFromDiag") := sqrt((hb_dateplustime1 - diagnosisDate_unix) ^ 2) , by=.(LinkId)]
merge_hb[, c("hb_flagClosest") := ifelse(hb_diffFromDiag == min(hb_diffFromDiag), 1, 0) , by=.(LinkId)]

merge_hb <- merge_hb[(hb_diffFromDiag < paramFromDiagnosisWindowSeconds) & hb_flagClosest == 1]

# SBP
merge_sbp <- merge(merge_hb, sbp_DT_forMerge, by = "LinkId")
merge_sbp[, c("sbp_diffFromDiag") := sqrt((sbp_dateplustime1 - diagnosisDate_unix) ^ 2) , by=.(LinkId)]
merge_sbp[, c("sbp_flagClosest") := ifelse(sbp_diffFromDiag == min(sbp_diffFromDiag), 1, 0) , by=.(LinkId)]

merge_sbp <- merge_sbp[(sbp_diffFromDiag < paramFromDiagnosisWindowSeconds) & sbp_flagClosest == 1]

# DBP
merge_dbp <- merge(merge_sbp, dbp_DT_forMerge, by = "LinkId")
merge_dbp[, c("dbp_diffFromDiag") := sqrt((dbp_dateplustime1 - diagnosisDate_unix) ^ 2) , by=.(LinkId)]
merge_dbp[, c("dbp_flagClosest") := ifelse(dbp_diffFromDiag == min(dbp_diffFromDiag), 1, 0) , by=.(LinkId)]

merge_dbp <- merge_dbp[(dbp_diffFromDiag < paramFromDiagnosisWindowSeconds) & dbp_flagClosest == 1]

# BMI
merge_bmi <- merge(merge_dbp, bmi_DT_forMerge, by = "LinkId")
merge_bmi[, c("bmi_diffFromDiag") := sqrt((bmi_dateplustime1 - diagnosisDate_unix) ^ 2) , by=.(LinkId)]
merge_bmi[, c("bmi_flagClosest") := ifelse(bmi_diffFromDiag == min(bmi_diffFromDiag), 1, 0) , by=.(LinkId)]

merge_bmi <- merge_bmi[(bmi_diffFromDiag < paramFromDiagnosisWindowSeconds) & bmi_flagClosest == 1]

# renal
merge_renal <- merge(merge_bmi, renal_DT_forMerge, by = "LinkId")
merge_renal[, c("egfr_diffFromDiag") := sqrt((egfr_dateplustime1 - diagnosisDate_unix) ^ 2) , by=.(LinkId)]
merge_renal[, c("egfr_flagClosest") := ifelse(egfr_diffFromDiag == min(egfr_diffFromDiag), 1, 0) , by=.(LinkId)]

merge_renal <- merge_renal[(egfr_diffFromDiag < paramFromDiagnosisWindowSeconds) & egfr_flagClosest == 1]




# finalset
diagnostic_test_set <- merge_bmi
diagnostic_test_set$Sex <- ifelse(diagnostic_test_set$Sex == "Male", 1, 0)
diagnostic_test_set$diabetesType <- ifelse(diagnostic_test_set$diabetesType == "Type 1 Diabetes Mellitus", 1, 0)

diagnostic_test_set <- diagnostic_test_set[substr(Ethnicity,1,3) != "ECD"]
  factorEthnicity <- factor(diagnostic_test_set$Ethnicity)
  diagnostic_test_set$Ethnicity <- as.numeric(factorEthnicity)


diagnostic_test_set$ageAtDiagnosis <- (diagnostic_test_set$diagnosisDate_unix - diagnostic_test_set$DOB_unix) / (60*60*24*365.25)

diagnostic_test_set <- data.table(diagnostic_test_set$ageAtDiagnosis, diagnostic_test_set$Ethnicity, diagnostic_test_set$Sex, diagnostic_test_set$hba1cNumeric, diagnostic_test_set$sbpNumeric, diagnostic_test_set$dbpNumeric,  diagnostic_test_set$bmiNumeric, diagnostic_test_set$diabetesType)

colnames(diagnostic_test_set) <- c("age", "ethnicity", "sex", "hba1c", "sbp", "dbp", "bmi", "diabetesType")

write.table(diagnostic_test_set, file = "~/R/_workingDirectory/t1_t2_ANN/diagSet_7p.csv", sep = ",", row.names = FALSE, col.names = TRUE)

## read in csv output from ann
## turn ethnicity levels into readable output for physician interpretation

levelKey <- levels(factorEthnicity)

X_test_RN <- read.csv("~/R/_workingDirectory/t1_t2_ANN/output/X_test_realNumbers.csv", sep=",", header = FALSE, row.names = NULL)
y_test    <- read.csv("~/R/_workingDirectory/t1_t2_ANN/output/y_test.csv", sep=",", header = FALSE, row.names = NULL)
y_pred    <- read.csv("~/R/_workingDirectory/t1_t2_ANN/output/y_pred.csv", sep=",", header = FALSE, row.names = NULL)









# cut_diagDT[, c("timeToHbA1c", "HbA1c_value") := findTimeToNearestHbA1c(LinkId, diagnosisDate_unix) , by=.(LinkId)]

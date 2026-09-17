# Analyze South Delta barriers project simulations
#
# Doug Jackson
# doug@QEDAconsulting.com
library(tidyverse)
library(ggplot2)
library(lubridate)

####################################################################################################
# Constants
####################################################################################################
workingDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

dataDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/fromXiao/studies_13sep26"

insertionLocsFile = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb/insertionLocs.csv"

# Number of resamples
N <- 10000

figWidth <- 7
figHeight <- 7
####################################################################################################
# Functions
####################################################################################################
readSalmon <- function(files) {
    datList <- list()
    for(f in files) {
        thisDat <- read.csv(f)
        
        datList[[length(datList)+1]] <- thisDat
    }
    dat <- bind_rows(datList)
    
    return(dat)
}

analyzeSalmon <- function(dF, loc) {
    
    dF <- dF |> mutate(Date=dmy(Date), ptm_start_date=dmy(ptm_start_date), 
                                             first_release_date=dmy(first_release_date), last_release_date=dmy(last_release_date),
                                             scenario=ifelse(scenario=="D-GO-BSL-10yr-b_salmon", "baseline", "preferred"))
    
    dF <- dF |> select(first_release_date, scenario:overall)
    
    vars <- names(dF)[which(!(names(dF) %in% c("first_release_date", "scenario")))]
    
    for(var in vars) {
        
        thisDF <- dF[, c("first_release_date", "scenario", var)]
        names(thisDF) <- c("first_release_date", "scenario", "var")
        
        thisDFwide <- thisDF |> pivot_wider(id_cols=first_release_date, names_from=scenario, values_from=var) |> 
            mutate(year=as.factor(year(first_release_date)), month=as.factor(month(first_release_date, label=T)), julianDay=yday(first_release_date),
                   diff=preferred-baseline)
        
        varType <- ifelse(grepl("frac", var), "routing", "survival")
        
        thisOutputDir <- file.path(outputDir, loc, varType)
        dir.create(thisOutputDir, recursive=T, showWarnings=F)
        
        p <- ggplot(thisDF) + geom_line(aes(x=first_release_date, y=var)) +
            facet_wrap(~scenario, ncol=1) +
            labs(title=paste0(var, ", ", loc), x="", y=varType) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("timeSeries_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        
        p <- ggplot(thisDFwide) + geom_point(aes(x=julianDay, y=diff, color=month), size=0.25) +
            facet_wrap(~year, ncol=1, scales="free_y") +
            labs(title=paste0("Difference in ", var, ", ", loc), x="Julian day", y=paste(varType, "difference (preferred - baseline)")) +
            guides(color=guide_legend(override.aes=list(size=2))) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("diff_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        
        p <- ggplot(thisDFwide) + geom_point(aes(x=baseline, y=preferred, group=month, color=month), alpha=0.25) +
            geom_abline(intercept=0, slope=1, color="red") +
            facet_wrap(~month, ncol=1) +
            labs(title=paste(var, "comparison,", loc), x=paste0(varType, ", baseline scenario"), y=paste0(varType, ", preferred alternative")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("compare_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        
        ####################################################################################################
        # Boxplots with significance
        # From Wikipedia: "The Wilcoxon test is a good alternative to the t-test when the normal distribution of the 
        # differences between paired individuals cannot be assumed. Instead, it assumes a weaker hypothesis that the 
        # distribution of this difference is symmetric around a central value and it aims to test whether this center 
        # value differs significantly from zero."
        sigs <- thisDFwide |> group_by(month) |> filter(n_distinct(diff)>1) |> 
            summarize(pValResampleMean=resampleDiff(diff),
                      pValResampleMed=resampleMedDiff(diff),
                      pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                      pValShapiro=shapiro.test(diff)$p.value,
                      pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value)
        
        sigMonths <- sigs |> filter(pWilcox<0.05)
        sigMonths <- sigMonths$month
        
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=month, y=diff)) + 
            annotate("text", x=sigMonths, y=max(thisDFwide$diff)*1.05, label="*", size=8, color="blue") +
            labs(title=paste(var, "comparison,", loc), x="month", y=paste(varType, "difference (preferred - baseline)")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        cat("--------------------------------------------------------------------\n")
        cat(paste0(var, ", ", loc, "\n"))
        print(sigs)
        
    }
    
    return(list(dF=dF))
}

readParticles <- function(files) {
    datList <- list()
    for(f in files) {
        thisScen <- gsub(".dat", "", basename(f))
        if(thisScen=="np_90day_D-GO-sdg9b") {
            thisDat <- read.table(f, skip=1, sep="\t", header=T)
        } else {
            thisDat <- read.csv(f, skip=1, header=T)
        }
        
        thisDat$scenario <- thisScen
        thisDat$startDate <- dmy(thisDat$SimPeriod)
        
        datList[[length(datList)+1]] <- thisDat
    }
    dat <- bind_rows(datList)
}

analyzeParticles <- function(dF, type) {
    
    yLab <- "90-day flux"
    
    dF <- dF |> mutate(scenario=ifelse(scenario %in% c("np_90day_D-GO-BSL-10yr-b", "sp_90day_D-GO-BSL-10yr-b"), "baseline", "preferred")) |> 
        select(startDate, SimLoc:scenario)
    
    vars <- names(dF)[which(!(names(dF) %in% c("startDate", "SimLoc", "scenario")))]
    
    locs <- unique(dF$SimLoc)
    
    typeStr <- ifelse(type=="neutrallyBuoyant", "neutrally buoyant", "surface-oriented")
    
    for(var in vars) {
        
        thisOutputDir <- file.path(outputDir, type, var)
        dir.create(thisOutputDir, recursive=T, showWarnings=F)
        
        for(releaseLoc in locs) {
            thisDF <- dF |> filter(SimLoc==releaseLoc)
            thisDF <- thisDF[, c("startDate", "scenario", var)]
            names(thisDF) <- c("startDate", "scenario", "var")
            
            if(sum(!is.na(thisDF$var))==0 | sum(thisDF$var!=0)==0) {next}
            
            thisInsertionLoc <- insertionLocs |> filter(loc==releaseLoc)
            thisInsertionNode <- thisInsertionLoc$node
            thisInsertionDesc <- thisInsertionLoc$name
            
            thisDFwide <- thisDF |> pivot_wider(id_cols=startDate, names_from=scenario, values_from=var) |> 
                mutate(year=as.factor(year(startDate)), month=as.factor(month(startDate)), julianDay=yday(startDate),
                       diff=preferred-baseline)
            
            p <- ggplot(thisDF) + geom_point(aes(x=startDate, y=var)) +
                facet_wrap(~scenario, ncol=1) +
                labs(title=paste0(var, ", release node ", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="", y=yLab) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("timeSeries_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)
            
            p <- ggplot(thisDFwide) + geom_point(aes(x=year, y=diff, color=month), size=2) +
                facet_wrap(~month, ncol=1, scales="free_y") +
                labs(title=paste0("Difference in ", var, ", release node ", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="Julian day", y=paste("90-day flux difference (preferred - baseline)")) +
                guides(color=guide_legend(override.aes=list(size=2))) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("diff_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)
            
            p <- ggplot(thisDFwide) + geom_point(aes(x=baseline, y=preferred, group=month, color=month), size=2, alpha=1) +
                geom_abline(intercept=0, slope=1, color="red") +
                facet_wrap(~month, ncol=1) +
                labs(title=paste(var, "comparison, release node", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="90-day flux, baseline scenario", y=paste0("90-day flux, preferred alternative")) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("compare_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)

            ####################################################################################################
            # Boxplots with significance
            # From Wikipedia: "The Wilcoxon test is a good alternative to the t-test when the normal distribution of the 
            # differences between paired individuals cannot be assumed. Instead, it assumes a weaker hypothesis that the 
            # distribution of this difference is symmetric around a central value and it aims to test whether this center 
            # value differs significantly from zero."
            sigs <- thisDFwide |> group_by(month) |> filter(n_distinct(diff)>1) |> 
                summarize(pValResampleMean=resampleDiff(diff),
                          pValResampleMed=resampleMedDiff(diff),
                          pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                          pValShapiro=shapiro.test(diff)$p.value,
                          pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value)
            
            sigMonths <- sigs |> filter(pWilcox<0.05)
            sigMonths <- sigMonths$month
            
            p <- ggplot(thisDFwide) + geom_boxplot(aes(x=month, y=diff)) + 
                annotate("text", x=sigMonths, y=max(thisDFwide$diff)*1.05, label="*", size=8, color="blue") +
                labs(title=paste(var, "comparison, release node", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="month", y=paste("90-day flux difference (preferred - baseline)")) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("boxPlot_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)
            cat("--------------------------------------------------------------------\n")
            cat(paste0(var, ", ", releaseLoc, "\n"))
            print(sigs)
        }
        
        # Plot all release locations together
        thisDF <- dF[, c("startDate", "SimLoc", "scenario", var)]
        names(thisDF) <- c("startDate", "SimLoc", "scenario", "var")
        thisDF <- left_join(thisDF, insertionLocs, by=c("SimLoc"="loc"))
        
        if(sum(!is.na(thisDF$var))==0 | sum(thisDF$var!=0)==0) {next}
        
        thisDFwide <- thisDF |> pivot_wider(id_cols=c("startDate", "SimLoc", "node", "name"), names_from=scenario, values_from=var) |> 
            mutate(month=as.factor(month(startDate, label=T)), diff=preferred-baseline, node=as.factor(node))
        
        sigs <- thisDFwide |> group_by(node) |> filter(n_distinct(diff)>1) |> 
            summarize(pValResampleMean=resampleDiff(diff),
                      pValResampleMed=resampleMedDiff(diff),
                      pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                      pValShapiro=shapiro.test(diff)$p.value,
                      pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value)
        
        sigNodes <- sigs |> filter(pWilcox<0.05)
        sigNodes <- sigNodes$node
        
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=node, y=diff)) +
            annotate("text", x=sigNodes, y=max(thisDFwide$diff)*1.05, label="*", size=8, color="blue") +
            labs(title=paste(var, "comparison,", typeStr), x="release node", y="90-day flux difference (preferred - baseline)") +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_", var, ".png")), width=figWidth, height=figHeight)
        
        sigs <- thisDFwide |> group_by(node, month) |> filter(n_distinct(diff)>1) |> 
            summarize(pValResampleMean=resampleDiff(diff),
                      pValResampleMed=resampleMedDiff(diff),
                      pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                      pValShapiro=shapiro.test(diff)$p.value,
                      pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value, 
                      .groups="drop")
        
        sigs <- sigs |> filter(pWilcox<0.05)
        
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=node, y=diff)) +
            facet_wrap(~month, ncol=1) +
            geom_text(data=sigs, aes(x=node, y=max(thisDFwide$diff)*0.9, label="*"), size=8, color="blue") +
            labs(title=paste(var, "comparison,", typeStr), x="release node", y="90-day flux difference (preferred - baseline)") +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_byMonth_", var, ".png")), width=6, height=8)
        
        p <- ggplot(thisDFwide) + geom_point(aes(x=baseline, y=preferred, color=node, group=node)) + 
            geom_abline(slope=1, intercept=0, color="red") + 
            #xlim(0, 100) + ylim(0, 100) +
            facet_wrap(~node, ncol=1) +
            labs(title=paste(var, "comparison, ", typeStr), subtitle=thisInsertionDesc,
                 x="90-day flux, baseline scenario", y=paste0("90-day flux, preferred alternative")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("compare_", var, ".png")), width=figWidth, height=figHeight)
    }
    
    return(list(dF=dF))
}

resampleDiff <- function(diffs) {
    
    origDiffs <- diffs
    numDiffs <- length(diffs)
    
    meanDiffs <- as.numeric(N)
    for(j in 1:N) {
        thisDiffs <- origDiffs * sample(c(-1, 1), numDiffs, replace=T)
        meanDiffs[j] <- mean(thisDiffs, na.rm=T)
    }
    nVal <- sum(abs(meanDiffs)>abs(mean(origDiffs, na.rm=T)))/N
    
    return(nVal)
}

resampleMedDiff <- function(diffs) {
    
    origDiffs <- diffs
    numDiffs <- length(diffs)
    
    medDiffs <- as.numeric(N)
    for(j in 1:N) {
        thisDiffs <- origDiffs * sample(c(-1, 1), numDiffs, replace=T)
        medDiffs[j] <- median(thisDiffs, na.rm=T)
    }
    nVal <- sum(abs(medDiffs)>abs(median(origDiffs, na.rm=T)))/N
    
    return(nVal)
}
####################################################################################################
# Run
####################################################################################################
setwd(workingDir)

outputDir <- file.path(workingDir, "output_analyze")
dir.create(outputDir, showWarnings=F)

####################################################################################################
# salmon
northDeltaFiles <- list.files(dataDir, pattern="salmon_nd*", full.names=T)
southDeltaFiles <- list.files(dataDir, pattern="salmon_sd*", full.names=T)

northDelta <- readSalmon(northDeltaFiles)
out <- analyzeSalmon(northDelta, "northDelta")

southDelta <- readSalmon(southDeltaFiles)
out <- analyzeSalmon(southDelta, "southDelta")

####################################################################################################
# neutrally buoyant and surface-oriented particles
insertionLocs <- read.csv(insertionLocsFile)

npFiles <- list.files(dataDir, pattern="np_*", full.names=T)
spFiles <- list.files(dataDir, pattern="sp_*", full.names=T)

np <- readParticles(npFiles)
out <- analyzeParticles(np, "neutrallyBuoyant")

sp <- readParticles(spFiles)
out <- analyzeParticles(sp, "surfaceOriented")

---
title: "R Notebook for effects of chrysin and flavone on zebrafish anxiety-like behavior (LaNeC)"
author:
- Caio Maximino^[Universidade Federal do Sul e Sudeste do Pará]
- Monica Gomes Lima^[Universidade do Estado do Pará]
- León German Ponciano^[Universidad Veracruzana]
- Juan Francisco Rodriguez-Landa
output:
  github_document 
subtitle: From project "Experimental studies on behavioral pharmacology and molecular biology of active ingredients of vegetable extracts with potential anxiolytic and antidepressant activity" (ESTUDIOS EXPERIMENTALES DE FARMACOLOGÍA CONDUCTUAL Y BIOLOGÍA MOLECULAR DE PRINCIPIOS ACTIVOS VEGETALES CON ACTIVIDAD ANSIOLÍTICA Y ANTIDEPRESIVA.)
tags:
- anxiety
- zebrafish
- rats
- flavonoids
abstract: |
  Datapackages and analysis scripts for the project "Experimental studies on behavioral pharmacology and molecular biology of active ingredients of vegetable extracts with potential anxiolytic and antidepressant activity"
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the data analysis of the research project Datapackages and analysis scripts for the project "Experimental studies on behavioral pharmacology and molecular biology of active ingredients of vegetable extracts with potential anxiolytic and antidepressant activity".

Data is produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará, in collaboration with members of Laboratorio de Neurofarmacología, affiliated to Universidad Veracruzana. The package will include primary data for a behavioral experiment on the effects of chrysin and flavone on rat and zebrafish anxiety-like behavior.

When you execute code within the notebook, the results appear beneath the code. 

* Load needed libraries:
```{r}
if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
}

if(!require(gridExtra)){
    install.packages("gridExtra")
    library(gridExtra)
}

if(!require(RCurl)){
    install.packages("RCurl")
    library(RCurl)
}

if(!require(coin)){
    install.packages("coin")
    library(coin)}

if(!require(FSA)){
    install.packages("FSA")
    library(FSA)
}

if(!require(rcompanion)){
    install.packages("rcompanion")
    library(rcompanion)
}

if(!require(multcompView)){
    install.packages("multcompView")
    library(multcompView)
}
```
A) Zebrafish

* Load data
```{r}
x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/chrysin/master/chrysinLDT.csv")
zebrafish <- read.csv(text = x1)
zebrafish$Treatment <- as.factor(zebrafish$Treatment)
View(zebrafish)
```

* Set seed to ensure reproducibility
```{r}
set.seed(42)
```

* Order factors, otherwise R will alphabetize them
```{r}
zebrafish$Treatment = factor(zebrafish$Treatment, ordered=FALSE, levels=unique(zebrafish$Treatment))
```

* Detect outliers using median ± 3 MADs; remove if needed
```{r}
gA <- zebrafish[c(1:12),]
median(gA$Time.on.white) + 3*mad(gA$Time.on.white)
median(gA$Time.on.white) - 3*mad(gA$Time.on.white)
gB <- zebrafish[c(13:23),]
median(gB$Time.on.white) + 3*mad(gB$Time.on.white)
median(gB$Time.on.white) - 3*mad(gB$Time.on.white)
gC <- zebrafish[c(24:35),]
median(gC$Time.on.white) + 3*mad(gC$Time.on.white)
median(gC$Time.on.white) - 3*mad(gC$Time.on.white)
gD <- zebrafish[c(26:47),]
median(gD$Time.on.white) + 3*mad(gD$Time.on.white)
median(gD$Time.on.white) - 3*mad(gD$Time.on.white)
#Animal B12 from group Flavone identified as outlier; remove from data frame
zebrafish_no.outlier <- zebrafish[-c(23), ]
View(zebrafish_no.outlier)
```

* Run ANOVAs with permutation tests for main effects (based on https://rcompanion.org/rcompanion/d_06a.html)

1) Time on white
1.1) Main test
```{r}
independence_test(Time.on.white ~ Treatment, data = zebrafish_no.outlier)
```

1.2) Produce post-hoc tests for main effects
```{r}
PT_TimeOnWhite = pairwisePermutationTest(Time.on.white ~ Treatment, data = zebrafish_no.outlier, method="fdr")
PT_TimeOnWhite
```

1.3) Draw plot
```{r}
TW_plot <- ggplot(zebrafish_no.outlier, aes(x = factor(Treatment), y = Time.on.white, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 280, label = "a") + annotate("text", x = 2, y = 750, label = "ab") + annotate("text", x = 3, y = 930, label = "b") +  annotate("text", x = 4, y = 80, label = "c") + labs(x = "Treatment", y = "Time on white (s)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 900, label = "B", size = 6)
TW_plot
```

2) Entry duration
2.1) Main test
```{r}
independence_test(Duration ~ Treatment, data = zebrafish_no.outlier)
```

2.2) Draw plot
```{r}
Dur_plot <- ggplot(zebrafish_no.outlier, aes(x = factor(Treatment), y = Duration, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 250, label = "a") + annotate("text", x = 2, y = 350, label = "a") + annotate("text", x = 3, y = 930, label = "a") + annotate("text", x = 4, y = 60, label = "a") + labs(x = "Treatment", y = "Entry duration (s/N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 900, label = "C", size = 6)
Dur_plot
```

3) Transitions to white
3.1) Main test
```{r}
independence_test(Entries ~ Treatment, data = zebrafish_no.outlier)
```

3.2) Draw plot
```{r}
Trans_plot <- ggplot(zebrafish_no.outlier, aes(x = factor(Treatment), y = Entries, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 100, label = "a") + annotate("text", x = 2, y = 100, label = "a") + annotate("text", x = 3, y = 110, label = "a") + annotate("text", x = 4, y = 50, label = "a") + labs(x = "Treatment", y = "Transitions to white (N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 100, label = "D", size = 6)
Trans_plot
```

4) Risk assessment
4.1) Main test
```{r}
independence_test(RA ~ Treatment, data = zebrafish_no.outlier)
```

4.2) Produce post-hoc tests for main effects
```{r}
PT_RA = pairwisePermutationTest(RA ~ Treatment, data = zebrafish_no.outlier, method="fdr")
PT_RA
```

4.3) Draw plot
```{r}
RA_plot <- ggplot(zebrafish_no.outlier, aes(x = factor(Treatment), y = RA, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 17, label = "a") + annotate("text", x = 2, y = 10, label = "b") + annotate("text", x = 3, y = 17, label = "ab") + annotate("text", x = 4, y = 10, label = "ab") + labs(x = "Treatment", y = "Risk assessment (N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 15, label = "E", size = 6)
RA_plot
```

5) Erratic swimming
5.1) Main test
```{r}
independence_test(NE ~ Treatment, data = zebrafish_no.outlier)
```

5.2) Produce post-hoc tests for main effects
```{r}
PT_NE = pairwisePermutationTest(NE ~ Treatment, data = zebrafish_no.outlier, method="fdr")
PT_NE
```

5.3) Draw plot
```{r}
ES_plot <- ggplot(zebrafish_no.outlier, aes(x = factor(Treatment), y = NE, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 15, label = "a") + annotate("text", x = 2, y = 5, label = "a") + annotate("text", x = 3, y = 10, label = "a") + annotate("text", x = 4, y = 10, label = "b") + labs(x = "Treatment", y = "Erratic swimming (N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 14, label = "F", size = 6)
ES_plot
```

6) Thigmotaxis
6.1) Main test
```{r}
independence_test(Thigmotaxis ~ Treatment, data = zebrafish_no.outlier)
```

6.2) Draw plot
```{r}
Thigmo_plot <- ggplot(zebrafish_no.outlier, aes(x = factor(Treatment), y = Thigmotaxis, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 50, label = "a") + annotate("text", x = 2, y = 42, label = "a") + annotate("text", x = 3, y = 65, label = "a") + annotate("text", x = 4, y = 12, label = "a") + labs(x = "Treatment", y = "Thigmotaxis (s)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 60, label = "G", size = 6)
Thigmo_plot
```

7) Freezing
6.1) Main test
```{r}
independence_test(Freezing ~ Treatment, data = zebrafish_no.outlier)
```

6.2) Draw plot
```{r}
Freezing_plot <- ggplot(zebrafish_no.outlier, aes(x = factor(Treatment), y = Freezing, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 200, label = "a") + annotate("text", x = 2, y = 180, label = "a") + annotate("text", x = 3, y = 930, label = "a") + annotate("text", x = 4, y = 100, label = "a") + labs(x = "Treatment", y = "Freezing (s)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 900, label = "H", size = 6)
Freezing_plot
```
* Join plots in panels
```{r}
grid.arrange(TW_plot, Dur_plot, Trans_plot, RA_plot, ES_plot, Thigmo_plot, Freezing_plot, nrow = 4)
```

B) Rats
* Load data
```{r}
x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/chrysin/master/chrysin-EPM_OFT.csv")
rat <- read.csv(text = x1)
rat$Treatment <- as.factor(rat$Treatment)
View(rat)
```

* Set seed to ensure reproducibility
```{r}
set.seed(42)
```

* Order factors, otherwise R will alphabetize them
```{r}
rat$Treatment = factor(rat$Treatment, ordered=FALSE, levels=unique(rat$Treatment))
```

* Detect outliers using median ± 3 MADs; remove if needed
```{r}
gA <- rat[c(1:8),]
median(gA$Time_OpenArms) + 3*mad(gA$Time_OpenArms)
median(gA$Time_OpenArms) - 3*mad(gA$Time_OpenArms)
gB <- rat[c(9:16),]
median(gB$Time_OpenArms) + 3*mad(gB$Time_OpenArms)
median(gB$Time_OpenArms) - 3*mad(gB$Time_OpenArms)
gC <- rat[c(17:24),]
median(gC$Time_OpenArms) + 3*mad(gC$Time_OpenArms)
median(gC$Time_OpenArms) - 3*mad(gC$Time_OpenArms)
gD <- rat[c(25:32),]
median(gD$Time_OpenArms) + 3*mad(gD$Time_OpenArms)
median(gD$Time_OpenArms) - 3*mad(gD$Time_OpenArms)
#No outliers
```

* Run ANOVAs with permutation tests for main effects (based on https://rcompanion.org/rcompanion/d_06a.html) for the elevated plus-maze
1) Entries on open arms
1.1) Main test
```{r}
independence_test(Entries_OpenArms ~ Treatment, data = rat)
```

1.2) Produce post-hoc tests for main effects
```{r}
PT_EntriesOA = pairwisePermutationTest(Entries_OpenArms ~ Treatment, data = rat, method="fdr")
PT_EntriesOA
```

1.3) Draw plot
```{r}
EntriesOA_plot <- ggplot(rat, aes(x = factor(Treatment), y = Entries_OpenArms, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 9, label = "a") + annotate("text", x = 2, y = 11, label = "b") + annotate("text", x = 3, y = 6, label = "a") + annotate("text", x = 4, y = 11, label = "b") + labs(x = "Treatment", y = "Entries on the open arms (N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 15, label = "B", size = 6) + ylim(0, 16)
EntriesOA_plot
```

2) Entries on closed arms
2.1) Main test
```{r}
independence_test(Entries_ClosedArms ~ Treatment, data = rat)
```

2.2) Produce post-hoc tests for main effects
```{r}
PT_EntriesOA = pairwisePermutationTest(Entries_OpenArms ~ Treatment, data = rat, method="fdr")
PT_EntriesOA
```

2.3) Draw plot
```{r}
EntriesCA_plot <- ggplot(rat, aes(x = factor(Treatment), y = Entries_ClosedArms, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 6, label = "a") + annotate("text", x = 2, y = 5, label = "a") + annotate("text", x = 3, y = 6, label = "a") + annotate("text", x = 4, y = 7, label = "a") + labs(x = "Treatment", y = "Entries on the closed arms (N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 15, label = "C", size = 6) + ylim(0, 16)
EntriesCA_plot
```

3) Total entries
3.1) Main test
```{r}
independence_test(TotalEntries ~ Treatment, data = rat)
```

3.2) Produce post-hoc tests for main effects
```{r}
PT_Total_Entries = pairwisePermutationTest(TotalEntries ~ Treatment, data = rat, method="fdr")
PT_Total_Entries
```

3.3) Draw plot
```{r}
TotalEntries_plot <- ggplot(rat, aes(x = factor(Treatment), y = Total_Entries, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 13, label = "a") + annotate("text", x = 2, y = 13, label = "a") + annotate("text", x = 3, y = 11, label = "a") + annotate("text", x = 4, y = 14, label = "a") + labs(x = "Treatment", y = "Entries on all arms (N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 15, label = "D", size = 6) + ylim(0, 16)
TotalEntries_plot
```

4) Percent entries on open arm
4.1) Main test
```{r}
independence_test(PercentEntries_OpenArms ~ Treatment, data = rat)
```

4.2) Produce post-hoc tests for main effects
```{r}
PT_PercentEntries = pairwisePermutationTest(PercentEntries_OpenArms ~ Treatment, data = rat, method="fdr")
PT_PercentEntries
```

4.3) Draw plot
```{r}
PercentEntries_plot <- ggplot(rat, aes(x = factor(Treatment), y = PercentEntries_OpenArms, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 80, label = "a") + annotate("text", x = 2, y = 95, label = "a") + annotate("text", x = 3, y = 65, label = "a") + annotate("text", x = 4, y = 95, label = "a") + labs(x = "Treatment", y = "Percent entries on the open arms (%)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 100, label = "E", size = 6) + ylim(0, 100)
PercentEntries_plot
```

5) Time spent on the open arm
5.1) Main test
```{r}
independence_test(Time_OpenArms ~ Treatment, data = rat)
```

5.2) Produce post-hoc tests for main effects
```{r}
PT_TimeOA = pairwisePermutationTest(Time_OpenArms ~ Treatment, data = rat, method="fdr")
PT_TimeOA
```

5.3) Draw plot
```{r}
TimeOA_plot <- ggplot(rat, aes(x = factor(Treatment), y = Time_OpenArms, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 100, label = "a") + annotate("text", x = 2, y = 300, label = "b") + annotate("text", x = 3, y = 180, label = "a") + annotate("text", x = 4, y = 185, label = "b") + labs(x = "Treatment", y = "Time spent on the open arms (s)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 300, label = "F", size = 6)
TimeOA_plot
```

6) Join plots
```{r}
grid.arrange(EntriesOA_plot, EntriesCA_plot, TotalEntries_plot, PercentEntries_plot, TimeOA_plot, nrow = 4)
```

* Run ANOVAs with permutation tests for main effects (based on https://rcompanion.org/rcompanion/d_06a.html) for the locomotor activity test
1) Squares crossed
1.1.) Main test
```{r}
independence_test(Time_OpenArms ~ Treatment, data = rat)
```
1.2) Draw plot
```{r}
CrossedSquares_plot <- ggplot(rat, aes(x = factor(Treatment), y = Crossed_Squares, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 70, label = "a") + annotate("text", x = 2, y = 45, label = "a") + annotate("text", x = 3, y = 55, label = "a") + annotate("text", x = 4, y = 45, label = "a") + labs(x = "Treatment", y = "Squares crossed (N)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 100, label = "B", size = 6)
CrossedSquares_plot
```

3) Rearing
3.1.) Main test
```{r}
independence_test(Grooming ~ Treatment, data = rat)
```
2.2) Draw plot
```{r}
Grooming_plot <- ggplot(rat, aes(x = factor(Treatment), y = Grooming, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 80, label = "a") + annotate("text", x = 2, y = 80, label = "a") + annotate("text", x = 3, y = 80, label = "a") + annotate("text", x = 4, y = 80, label = "a") + labs(x = "Treatment", y = "Time spent grooming (s)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 300, label = "C", size = 6)
Grooming_plot
```

3) Rearing
3.1.) Main test
```{r}
independence_test(Rearing ~ Treatment, data = rat)
```

3.2) Produce post-hoc tests for main effects
```{r}
PT_Rearing = pairwisePermutationTest(Rearing ~ Treatment, data = rat, method="fdr")
PT_Rearing
```

3.2) Draw plot
```{r}
Rearing_plot <- ggplot(rat, aes(x = factor(Treatment), y = Rearing, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 50, label = "a") + annotate("text", x = 2, y = 80, label = "a") + annotate("text", x = 3, y = 80, label = "b") + annotate("text", x = 4, y = 50, label = "a") + labs(x = "Treatment", y = "Time spent rearing (s)") + guides(colour=FALSE) + annotate("text", x = 4.5, y = 300, label = "D", size = 6)
Rearing_plot
```

4) Join plots
```{r}
grid.arrange(blank, CrossedSquares_plot, Grooming_plot, Rearing_plot, nrow = 2)
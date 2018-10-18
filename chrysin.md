---
title: "R Notebook for effects of chrysin and flavone on zebrafish anxiety-like behavior (LaNeC)"
author:
- Caio Maximino^[Universidade Federal do Sul e Sudeste do Pará]
- Monica Gomes Lima^[Universidade do Estado do Pará]
- León German Ponciano^[Universidad Veracruzana]
- Juan Francisco Rodriguez-Landa
output:
  github_document 
subtitle: From project "Use of zebrafish to study the neurobehavioral effects of flavonoids"
tags:
- anxiety
- zebrafish
- flavonoids
abstract: |
  Datapackages and analysis scripts for the project "Use of zebrafish to study the neurobehavioral effects of flavonoids"
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook for the data analysis of the research project Datapackages and analysis scripts for the project "Use of zebrafish to study the neurobehavioral effects of flavonoids".

Data is produced by members from Laboratório de Neurociências e Comportamento "Frederico Guilherme Graeff", affiliated to Universidade Federal do Sul e Sudeste do Pará and Universidade do Estado do Pará, in collaboration with members of Laboratorio de Neurofarmacología, affiliated to Universidad Veracruzana. The package will include primary data for a behavioral experiment on the effects of chrysin and flavone on zebrafish anxiety-like behavior.

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

* Load data
```{r}
x1 <- getURL("https://raw.githubusercontent.com/lanec-unifesspa/chrysin/master/chrysinLDT.csv")
exp1 <- read.csv(text = x1)
exp1$Treatment <- as.factor(exp1$Treatment)
View(exp1)
```

* Set seed to ensure reproducibility
```{r}
set.seed(42)
```

* Order factors, otherwise R will alphabetize them
```{r}
exp1$Treatment = factor(exp1$Treatment, ordered=FALSE, levels=unique(exp1$Treatment))
```

* Run ANOVAs with permutation tests for main effects (based on https://rcompanion.org/rcompanion/d_06a.html)

1) Time on white
1.1) Main test
```{r}
independence_test(Time.on.white ~ Treatment, data = exp1)
```

1.2) Produce post-hoc tests for main effects
```{r}
PT_TimeOnWhite = pairwisePermutationTest(Time.on.white ~ Treatment, data = exp1, method="fdr")
```

1.3) Draw plot
```{r}
TW_plot <- ggplot(exp1, aes(x = factor(Treatment), y = Time.on.white, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 280, label = "a") + annotate("text", x = 2, y = 750, label = "ab") + annotate("text", x = 3, y = 930, label = "b") + labs(x = "Treatment", y = "Time on white (s)") + guides(colour=FALSE) + annotate("text", x = 3.5, y = 900, label = "A", size = 6)
```

2) Entry duration
2.1) Main test
```{r}
independence_test(Duration ~ Treatment, data = exp1)
```

2.2) Draw plot
```{r}
Dur_plot <- ggplot(exp1, aes(x = factor(Treatment), y = Duration, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) + annotate("text", x = 1, y = 250, label = "a") + annotate("text", x = 2, y = 350, label = "a") + annotate("text", x = 3, y = 930, label = "a") + labs(x = "Treatment", y = "Entry duration (s/N)") + guides(colour=FALSE) + annotate("text", x = 3.5, y = 900, label = "B", size = 6)
```

3) Transitions to white
3.1) Main test
```{r}
independence_test(Entries ~ Treatment, data = exp1)
```

3.2) Draw plot
```{r}
Trans_plot <- ggplot(exp1, aes(x = factor(Treatment), y = Entries, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 100, label = "a") + annotate("text", x = 2, y = 100, label = "a") + annotate("text", x = 3, y = 110, label = "a") + labs(x = "Treatment", y = "Transitions to white (N)") + guides(colour=FALSE) + annotate("text", x = 3.5, y = 100, label = "C", size = 6)
```

4) Risk assessment
4.1) Main test
```{r}
independence_test(RA ~ Treatment, data = exp1)
```

4.2) Produce post-hoc tests for main effects
```{r}
PT_RA = pairwisePermutationTest(RA ~ Treatment, data = exp1, method="fdr")
```

4.3) Draw plot
```{r}
RA_plot <- ggplot(exp1, aes(x = factor(Treatment), y = RA, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 17, label = "a") + annotate("text", x = 2, y = 10, label = "b") + annotate("text", x = 3, y = 17, label = "ab") + labs(x = "Treatment", y = "Risk assessment (N)") + guides(colour=FALSE) + annotate("text", x = 3.5, y = 15, label = "D", size = 6)
```

5) Erratic swimming
5.1) Main test
```{r}
independence_test(NE ~ Treatment, data = exp1)
```

5.2) Draw plot
```{r}
ES_plot <- ggplot(exp1, aes(x = factor(Treatment), y = NE, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 15, label = "a") + annotate("text", x = 2, y = 5, label = "a") + annotate("text", x = 3, y = 10, label = "a") + labs(x = "Treatment", y = "Erratic swimming (N)") + guides(colour=FALSE) + annotate("text", x = 3.5, y = 14, label = "E", size = 6)
```

6) Erratic swimming
6.1) Main test
```{r}
independence_test(Thigmotaxis ~ Treatment, data = exp1)
```

6.2) Draw plot
```{r}
Thigmo_plot <- ggplot(exp1, aes(x = factor(Treatment), y = Thigmotaxis, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 50, label = "a") + annotate("text", x = 2, y = 42, label = "a") + annotate("text", x = 3, y = 65, label = "a") + labs(x = "Treatment", y = "Thigmotaxis (s)") + guides(colour=FALSE) + annotate("text", x = 3.5, y = 60, label = "F", size = 6)
```

7) Freezing
6.1) Main test
```{r}
independence_test(Freezing ~ Treatment, data = exp1)
```

6.2) Draw plot
```{r}
Freezing_plot <- ggplot(exp1, aes(x = factor(Treatment), y = Freezing, colour = Treatment)) + geom_boxplot(outlier.shape = NA) + geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +  annotate("text", x = 1, y = 200, label = "a") + annotate("text", x = 2, y = 180, label = "a") + annotate("text", x = 3, y = 930, label = "a") + labs(x = "Treatment", y = "Freezing (s)") + guides(colour=FALSE) + annotate("text", x = 3.5, y = 900, label = "G", size = 6)
```
* Join plots in panels
```{r}
grid.arrange(TW_plot, Dur_plot, Trans_plot, RA_plot, ES_plot, Thigmo_plot, Freezing_plot, nrow = 4)
```
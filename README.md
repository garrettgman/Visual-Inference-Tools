# Visual Inference Tools

Traditional methods of teaching statistical inference rely on concepts such as probability distributions that are unfamiliar to students. This may overwhelm a student's cognitive load and hinder understanding. These tools simplify the intuition behind inference to a visual level. They can be used to develop a student's intuitions about inference before teaching mathematical and probabilistic concepts.

The code has been developed by Danny Chang, Vivian Li, Garrett Grolemund and Ben Stevenson. 

## Overview

The vit library presents a model for displaying visual information related to inference processes. Inference revolves around four concepts that relate to each other in fixed ways: a population of data, a sample of data collected from this population, a measurement of the population, and the corresponding measurement of the sample - known as a statistic. 

Vit displays these relationships by organizing data visualizations in a heirarchy with a visualization of the population at the top of the screen, a visualization of the sample of data in the middle, and a visualization of the statistic(s) calculated from the sample(s) at the bottom of the screen. The relationships between these parts are directly illustrated. An animation is used to construct the sample from the population and a second animation is used to construct a statistic from the sample. 

A primary goal of inference is to understand how reliably the sample statistic can be used as a proxy for the population measurement. Vit promotes this understanding by plotting repeated samples and their corresponding sample statistics. These sample statistics accumulate at the bottom of the screen to make a sampling distribution, which can be compared to the actual population measurements at the top of the screen. The visualization of the distribution reveals how often the sampling method results in an accurate statistic and how often it does not. The animations that create the samples and statistics highlight the relationships between data, samples, and statistics that inference relies on.

## Details

The vit model can be used to illustrate a wide variety of inferential and sampling procedures (e.g, examing the coverage of confidence intervals, the effects of sample size on standard error, or the performance of bootstrap methods) and it can be applied to various types of data (e.g, categorical, numeric, multi-dimensional). Each of these tasks involve the same bbasic methods, for example plotting 
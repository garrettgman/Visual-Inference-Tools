# Visual Inference Tools

Traditional methods of teaching statistical inference rely on concepts such as probability distributions that are unfamiliar to students. This may overwhelm a student's cognitive load and hinder understanding. These tools simplify the intuition behind inference to a visual level. They can be used to develop a student's intuitions about inference before teaching mathematical and probabilistic concepts.

The code has been developed by Danny Chang, Vivian Li, Garrett Grolemund and Ben Stevenson. 

## Overview

The vit library presents a model for displaying visual information related to inference processes. Inference revolves around four concepts that relate to each other in fixed ways: a population of data, a sample of data collected from this population, a measurement of the population, and the corresponding measurement of the sample - known as a statistic. 

Vit displays these relationships by organizing data visualizations in a heirarchy with a visualization of the population at the top of the screen, a visualization of the sample of data in the middle, and a visualization of the statistic(s) calculated from the sample(s) at the bottom of the screen. The relationships between these parts are directly illustrated. An animation is used to construct the sample from the population and a second animation is used to construct a statistic from the sample. 

A primary goal of inference is to understand how reliably the sample statistic can be used as a proxy for the population measurement. Vit promotes this understanding by plotting repeated samples and their corresponding sample statistics. These sample statistics accumulate at the bottom of the screen to make a sampling distribution, which can be compared to the actual population measurements at the top of the screen. The visualization of the distribution reveals how often the sampling method results in an accurate statistic and how often it does not. The animations that create the samples and statistics highlight the relationships between data, samples, and statistics that inference relies on.

## Technical Details

The vit model can be used to illustrate a wide variety of inferential and sampling procedures (e.g, examining the coverage of confidence intervals, the effects of sample size on standard error, or the performance of bootstrap methods) and it can be applied to various types of data (e.g, categorical, numeric, multi-dimensional). Each of these tasks involve the same basic actions, for example:
1. plotting data
2. calculating a statistic
3. plotting a statistic
4. etc
The actual details of these actions will vary from task to task. To make vit as widely useful as possible, and to facilitate future extensions of the application, the package has been written around these generic actions. Once the user has provided data and selected a statistical method to explore, vit fills the action functions with their appropriate details.

The vit code is structured into different parts:
* An evironment, e, that builds the gui controls and contains 1) information required by the gui, 2) a canvas object that controls the images vit displays, 3) intermediary functions that link the gui controls to the canvas actions
* A canvas reference class that is used to define the canvas object, c1, stored in the environment, e. The canvas object has methods and fields that store information the canvas needs to create animations and images. The canvas is an r5 reference class object, which saves us from having to pass the canvas through R functions and then collect it on the other side (although there are other strategies that could've done the same thing). The use of the canvas methods also prevents the accumulation of function arguments that overwhelmed earlier versions of vit code.
* Action functions to be filled with details. (This is like a manual version of method dispatch)
* Method details to be assigned to the action functions. These are organized by the type of data or statistical method they work with. A new method can be enabled by writing method details for it and then altering the load-functions to load those details when appropriate
* Load functions that oversee the assignment of details to actions
* grid grob code and methods that create the type of visualizations required by the method details

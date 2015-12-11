# Tyrion

Small data analysis and machine learning framework for Clojure.  
The aim is to provide the tools normally used in data-analysis/machine-learning in Clojure in one place.  
It's built on top of common/available Clojure's related libraries like core.matrix and gorilla repl.

Why not broken-down into small libraries?   
Because it's easier to start this way, and we're using most of it anyway :)  

## Leiningen

[![Clojars Project](http://clojars.org/tyrion/latest-version.svg)](http://clojars.org/tyrion) 

## Status

![alt tag](https://circleci.com/gh/zeniuseducation/Tyrion.svg?style=shield&circle-token=:circle-token)
[![Circle CI](https://circleci.com/gh/zeniuseducation/Tyrion/tree/master.svg?style=svg)](https://circleci.com/gh/zeniuseducation/Tyrion/tree/master)  

## Warning!!

It's not even started yet...   

## Roadmap

1. Basic descriptive and inferential statistics
2. Common algorithms for classification and clustering
3. Regressions
4. Easy web-based visualisations  

## Already implemented  

##### stats.core

mode, median, mean, quartiles, inter-quartile-range (iq-range), deviations, covariance, correlation, variance, std-dev  

##### regressions

linear-regression (using OLS)  

##### distance  

euclidean, squared-euclidean, chebyshev, chi-square, correlation, cosine, cityblock (manhattan), hamming, jaccard  
minkowski, span-norm, weighted-cityblock, weighted-euclidean, weighted-squared-euclidean, weigthed-hamming  

##### view (using gorilla-plot)  

list-plot-compose : like (compose & plot) but instead of plots, it takes lists (and fix the range issue).  
plot-components : plot x-y of two key-pairs from a given list of maps.  
lm-plot : linear-model plot, plot the xy-pairs and its linear-model function (as line).  
lm-plot-compose : linear-model plot for several xy-pairs.  

## Usage

Basic usage for plotting:  

1. You need to use gorilla-repl and you can read the instruction on its website on how to use it  
2. Tyrion adds some features in tyrion.view namespace for plotting  

## License

Copyright © 2015 Zenius Education

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

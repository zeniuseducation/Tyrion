;; gorilla-repl.fileformat = 1

;; **
;;; # Tyrion's examples using Gorilla-REPL
;;; 
;;; Welcome to Gorilla-powered Tyrion.
;;; 
;;; Here are some examples of how to use Tyrion using Gorilla.
;; **

;; @@
(ns harmonious-snowflake
  (:require 
    [gorilla-plot.core :as gp]
    [tyrion.stats :as s]))

;; Let's start with stats namespace
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; #### Commonly used functions in stats namespace
;;; 
;;; Most Tyrion's functions work for either a list of single-value, list of maps, core.matrix's matrix, or core.matrix's dataset
;; **

;; @@
;; Let's create a single-value list of data 

(def one-dim (repeatedly 100 #(rand-int 100)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;harmonious-snowflake/one-dim</span>","value":"#'harmonious-snowflake/one-dim"}
;; <=

;; @@
;; Let's try available functions at once to this one-dimensional data

(zipmap [:mean :freq :freq-by-odd :stdev :variance] 
        ((juxt s/mean s/freq #(s/freq-by odd? %) s/stdev s/variance) one-dim))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mean</span>","value":":mean"},{"type":"html","content":"<span class='clj-double'>49.95</span>","value":"49.95"}],"value":"[:mean 49.95]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:freq</span>","value":":freq"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[0 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>65</span>","value":"65"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[65 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>70</span>","value":"70"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[70 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[7 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[20 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>72</span>","value":"72"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[72 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>58</span>","value":"58"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[58 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[27 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>69</span>","value":"69"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[69 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[24 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>39</span>","value":"39"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[39 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>88</span>","value":"88"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[88 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>46</span>","value":"46"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[46 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[4 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>77</span>","value":"77"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[77 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>95</span>","value":"95"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[95 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>54</span>","value":"54"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[54 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>92</span>","value":"92"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[92 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>75</span>","value":"75"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[75 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[21 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>31</span>","value":"31"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[31 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>91</span>","value":"91"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[91 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[33 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[22 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>90</span>","value":"90"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[90 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>89</span>","value":"89"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[89 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[43 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>61</span>","value":"61"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[61 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>44</span>","value":"44"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[44 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>64</span>","value":"64"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[64 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>51</span>","value":"51"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[51 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[25 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>34</span>","value":"34"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[34 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[17 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[3 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>12</span>","value":"12"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[12 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[2 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[23 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>47</span>","value":"47"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[47 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[19 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>57</span>","value":"57"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[57 4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>68</span>","value":"68"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[68 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[5 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>83</span>","value":"83"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[83 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[14 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>45</span>","value":"45"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[45 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>78</span>","value":"78"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[78 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[26 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[16 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>81</span>","value":"81"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[81 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>98</span>","value":"98"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[98 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>30</span>","value":"30"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[30 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>73</span>","value":"73"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[73 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[10 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>52</span>","value":"52"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[52 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>67</span>","value":"67"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[67 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>71</span>","value":"71"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[71 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>42</span>","value":"42"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[42 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>94</span>","value":"94"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[94 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[8 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>49</span>","value":"49"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[49 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>84</span>","value":"84"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[84 2]"}],"value":"{0 1, 65 1, 70 3, 7 2, 20 1, 72 1, 58 1, 27 1, 69 1, 24 1, 39 1, 88 2, 46 1, 4 2, 77 1, 95 2, 54 2, 92 1, 75 2, 21 3, 31 2, 91 3, 33 1, 22 1, 90 2, 89 1, 43 1, 61 1, 44 1, 64 1, 51 1, 25 1, 34 2, 17 3, 3 1, 12 1, 2 1, 23 1, 47 2, 19 2, 57 4, 68 3, 5 1, 83 2, 14 1, 45 3, 78 1, 26 2, 16 3, 81 1, 98 2, 30 2, 73 1, 10 1, 52 1, 67 3, 71 2, 42 1, 94 2, 8 1, 49 2, 84 2}"}],"value":"[:freq {0 1, 65 1, 70 3, 7 2, 20 1, 72 1, 58 1, 27 1, 69 1, 24 1, 39 1, 88 2, 46 1, 4 2, 77 1, 95 2, 54 2, 92 1, 75 2, 21 3, 31 2, 91 3, 33 1, 22 1, 90 2, 89 1, 43 1, 61 1, 44 1, 64 1, 51 1, 25 1, 34 2, 17 3, 3 1, 12 1, 2 1, 23 1, 47 2, 19 2, 57 4, 68 3, 5 1, 83 2, 14 1, 45 3, 78 1, 26 2, 16 3, 81 1, 98 2, 30 2, 73 1, 10 1, 52 1, 67 3, 71 2, 42 1, 94 2, 8 1, 49 2, 84 2}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:freq-by-odd</span>","value":":freq-by-odd"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>53</span>","value":"53"}],"value":"[true 53]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>47</span>","value":"47"}],"value":"[false 47]"}],"value":"{true 53, false 47}"}],"value":"[:freq-by-odd {true 53, false 47}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:stdev</span>","value":":stdev"},{"type":"html","content":"<span class='clj-double'>29.067102790723133</span>","value":"29.067102790723133"}],"value":"[:stdev 29.067102790723133]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:variance</span>","value":":variance"},{"type":"html","content":"<span class='clj-double'>844.8964646464647</span>","value":"844.8964646464647"}],"value":"[:variance 844.8964646464647]"}],"value":"{:mean 49.95, :freq {0 1, 65 1, 70 3, 7 2, 20 1, 72 1, 58 1, 27 1, 69 1, 24 1, 39 1, 88 2, 46 1, 4 2, 77 1, 95 2, 54 2, 92 1, 75 2, 21 3, 31 2, 91 3, 33 1, 22 1, 90 2, 89 1, 43 1, 61 1, 44 1, 64 1, 51 1, 25 1, 34 2, 17 3, 3 1, 12 1, 2 1, 23 1, 47 2, 19 2, 57 4, 68 3, 5 1, 83 2, 14 1, 45 3, 78 1, 26 2, 16 3, 81 1, 98 2, 30 2, 73 1, 10 1, 52 1, 67 3, 71 2, 42 1, 94 2, 8 1, 49 2, 84 2}, :freq-by-odd {true 53, false 47}, :stdev 29.067102790723133, :variance 844.8964646464647}"}
;; <=

;; @@
(zipmap [:median :mode :quartiles :inter-quartile-range]
        ((juxt s/median s/mode s/quartiles s/iq-range) one-dim))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:median</span>","value":":median"},{"type":"html","content":"<span class='clj-double'>50.0</span>","value":"50.0"}],"value":"[:median 50.0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mode</span>","value":":mode"},{"type":"html","content":"<span class='clj-unkown'>57</span>","value":"57"}],"value":"[:mode 57]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:quartiles</span>","value":":quartiles"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-unkown'>51</span>","value":"51"},{"type":"html","content":"<span class='clj-unkown'>73</span>","value":"73"}],"value":"[23 51 73]"}],"value":"[:quartiles [23 51 73]]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:inter-quartile-range</span>","value":":inter-quartile-range"},{"type":"html","content":"<span class='clj-long'>50</span>","value":"50"}],"value":"[:inter-quartile-range 50]"}],"value":"{:median 50.0, :mode 57, :quartiles [23 51 73], :inter-quartile-range 50}"}
;; <=

;; @@
(s/correlation one-dim (mapv #((rand-nth [+ -]) % (rand-int 10)) one-dim))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.9838861855452712</span>","value":"0.9838861855452712"}
;; <=

;; @@
(s/covariance one-dim (shuffle one-dim))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>-100.47727272727275</span>","value":"-100.47727272727275"}
;; <=

;; **
;;; #### Tyrion also provides some dataset examples
;;; 
;;; They are available in tyrion.data namespace and you can see them in datasets 
;; **

;; @@
(require '[tyrion.data :as d])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
d/datasets
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:iris</span>","value":":iris"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:name</span>","value":":name"},{"type":"html","content":"<span class='clj-string'>&quot;Iris&quot;</span>","value":"\"Iris\""}],"value":"[:name \"Iris\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:description</span>","value":":description"},{"type":"html","content":"<span class='clj-string'>&quot;Iris dataset, good for clustering/classifications, or basic stats&quot;</span>","value":"\"Iris dataset, good for clustering/classifications, or basic stats\""}],"value":"[:description \"Iris dataset, good for clustering/classifications, or basic stats\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:file</span>","value":":file"},{"type":"html","content":"<span class='clj-string'>&quot;./data/iris.edn&quot;</span>","value":"\"./data/iris.edn\""}],"value":"[:file \"./data/iris.edn\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:numerics</span>","value":":numerics"},{"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"html","content":"<span class='clj-keyword'>:sepal-width</span>","value":":sepal-width"},{"type":"html","content":"<span class='clj-keyword'>:petal-length</span>","value":":petal-length"},{"type":"html","content":"<span class='clj-keyword'>:petal-width</span>","value":":petal-width"}],"value":"#{:sepal-length :sepal-width :petal-length :petal-width}"}],"value":"[:numerics #{:sepal-length :sepal-width :petal-length :petal-width}]"}],"value":"{:name \"Iris\", :description \"Iris dataset, good for clustering/classifications, or basic stats\", :file \"./data/iris.edn\", :numerics #{:sepal-length :sepal-width :petal-length :petal-width}}"}],"value":"[:iris {:name \"Iris\", :description \"Iris dataset, good for clustering/classifications, or basic stats\", :file \"./data/iris.edn\", :numerics #{:sepal-length :sepal-width :petal-length :petal-width}}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:mammals</span>","value":":mammals"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:name</span>","value":":name"},{"type":"html","content":"<span class='clj-string'>&quot;Mammals&quot;</span>","value":"\"Mammals\""}],"value":"[:name \"Mammals\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:description</span>","value":":description"},{"type":"html","content":"<span class='clj-string'>&quot;Brain and body weight of various mammals&quot;</span>","value":"\"Brain and body weight of various mammals\""}],"value":"[:description \"Brain and body weight of various mammals\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:file</span>","value":":file"},{"type":"html","content":"<span class='clj-string'>&quot;./data/mammals.edn&quot;</span>","value":"\"./data/mammals.edn\""}],"value":"[:file \"./data/mammals.edn\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:numerics</span>","value":":numerics"},{"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:brain-weight</span>","value":":brain-weight"},{"type":"html","content":"<span class='clj-keyword'>:body-weight</span>","value":":body-weight"}],"value":"#{:brain-weight :body-weight}"}],"value":"[:numerics #{:brain-weight :body-weight}]"}],"value":"{:name \"Mammals\", :description \"Brain and body weight of various mammals\", :file \"./data/mammals.edn\", :numerics #{:brain-weight :body-weight}}"}],"value":"[:mammals {:name \"Mammals\", :description \"Brain and body weight of various mammals\", :file \"./data/mammals.edn\", :numerics #{:brain-weight :body-weight}}]"}],"value":"{:iris {:name \"Iris\", :description \"Iris dataset, good for clustering/classifications, or basic stats\", :file \"./data/iris.edn\", :numerics #{:sepal-length :sepal-width :petal-length :petal-width}}, :mammals {:name \"Mammals\", :description \"Brain and body weight of various mammals\", :file \"./data/mammals.edn\", :numerics #{:brain-weight :body-weight}}}"}
;; <=

;; @@
;; or if you just want to know the name of the datasets that are used when called

(keys d/datasets)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(:iris :mammals)</span>","value":"(:iris :mammals)"}
;; <=

;; @@
;; let's load iris

(def iris (d/load-data :iris))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;harmonious-snowflake/iris</span>","value":"#'harmonious-snowflake/iris"}
;; <=

;; @@
;; if you want just the data you need to get :data from iris

(def iris-data (:data iris))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;harmonious-snowflake/iris-data</span>","value":"#'harmonious-snowflake/iris-data"}
;; <=

;; @@
;; let's apply various stats functions to this iris-data
;; all Tyrion's stats function have a corresponding n(fn) to be used to simultaneously calculate (fn) to multiple dimensions, they work for n-dimensional data

(s/nmean [:sepal-length :petal-length] iris-data)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"html","content":"<span class='clj-double'>5.843333333333335</span>","value":"5.843333333333335"}],"value":"[:sepal-length 5.843333333333335]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:petal-length</span>","value":":petal-length"},{"type":"html","content":"<span class='clj-double'>3.7586666666666693</span>","value":"3.7586666666666693"}],"value":"[:petal-length 3.7586666666666693]"}],"value":"{:sepal-length 5.843333333333335, :petal-length 3.7586666666666693}"}
;; <=

;; @@
(s/nfreq-by #(< % 5) [:sepal-length :sepal-width] iris-data)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>128</span>","value":"128"}],"value":"[false 128]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"}],"value":"[true 22]"}],"value":"{false 128, true 22}"}],"value":"[:sepal-length {false 128, true 22}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-width</span>","value":":sepal-width"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>150</span>","value":"150"}],"value":"[true 150]"}],"value":"{true 150}"}],"value":"[:sepal-width {true 150}]"}],"value":"{:sepal-length {false 128, true 22}, :sepal-width {true 150}}"}
;; <=

;; @@
(s/nfreq-by {:petal-width #(< % 0.5)
             :petal-length #(< % 1.3)}
            [:petal-width :petal-length]
            iris-data)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:petal-width</span>","value":":petal-width"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>48</span>","value":"48"}],"value":"[true 48]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>102</span>","value":"102"}],"value":"[false 102]"}],"value":"{true 48, false 102}"}],"value":"[:petal-width {true 48, false 102}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:petal-length</span>","value":":petal-length"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>146</span>","value":"146"}],"value":"[false 146]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[true 4]"}],"value":"{false 146, true 4}"}],"value":"[:petal-length {false 146, true 4}]"}],"value":"{:petal-width {true 48, false 102}, :petal-length {false 146, true 4}}"}
;; <=

;; @@
;; we can also test the correlations between variables

(s/correlation :sepal-width :sepal-length iris-data)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>-0.10936924995064935</span>","value":"-0.10936924995064935"}
;; <=

;; @@
;; or extract the data first

(s/correlation (mapv :petal-length iris-data)(mapv :petal-width iris-data))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.9627570970509669</span>","value":"0.9627570970509669"}
;; <=

;; @@
;; covariance works the same

(s/covariance :petal-length :petal-width iris-data)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1.2963874720357946</span>","value":"1.2963874720357946"}
;; <=

;; @@
(s/covariance (mapv :sepal-length iris-data)(mapv :petal-length iris-data))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>1.2736823266219242</span>","value":"1.2736823266219242"}
;; <=

;; @@
;; let's try other things

(s/nstdev [:sepal-length :petal-width :sepal-width] iris-data)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"html","content":"<span class='clj-double'>0.8280661279778629</span>","value":"0.8280661279778629"}],"value":"[:sepal-length 0.8280661279778629]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:petal-width</span>","value":":petal-width"},{"type":"html","content":"<span class='clj-double'>0.7631607417008414</span>","value":"0.7631607417008414"}],"value":"[:petal-width 0.7631607417008414]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-width</span>","value":":sepal-width"},{"type":"html","content":"<span class='clj-double'>0.4335943113621737</span>","value":"0.4335943113621737"}],"value":"[:sepal-width 0.4335943113621737]"}],"value":"{:sepal-length 0.8280661279778629, :petal-width 0.7631607417008414, :sepal-width 0.4335943113621737}"}
;; <=

;; @@
;; what if we filter the data first

(s/nstdev [:sepal-length :petal-width :sepal-width] (filter #(= "Iris-setosa" (:species %)) iris-data))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"html","content":"<span class='clj-double'>0.3524896872134512</span>","value":"0.3524896872134512"}],"value":"[:sepal-length 0.3524896872134512]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:petal-width</span>","value":":petal-width"},{"type":"html","content":"<span class='clj-double'>0.10720950308167837</span>","value":"0.10720950308167837"}],"value":"[:petal-width 0.10720950308167837]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-width</span>","value":":sepal-width"},{"type":"html","content":"<span class='clj-double'>0.38102439795469095</span>","value":"0.38102439795469095"}],"value":"[:sepal-width 0.38102439795469095]"}],"value":"{:sepal-length 0.3524896872134512, :petal-width 0.10720950308167837, :sepal-width 0.38102439795469095}"}
;; <=

;; @@
(s/nmean [:sepal-length :petal-width :sepal-width] (filter #(= "Iris-virginica" (:species %)) iris-data))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"html","content":"<span class='clj-double'>6.587999999999998</span>","value":"6.587999999999998"}],"value":"[:sepal-length 6.587999999999998]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:petal-width</span>","value":":petal-width"},{"type":"html","content":"<span class='clj-double'>2.026</span>","value":"2.026"}],"value":"[:petal-width 2.026]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-width</span>","value":":sepal-width"},{"type":"html","content":"<span class='clj-double'>2.9739999999999998</span>","value":"2.9739999999999998"}],"value":"[:sepal-width 2.9739999999999998]"}],"value":"{:sepal-length 6.587999999999998, :petal-width 2.026, :sepal-width 2.9739999999999998}"}
;; <=

;; @@
(s/nvariance [:sepal-length :petal-width :sepal-width] (filter #(= "Iris-versicolor" (:species %)) iris-data))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"html","content":"<span class='clj-double'>0.2664326530612246</span>","value":"0.2664326530612246"}],"value":"[:sepal-length 0.2664326530612246]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:petal-width</span>","value":":petal-width"},{"type":"html","content":"<span class='clj-double'>0.039106122448979576</span>","value":"0.039106122448979576"}],"value":"[:petal-width 0.039106122448979576]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-width</span>","value":":sepal-width"},{"type":"html","content":"<span class='clj-double'>0.09846938775510206</span>","value":"0.09846938775510206"}],"value":"[:sepal-width 0.09846938775510206]"}],"value":"{:sepal-length 0.2664326530612246, :petal-width 0.039106122448979576, :sepal-width 0.09846938775510206}"}
;; <=

;; @@
;; We can also turn them into matrix and do the same things

(require '[clojure.core.matrix :as mat])

(def iris-mat (mat/matrix (mapv #(butlast (vals %)) iris-data)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;harmonious-snowflake/iris-mat</span>","value":"#'harmonious-snowflake/iris-mat"}
;; <=

;; @@
(s/nmean [0 1 2 3] iris-mat)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>5.843333333333335</span>","value":"5.843333333333335"},{"type":"html","content":"<span class='clj-double'>3.0540000000000007</span>","value":"3.0540000000000007"},{"type":"html","content":"<span class='clj-double'>3.7586666666666693</span>","value":"3.7586666666666693"},{"type":"html","content":"<span class='clj-double'>1.1986666666666672</span>","value":"1.1986666666666672"}],"value":"[5.843333333333335 3.0540000000000007 3.7586666666666693 1.1986666666666672]"}
;; <=

;; @@
;; we can also opt for certain dimensions that we're interested in 

(s/nfreq-by #(< % 4) [0 1 3] iris-mat)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>150</span>","value":"150"}],"value":"[false 150]"}],"value":"{false 150}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>146</span>","value":"146"}],"value":"[true 146]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[false 4]"}],"value":"{true 146, false 4}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>150</span>","value":"150"}],"value":"[true 150]"}],"value":"{true 150}"}],"value":"[{false 150} {true 146, false 4} {true 150}]"}
;; <=

;; @@
;; and/or choose the function we want to apply for different column/key

(s/nfreq-by {0 #(< % 5) 1 #(< % 3) 2 #(< % 4)} [0 1 2] iris-mat)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>128</span>","value":"128"}],"value":"[false 128]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"}],"value":"[true 22]"}],"value":"{false 128, true 22}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>93</span>","value":"93"}],"value":"[false 93]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>57</span>","value":"57"}],"value":"[true 57]"}],"value":"{false 93, true 57}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-long'>61</span>","value":"61"}],"value":"[true 61]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"},{"type":"html","content":"<span class='clj-long'>89</span>","value":"89"}],"value":"[false 89]"}],"value":"{true 61, false 89}"}],"value":"[{false 128, true 22} {false 93, true 57} {true 61, false 89}]"}
;; <=

;; **
;;; ### Plotting in Tyrion
;;; 
;;; Tyrion tries to extend gorilla-plot capabilities to make data visualisation more enjoyable
;; **

;; @@
(require '[tyrion.view :as v])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
;; Here's how to create a table view

(v/table (take 10 iris-mat))
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.1</span>","value":"5.1"},{"type":"html","content":"<span class='clj-double'>3.5</span>","value":"3.5"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [5.1,3.5,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.9</span>","value":"4.9"},{"type":"html","content":"<span class='clj-double'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.9,3.0,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.7</span>","value":"4.7"},{"type":"html","content":"<span class='clj-double'>3.2</span>","value":"3.2"},{"type":"html","content":"<span class='clj-double'>1.3</span>","value":"1.3"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.7,3.2,1.3,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.6</span>","value":"4.6"},{"type":"html","content":"<span class='clj-double'>3.1</span>","value":"3.1"},{"type":"html","content":"<span class='clj-double'>1.5</span>","value":"1.5"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.6,3.1,1.5,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-double'>3.6</span>","value":"3.6"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [5.0,3.6,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.4</span>","value":"5.4"},{"type":"html","content":"<span class='clj-double'>3.9</span>","value":"3.9"},{"type":"html","content":"<span class='clj-double'>1.7</span>","value":"1.7"},{"type":"html","content":"<span class='clj-double'>0.4</span>","value":"0.4"}],"value":"#vectorz/vector [5.4,3.9,1.7,0.4]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.6</span>","value":"4.6"},{"type":"html","content":"<span class='clj-double'>3.4</span>","value":"3.4"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.3</span>","value":"0.3"}],"value":"#vectorz/vector [4.6,3.4,1.4,0.3]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-double'>3.4</span>","value":"3.4"},{"type":"html","content":"<span class='clj-double'>1.5</span>","value":"1.5"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [5.0,3.4,1.5,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.4</span>","value":"4.4"},{"type":"html","content":"<span class='clj-double'>2.9</span>","value":"2.9"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.4,2.9,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.9</span>","value":"4.9"},{"type":"html","content":"<span class='clj-double'>3.1</span>","value":"3.1"},{"type":"html","content":"<span class='clj-double'>1.5</span>","value":"1.5"},{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"}],"value":"#vectorz/vector [4.9,3.1,1.5,0.1]"}],"value":"#gorilla_repl.table.TableView{:contents (#vectorz/vector [5.1,3.5,1.4,0.2] #vectorz/vector [4.9,3.0,1.4,0.2] #vectorz/vector [4.7,3.2,1.3,0.2] #vectorz/vector [4.6,3.1,1.5,0.2] #vectorz/vector [5.0,3.6,1.4,0.2] #vectorz/vector [5.4,3.9,1.7,0.4] #vectorz/vector [4.6,3.4,1.4,0.3] #vectorz/vector [5.0,3.4,1.5,0.2] #vectorz/vector [4.4,2.9,1.4,0.2] #vectorz/vector [4.9,3.1,1.5,0.1]), :opts nil}"}
;; <=

;; @@
;; or you can add a header

(v/table (take 10 iris-mat) (keys (dissoc (first iris-data) :species)))
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><th>","close":"</th></tr>","separator":"</th><th>","items":[{"type":"html","content":"<span class='clj-keyword'>:sepal-length</span>","value":":sepal-length"},{"type":"html","content":"<span class='clj-keyword'>:sepal-width</span>","value":":sepal-width"},{"type":"html","content":"<span class='clj-keyword'>:petal-length</span>","value":":petal-length"},{"type":"html","content":"<span class='clj-keyword'>:petal-width</span>","value":":petal-width"}],"value":"(:sepal-length :sepal-width :petal-length :petal-width)"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.1</span>","value":"5.1"},{"type":"html","content":"<span class='clj-double'>3.5</span>","value":"3.5"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [5.1,3.5,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.9</span>","value":"4.9"},{"type":"html","content":"<span class='clj-double'>3.0</span>","value":"3.0"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.9,3.0,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.7</span>","value":"4.7"},{"type":"html","content":"<span class='clj-double'>3.2</span>","value":"3.2"},{"type":"html","content":"<span class='clj-double'>1.3</span>","value":"1.3"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.7,3.2,1.3,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.6</span>","value":"4.6"},{"type":"html","content":"<span class='clj-double'>3.1</span>","value":"3.1"},{"type":"html","content":"<span class='clj-double'>1.5</span>","value":"1.5"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.6,3.1,1.5,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-double'>3.6</span>","value":"3.6"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [5.0,3.6,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.4</span>","value":"5.4"},{"type":"html","content":"<span class='clj-double'>3.9</span>","value":"3.9"},{"type":"html","content":"<span class='clj-double'>1.7</span>","value":"1.7"},{"type":"html","content":"<span class='clj-double'>0.4</span>","value":"0.4"}],"value":"#vectorz/vector [5.4,3.9,1.7,0.4]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.6</span>","value":"4.6"},{"type":"html","content":"<span class='clj-double'>3.4</span>","value":"3.4"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.3</span>","value":"0.3"}],"value":"#vectorz/vector [4.6,3.4,1.4,0.3]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>5.0</span>","value":"5.0"},{"type":"html","content":"<span class='clj-double'>3.4</span>","value":"3.4"},{"type":"html","content":"<span class='clj-double'>1.5</span>","value":"1.5"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [5.0,3.4,1.5,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.4</span>","value":"4.4"},{"type":"html","content":"<span class='clj-double'>2.9</span>","value":"2.9"},{"type":"html","content":"<span class='clj-double'>1.4</span>","value":"1.4"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"#vectorz/vector [4.4,2.9,1.4,0.2]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-double'>4.9</span>","value":"4.9"},{"type":"html","content":"<span class='clj-double'>3.1</span>","value":"3.1"},{"type":"html","content":"<span class='clj-double'>1.5</span>","value":"1.5"},{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"}],"value":"#vectorz/vector [4.9,3.1,1.5,0.1]"}],"value":"#gorilla_repl.table.TableView{:contents (#vectorz/vector [5.1,3.5,1.4,0.2] #vectorz/vector [4.9,3.0,1.4,0.2] #vectorz/vector [4.7,3.2,1.3,0.2] #vectorz/vector [4.6,3.1,1.5,0.2] #vectorz/vector [5.0,3.6,1.4,0.2] #vectorz/vector [5.4,3.9,1.7,0.4] #vectorz/vector [4.6,3.4,1.4,0.3] #vectorz/vector [5.0,3.4,1.5,0.2] #vectorz/vector [4.4,2.9,1.4,0.2] #vectorz/vector [4.9,3.1,1.5,0.1]), :opts (:columns (:sepal-length :sepal-width :petal-length :petal-width))}"}
;; <=

;; @@

;; @@

# InteractiveVenny


### Install dependencies
To check the dependencies installed correctly, this command can help users to check the status of each installation<br/>
```R
library("Packages for check")
```
Code for install dependencies R packages 
```R
cDep <- c("ggplot2","shiny","shinyBS","colourpicker","ggthemes","DT")

###INSTALLED PACKAGES
#get installed list
inst <- packageStatus()$inst

#check and install DEPENDENCIES from CRAN
for(i in 1:length(cDep)){
  tag = which(inst$Package == cDep[i])
  if(length(tag)){
    remove.packages(cDep[i])
  }
  install.packages(cDep[i])
}

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("AnalytixWare/shinysky.incubator")
devtools::install_github("AnalytixWare/ShinySky")

```
### Install TBploter
To install the latest development build directly from GitHub, run this:

```R
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("likelet/InteractiveVenny")
```
### Update Information
####2016-8-15
add tiff plot function 

### Designers:
Qi Zhao, zhaoqi@sysucc.org.cn<br/>

### Developers:
Qi Zhao, zhaoqi3@msysucc.org.cn <br/>

### Maintainer:
Qi Zhao

### Copyright
MIT license

### Citation 
during preparation

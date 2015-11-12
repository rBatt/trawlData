<!--!![Travis-CI Build Status](https://magnum.travis-ci.com/rBatt/trawlData.svg?token=ZmFLF6sygbxo9Je63ydg&branch=master)-->


# trawlData
<p align="center">
 <!--:shell: :tropical_fish: :octopus: :fish: :fishing_pole_and_fish: -->
</p>

<!--![Random Trawl Species](./inst/extdata/taxPictures/Squalus_acanthias.jpg?raw=TRUE)-->
<p align="center">
<img src="./inst/extdata/taxPictures/Squalus_acanthias.jpg?raw=TRUE", width="500">
</p>


R package for maintaining and manipulating data from bottom trawl survey and associated data sets

---


## Install this R package
~~To install this repo as an R package:~~  
```{r}
# library(devtools)
# install_github("rBatt/trawlData", auth_token=pat) # see note below for pat token
```
~~Because this is a private repo, you need to generate a Personal Access Token [HERE](https://github.com/settings/tokens). After you sign in to GitHub, click "Generate new token". The default settings for the token are fine. When the token is generated, set copy and paste it in place of `pat` in the code above.~~ *Note:* [`install_github` currently doesn't work with Git LFS](https://github.com/hadley/devtools/issues/889).  

To install this repo, the easiest thing to do is to clone it, then then use `devtools:install()`:  
 - In terminal, type:  
 ```
cd path/to/folder/to/hold/repo
git clone https://github.com/rBatt/trawlData
 ```  
 - In R, type:  
```{r}
library(devtools)
setwd("path/to/folder/to/hold/repo/trawlData")
devtools::install()
```
 - There is currently a problem with using the latest version of data.table; thus, to install 1.9.4:  
 ```{r}
 library(devtools)
 install_version("data.table", version="1.9.4")
 ```
 
## Use this R package
```{r}
# load package
library(trawlData)

# both raw & clean data + taxonomy already in workspace
data(raw.newf) # raw Newfoundland
data(clean.ebs) # clean Eastern Berring Sea
data(spp.key) # the species key

# check documentation
?raw.sa
?clean.wctri

# easy function to read in from source data
my.goa <- read.trawl(reg="goa") # same as raw.goa

# functions for cleaning raw data
# a specific order for calling these functions!
clean.names(my.goa) # note it affects my.goa directly!
clean.format(my.goa)
clean.columns(my.goa)
my.goa <- clean.tax(my.goa) # only clean.__ function that doesn't change data.table in place!
clean.trimRow(my.goa) # doesn't drop rows, just adds suggestive keep.row column

# somewhat confusingly, clean.trimCol actually
# reduces information in your data set!
# (it drops columns)
skinny.goa <- copy(clean.goa)
clean.trimCol(clean.goa)

# these functions are documented
?clean.trimCol # you will see the options for which columns to keep (or not)!

# check which data sets are available
data(package="trawlData")
```

## Contribute to this repo
 1. Be sure to have Git and Git LFS  
 2. `inst/extdata/taxonomy/spp.key.csv` has the taxonomic key, which needs help  
 3. Documentation and (particularly data set metadata) needs improvement  
   - edit `R/datasets.R`
   - use `devtools::document()`
 4. If you see something that is missing or needs improvement, [create an issue](https://github.com/rBatt/trawlData/issues)

This repository uses `git-lfs`. Git Large File Storage is a way to manage large binary files into a git workflow without taking up a ton of disk space (and it lets you put large files up to GitHub!).

[Installing `git-lfs` is easy; see this link for instructions](https://git-lfs.github.com/). 

####**Install Homebrew**  
If you're on a Mac, let me provide some additional guidance. The *easiest* way involves installing it via a package manager called [Homebrew](http://brew.sh/). Here's how to get Homebrew:  
 1. Make sure you have Xcode downloaded. You can get it through the App Store.  
 2. In Terminal, enter the following: 
   - `ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

####**Install Git LFS**  
Now that you have Homebrew installed, you can get `git-lfs` installed by entering the following into Terminal:  
 - `brew install git-lfs`
 
####**Make sure your Git (or Git LFS) is up to date**  
Git LFS requires Git v 1.8.2 or later. To check your version of Git, type in Terminal `git version`. The following steps apply to Git LFS, too (replace occurrences of `git` with `git-lfs` for all calls to  `brew`). Type `git lfs version` to see the Git LFS version.


*Never used Git Before (Homebrew Install)*  
If you've never used Git before, it's easy to get started. Enter the following into Terminal:  
 1. `brew update` # make sure Homebrew is up to date  
 2. `brew install git` # install git via homebrew  
 3. `brew link git` # might tell you this has already been done  
 4. Next, you should get set up with a credential manager (**Note** not relevant for `git-lfs`)  
   1. `git config --global user.name "yourGitHubName"` # use your GitHub account name  
   2. `git config --global user.email youraddress@domain.com`  
   3. `git credential-osxkeychain` # should see stuff like usage: git crediential-osxkeychain ...  
   4. `git config --global credential.helper osxkeychain` # should see nothing  
   5. Next time you try to do something with git, you'll enter your name and pass, then good to go from now on!  


*Update Homebrew Installation of Git*  
If you have installed Git through Homebrew, but need to update your Git:  
  1. `brew update` # update homebrew
  2. `brew upgrade git` # update git
  3. `brew link git` # make sure it's linked; can even follow the unlink then link instructions it'll print to be sure


*Start Managing Git w/ Homebrew (used something else previously)*  
If you have previously installed Git through means other than Homebrew, and want to start managing with Homebrew:  
 1. `brew update` # makes sure Homebrew is up to date
 2. `brew install git` # installs Git
 3. `echo $PATH` # if `/usr/local/bin` isn't listed before `/usr/bin`, your system won't look to the Homebrew versino of Git
   * If you need to change your `PATH` variable, there are a few approaches
   * Try this first. 
     * In Terminal, enter: `export PATH="$(brew --prefix)/bin:$PATH"`
     * Restart Terminal
     * type `which git`; hopefully the output is `/usr/local/bin/git`
     * type `git version`; hopefully the output is the latest version
     * If these output are correct, you're good to go!
    * If you aren't having success here, try going on to Step 4 (below), and see if things are corrected via more `brew` commands
    * If those steps don't work, look at [this Stack Overflow question, and the answers within](http://stackoverflow.com/questions/8957862/how-to-upgrade-git-to-latest-version-on-mac-os-x)  
 4. `brew link git` # try the unlink && link thing too
 
If there are any issues, try doing a `brew uninstall git` followed by `brew install git`.

Steps for updating Git via Homebrew apply equally to updating/ installing Git LFS via Homebrew.

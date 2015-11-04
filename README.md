# trawlData
R package for maintaining and manipulating data from bottom trawl survey and associated data sets

---

__Note:__ This repository uses `git-lfs`. Git Large File Storage is a way to manage large binary files into a git workflow without taking up a ton of disk space (and it lets you put large files up to GitHub!).

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

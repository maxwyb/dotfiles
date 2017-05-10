# ---macOS-specific bash configurations---

# Setting PATH for Python 3.4
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH

# added by Anaconda3 2.3.0 installer
export PATH="/Users/Max/anaconda/bin:$PATH"

##
# Your previous /Users/Max/.bash_profile file was backed up as /Users/Max/.bash_profile.macports-saved_2016-02-13_at_17:55:50
##

# MacPorts Installer addition on 2016-02-13_at_17:55:50: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# Android SDK directory added by myself
export PATH="/Users/Max/Library/Android/sdk/platform-tools:$PATH"

# alias added by myself for convenience
#alias ls='ls -G'
objdump() {
    if [[ $1 == "-d" ]]; then
	command otool -tV $2
    fi
}
alias lsusb='system_profiler SPUSBDataType'
alias racket='/Applications/Racket\ v6.8/bin/racket'

# ls color scheme in dark terminal background
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
# bash prompt color in Ubuntu Mate
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# git tab auto-completion
source ~/.git-completion.bash
source ~/.ssh-completion.bash 

# OPAM configuration
. /Users/Max/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true


# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=100000
HISTTIMEFORMAT='%Y/%m/%d %H:%M:%S '

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

## Alias Commands
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias grep='grep --exclude-dir=.svn --color=always'
alias clip='xclip -selection clipboard'
alias open='gnome-open'

## SVN and SSH
export SSH_USER=h-takeda
export SVN_SSH="ssh -l ${SSH_USER}"

## Rviz for a laptop user
export OGRE_RTT_MODE=Copy

# emacs
alias e='emacs -nw'
export EDITOR='emacs -nw'

# roseus
alias r='roseus'
alias er='rlwrap roseus'

# ros
# source /opt/ros/kinetic/setup.bash
# source ~/catkin_ws/jaxon_tutorial2/devel/setup.bash
source ~/catkin_ws/poyon/devel/setup.bash
# source ~/catkin_ws/enshu/devel/setup.bash
# source ~/catkin_ws/khi_tutorial/devel/setup.bash
# source `rospack find jsk_tools`/src/bashrc.ros
export CNOID_INSTALL_DIR=/usr/local/choreonoid
# export CNOID_RTM_DIR=~/catkin_ws/jaxon_tutorial2/devel
export CNOID_RTM_DIR=/opt/ros/kinetic
export PKG_CONFIG_PATH=${CNOID_INSTALL_DIR}/lib/pkgconfig:$PKG_CONFIG_PATH
export PATH=${CNOID_INSTALL_DIR}/bin:$PATH
export ROBOT=RHP4B

# ternimal
export PS1='\[\033[01;36m\][$(roscd; echo `pwd`|rev|cut -f2 -d '/'|rev|cut -c 1-3)] \[\033[01;32m\]\h\[\033[01;33m\] \w$(__git_ps1) \[\033[01;34m\]\$\[\033[00m\] '

# open pdf
function pdf() {
	evince $1 &
}

# convert newest mp4 -> gif
function gif() {
	cd ~/Videos;
	VNAME=`ls -t |sed -n 1p`
	if [ ! "$1" ]; then NEWVNAME=`echo $VNAME |sed -e 's/mp4/gif/'`; fi
	ffmpeg -i $VNAME -an -s 624x774 -pix_fmt rgb24 -f gif $NEWVNAME
	nautilus .
	exit
}

# latex
function latex() {
	platex $1.tex; dvipdfmx $1.dvi; gnome-open $1.pdf
}

export QSYS_ROOTDIR="/home/htakeda/altera/15.0/quartus/sopc_builder/bin"
export PATH=$PATH:$HOME/altera/15.0/quartus/bin
export ALTERAOCLSDKROOT="/home/htakeda/altera/15.0/hld"

function median3() {
	awk '{if($1 <= $2 && $2 <= $3) print $2;
     else if($1 <= $3 && $3 <= $2) print $3;
     else if($2 <= $1 && $1 <= $3) print $1;
     else if($2 <= $3 && $3 <= $1) print $3;
     else if($3 <= $1 && $1 <= $2) print $1;
     else print $2}'
}

function bag2log() {
	rostopic echo /iq -p -b ~/bag/${1}kg${2}.bag | sed -e '/%/d' | cut -f 2-3 -d "," | sed -e "s/,/ /" | awk 'BEGIN{tm=0;lasttm=0} {if(NR==1) lasttm=$1} {if(0<=$1-lasttm)tm=tm+$1-lasttm;else tm=tm+$1-lasttm+65536} {lasttm=$1} {print tm/(10^6),$2*2*3.14159/(2^16)}'> ~/bag/${1}kg${2}.log;
	echo -e "\e[1;34mFind dt larger than 3000\e[1;37m";
	cat ~/bag/${1}kg${2}.log | awk '{if(NR==1) lasttm=0} {if($1-lasttm>3000) print NR,$1-lasttm} {lasttm=$1}';
	cat ~/bag/${1}kg${2}.log | awk 'BEGIN{lastt=0; lastx=0} {dt=$1-lastt; dx=$2-lastx; lastt=$1; lastx=$2; if(NR>1) print $1-dt/2,dx/dt}' > ~/bag/${1}kg${2}.vel.log;
	cat ~/bag/${1}kg${2}.vel.log | awk '{if(NR>2) print $2,last2,llast2} {llast2=last2; last2=$2}' | median3 > ~/bag/${1}kg${2}.med.log;
	cat ~/bag/${1}kg${2}.med.log | awk 'BEGIN{v=0;last1=0} {if(sqrt(($1-last1)^2)<0.5)print v=0.9*v+0.1*$1} {last1=$1}' > ~/bag/${1}kg${2}.lpf.log;
	cat ~/bag/${1}kg${2}.vel.log | sed -e '1d' | sed '$d' > ~/bag/${1}kg${2}.velrm.log;
	paste ~/bag/${1}kg${2}.velrm.log ~/bag/${1}kg${2}.med.log ~/bag/${1}kg${2}.lpf.log > ~/bag/${1}kg${2}.tmp.log;
	rm -f ~/bag/${1}kg${2}.vel.log ~/bag/${1}kg${2}.velrm.log ~/bag/${1}kg${2}.med.log ~/bag/${1}kg${2}.lpf.log;
	mv ~/bag/${1}kg${2}.tmp.log ~/bag/${1}kg${2}.vel.log;
	echo -e "\e[1;34mMax v and min v\e[1;37m";
	cat ~/bag/${1}kg${2}.vel.log | awk 'BEGIN{max=0} {if(max<$3) max=$3} END{print "max: ",max}';
	cat ~/bag/${1}kg${2}.vel.log | awk 'BEGIN{min=0} {if(min>$3) min=$3} END{print "min: ",min}';
	echo "plot '~/bag/${1}kg${2}.vel.log' using 1:3 w lp" | gnuplot -persist;
	}

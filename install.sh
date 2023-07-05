#!/bin/bash

## Script to automatically install custom graphical environment and repository configurations https://codeberg.org/80r151a/dotfiles
#
## It is assumed that the installation will be performed on the OpenSuse LEAP distribution. 
## But in theory the script can work successfully on other distributions if the variables are changed to valid values for a particular environment
# 
## Script created 80r151a
## https://codeberg.org/80r151a
## https://github.com/80r151a


## VARIABLES ##

# calling the package manager with the install attribute
installPM="zypper in"

# Xorg server and compatibility metapackage
xorgPack=xorg-x11-server

# X Window System initializer
xinitPack=xinit

# GNU make tool
makePack=make

# GNU Compiler Collection
gccPack=gcc

# Glasgow Haskell Compiler
ghcPack=ghc

# Development files for the Core X11 protocol library
libX11DevPack=libX11-devel

# Development files for the X FreeType library
libXftDevPack=libXft-devel

# Development files for the X11 Xinerama extension
libXineramaDevPack=libXinerama-devel

# Development files for the X Resize-Rotate-Reflection library
libXrandrDevPack=libXrandr-devel

# Development files for the X11 Screen Saver extension library
libXssDevPack=libXss-devel

# Compositing manager for X servers
composerPack=picom

# Screenshot utility
screenshotPack=maim

# Screen lock utility
lockPack=xsecurelock

# Extensible screen saver framework
screensaverPack=xscreensaver

# Xmobar - minimalistic text based status bar
xmobarPack=xmobar

# Directory with downloaded dotfiles repository
dirDotfiles=$HOME/dotfiles

# Directory with xmonad configuration and sources
dirXmonad=$HOME/.config/xmonad/

# Directory with xmobar configuration
dirXmobar=$HOME/.config/xmobar/


## PREPARATION ##

# Installing the required packages
sudo $installPM $xorgPack $xinitPack $makePack $gccPack $ghcPack $libX11DevPack $libXftDevPack $libXineramaDevPack $libXrandrDevPack $libXssDevPauck $composerPack $lockPack $screenshotPack$screensaverPack $xmobarPack

# Creating directories for Xmonad and xmobar configurations
mkdir $dirXmonad $dirXmobar

# Creating a custom bin directory in its home directory
# It will contain the compiled xmonad executable
mkdir $HOME/.local/bin

# Copy xmonad and xmonad-contrib repositories to xmonad config directory for build
git clone https://github.com/xmonad/xmonad $dirXmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib $dirXmonad/xmonad-contrib

# Moving xmonad and xmobar config files from dotfiles to config directories
cp $dirDotfiles/xmonad/xmonad.hs $dirXmonad
cp $dirDotfiles/xmobar/xmobarrc $dirXmobar

# Installing the stack to build and install the xmonad project 
curl -sSL https://get.haskellstack.org/ | sh

# Adding the path to the ~/.local/bin directory to the PATH variable via the bashrc configuration file
# # To run the xmonad executable
echo -e '\nPATH=$PATH:$HOME/.local/bin\nexport PATH' >> $HOME/.bashrc

# Applying the updated bashrc configuration file
source $HOME/.bashrc


## INSTALL XMONAD ##

# Moving to the directory with the xmonad project (aka xmonad configuration directory) to build and install it 
cd $dirXmonad

# Project initialization
stack init

# Installing the project (building and moving the executable to ~/.local/bin)
stack install

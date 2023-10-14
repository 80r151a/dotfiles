#!/usr/bin/env bash
#
## Script to automatically install custom graphical environment and repository configurations https://codeberg.org/80r151a/dotfiles
#
## It is assumed that the installation will be performed on the OpenSuse LEAP distribution. 
## But in theory the script can work successfully on other distributions if the variables are changed to valid values for a particular environment
# 
## Script created 80r151a
## https://codeberg.org/80r151a
## https://github.com/80r151a
stty -echo #Disable keyboard input.
clear
echo -e '\n\n\n

             ___         __        _            __        ____                  _       __ 
            /   | __  __/ /_____  (_)___  _____/ /_____ _/ / /  _______________(_)___  / /_
           / /| |/ / / / __/ __ \/ / __ \/ ___/ __/ __ `/ / /  / ___/ ___/ ___/ / __ \/ __/
          / ___ / /_/ / /_/ /_/ / / / / (__  ) /_/ /_/ / / /  (__  ) /__/ /  / / /_/ / /_  
         /_/  |_\__,_/\__/\____/_/_/ /_/____/\__/\__,_/_/_/  /____/\___/_/  /_/ .___/\__/  
         by 80r151a                                                          /_/    
\n\n\n'
cat <<EOF
There is hope that nothing will break.
But you can exit by pressing ^C before it's too late.
EOF

for i in `seq 5 -1 0` ; do echo -ne "$i\rThe setup will start in... " ; sleep 1 ; done
echo -e '\n'

#### FUNCTIONS ####

# Configuration file search function.
# Returns a value depending on the result
# In the first argument ($1) the variable $PWD is passed.
function configuration_file_search {
        declare configFilePathsCounter=$(find "${1}" -name install.conf | wc -l)
        if [[ "${configFilePathsCounter}" == "1" ]]; then
                # Configuration file found.
                return 0
		
        elif [[ "${configFilePathsCounter}" == "0" ]]; then
                # Configuration file not found.
                return 1
        else
                # Configuration file conflict (more than one configuration file).
                return 3
        fi
}

## The function gets the name of the package manager from the configuration file.
## In the first argument ($1) the variable $configFileHere is passed.
## The local variable $output gets a second argument ($2) to pass the result of the function back...
# to the $packageManagerResult variable that was passed as the second argument.
function getting_name_package_manager {

	# Variable to output the result from the function to the outside.
	declare output="${2}"

	## Variable holding the result of the function.
	## A variable containing the result of the function.
 	## We get the desired part of the config (containing the name of the package manager)...
	# and remove all unnecessary (spaces, # characters, etc.).
	## The result is the name of the package manager.
        declare result=$(sed '{
        /^## PACKAGE MANAGER ##$/,/^## END ##$/!d
        /^$/d
        /#/d
	} ' "${1}" | tr '\n' ' '
	)
	# Checking that the $result variable is not empty.
	if ! test -z "${result}"; 
	then
		## The moment the result ($result) is passed outside...
		# (at first in $output, and since this variable takes ...
		# the function argument $2, when the function is called, the result...
		# will get the variable that will be passed as the argument $2).
		##  Magic!
		eval $output="'${result}'"
		return 0
	else
		return 1
	fi

}


## The function gets the list of packages to install from the configuration file.
## In the first argument ($1) the variable $configFileHere is passed.
## The local variable $output gets a second argument ($2) to pass the result of the function back...
# to the $packagesListResult variable that was passed as the second argument.
function getting_packages_list {

	# Variable to output the result from the function to the outside.
	declare output="${2}"

	## Variable holding the result of the function.
	## We get the desired part of the config (containing a list of packages)... 
	# and remove all unnecessary (spaces, #characters, etc.).
	## The result is a list of packages in a space-separated string.
        declare result=$(sed '{
        /^## INSTALLATION PACKAGE LIST ##$/,/^## END ##$/!d
        /^$/d
        /#/d
	} ' "${1}" | tr '\n' ' '
	)
	# Checking that the $result variable is not empty.
        if ! test -z "${result}";
	then
		## The moment the result ($result) is passed outside...
		# (at first in $output, and since this variable takes ...
		# the function argument $2, when the function is called, the result...
		# will get the variable that will be passed as the argument $2).
		##  There’s magic again!
		eval $output="'$result'"
		return 0
	else
		return 1
	fi
}

# Checking root to use sudo.
# In the first argument ($1) the variable $EUID is passed.
function checks_for_root {

        if [[ "${1}" -eq 0 ]]; then
                 return 0
        else
                 return 1
        fi
}


# Feature for fun. Rotates the spinner during code execution.
# The first argument ($1) is "OFF" to stop the function.
function spin {

	# The PID of this function is stored here.
	declare spinerPID=$!

	# Spinner blades.
	declare -a spinsElement=( '|' '/' '-' '\' )

	# Continue to draw the spinner until we receive the OFF argument.
	while [[ "${1}" != "OFF" ]]
	do
		# Drawing a spinner.
		for i in "${spinsElement[@]}"
		do
			echo -ne "\r${i}"
                        sleep 0.05
		done
	done

	# Erase the last spinner symbol.
	echo -ne "\r "

	# Kill the process of this function.
	kill -SIGTERM "${spinerPID}"

	# Stopping a process causes a stop message. We send it to bit-bucket.
	wait "${spinerPID}" 2>/dev/null

}


#### GLOBAL VARIABLES ####

# Directory with the configuration file. 
declare configFileHere

# Variable with the name of the package manager and the install argument with a list of packages to install.
declare -a installationPipe

# Directory with xmonad configuration and sources.
declare dirXmonad="${HOME}"/.config/xmonad/

# Directory with xmobar configuration.
declare dirXmobar="${HOME}"/.config/xmobar/


#### PREPARATION ####
#
# The ability to run the script as root is prohibited. 
# We check the user using the checks_for_root function and if the user's EUID is 0 (root) we stop the script (exit 130).
checks_for_root "${EUID}"
case "${?}" in
        "0")	## If the script is launched as root, then we terminate the script (function checks_for_root returns 0).

                echo "Installation as root user is not possible. Please run the script as a different user with sudo."
		exit 130
        ;;
        "1")	## The script was not launched from root, continue (function checks_for_root returns 1).
		
		# Finding the path to the configuration file.
		echo "Find and Apply a Configuration File."
		spin & #Drawing a spinner (the spin function is running in the background).
		
		# Loop checking whether variable $configFileHere (variable with the path to the configuration file) is empty.
		while [ -z "${configFileHere}" ];
		do
			# Function to find the path to the configuration file.
			configuration_file_search "${PWD}"
			case "${?}" in
				"0")    ## Configuration file found (this is the only path found.).
					## We place the path to the configuration file in variable $configFileHere.
					spin "OFF" #Turn off the spinner.

					configFileHere=$(find ${PWD} -name install.conf)
				;;
				"1")	## Configuration file not found.
					## Repeat the search (returning to the while loop) or exit using the numbering of actions and the read utility.
					spin "OFF" #Turn off the spinner.
					cat <<EOF
Configuration file not found in directory "${PWD}".
The configuration file must be in the same directory as the executable script.
You can abort the script by typing 0, or move/create the install.conf config file to the script directory and repeat by typing 1.
0: abort the script.
1: retry script search.
EOF
					stty echo
					read -p "Enter:"
					stty -echo
					if [[ "${REPLY}" == "0" ]]; then
						# Stop the script. The user entered 0.
						echo -e "\nScript stopped."
						exit 130
					elif [[ "${REPLY}" == "1" ]]; then
						# Return to the beginning of the while loop. User entered 1.
						continue
					else
						# When entering an invalid value, we return to the while loop to repeat the input.
						echo "Invalid value entered. Try again."
						continue
					fi
				;;
				"3")	## Configuration file conflict (more than one configuration file).
					## Display all paths, number them and offer to select the desired path using the numbering read utility.
					
					spin "OFF" #Turn off the spinner.
					cat <<EOF

More than one config file path found.
You can abort the script by typing 0, or specify a valid path to the install.conf configuration file by selecting the number corresponding to the correct path (1, 2, etc.).
0: abort the script.
EOF
					declare -a configFilePaths[0]=130
					declare count=0
					for i in $(find ${PWD} -name install.conf)
					do
						count=$(( $count + 1 ))
						echo "${count}: ${i}"
						configFilePaths["${count}"]="${i}"
					done

					while :
					do
						stty echo
						read -p "Enter:"
						stty -echo
						if [[ "${REPLY}" == "${count}" ]] || [[ "${REPLY}" < "${count}"  ]]; then
							if [[ "${REPLY}" < "0" ]]; then
								# When entering an invalid value, we return to the while loop to repeat the input.
								echo "Invalid value entered. Try again."
								continue
							elif [[ "${REPLY}" == "0" ]]; then
								echo -e "\nScript stopped."
								exit 130
							else
								configFileHere=$(echo "${configFilePaths[${REPLY}]}")
								break
							fi
						else
							# When entering an invalid value, we return to the while loop to repeat the input.
							echo "Invalid value entered. Try again."
							continue
						fi
					done
				;;
				*)

					spin "OFF" #Turn off the spinner.
					cat <<EOF
Unknown script behavior.
It is impossible to understand if the configuration file install.conf exists and its location path.
EOF
					exit 130
				esac
		done

		echo -e "\n"

		## When the function outputs 0, which means the package manager name value was successfully found, put that value in the install pipe.
		########function################path to file######var returning value##
		getting_name_package_manager "${configFileHere}" packageManagerResult
		case "${?}" in
			"0")	## Place the package manager specified in the configuration file into the installation pipe.
				installationPipe[1]="${packageManagerResult}"
			;;
			"1")	## The package manager could not be read from the configuration file.
				echo "Failed to read config file (not clear package manager)"
			;;
			*)	## Something went wrong. Function getting_name_package_manager returned an unexpected value.Place the package manager specified in the configuration file into the installation pipe.
				echo "Unknown script behavior when defining a package manager"
			esac
		
		##
		######function###########path to file######var returning value##
		getting_packages_list "${configFileHere}" packagesListResult
		case "${?}" in
			"0")	## Place the installation packages specified in the configuration file into the installation channel.
				installationPipe[2]="${packagesListResult}"
			;;
			"1")	## Installation packages could not be read from the configuration file.
				echo "Failed to read configuration file (failed to determine list of packages to install)"
			;;
			*)
				echo "Unknown script behavior when determining the list of packages to install"
			esac
			stty echo
			
			exit 130

		#### INSTALL ####
		
		# Installing packages specified in the configuration file.
		sudo "${installationPipe[@]}"

		# Create directories for Xmonad and xmobar configurations and a local binary directory to house the compiled Xmonad.
		mkdir --verbose "${dirXmonad}" "${dirXmobar}" "${HOME}"/.local/bin

		# Copy xmonad and xmonad-contrib repositories to xmonad config directory for build.
		git clone https://github.com/xmonad/xmonad "${dirXmonad}"/xmonad
		git clone https://github.com/xmonad/xmonad-contrib "${dirXmonad}"/xmonad-contrib

		# Moving xmonad and xmobar config files from dotfiles to config directories.
		cp "${PWD}"/xmonad/xmonad.hs "${dirXmonad}"
		cp "${PWD}"/xmobar/xmobarrc "${dirXmobar}"

		# Installing the stack to build and install the xmonad project.
		curl -sSL https://get.haskellstack.org/ | sh

		# Adding the path to the ~/.local/bin directory to the PATH variable via the bashrc configuration file
		## to run the xmonad executable.
		echo -e '\nPATH=$PATH:$HOME/.local/bin\nexport PATH' >> "${HOME}"/.bashrc

		# Applying the updated bashrc configuration file.
		source "${HOME}"/.bashrc


		### INSTALL XMONAD ###

		# Moving to the directory with the xmonad project (aka xmonad configuration directory) to build and install it.
		cd "${dirXmonad}"

		# Project initialization.
		stack init

		# Installing the project (building and moving the executable to ~/.local/bin).
		stack install


        ;;
        *)
                #
        esac


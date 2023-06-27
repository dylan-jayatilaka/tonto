#!/usr/bin/bash
# Run this script as "sudo ./install-all-you-need.sh"
sudo apt-get update -y
sudo apt-get upgrade -y
sudo apt-get install g++ -y
sudo apt-get install gfortran-9 -y
sudo apt-get install libblas-dev libblas3 -y
sudo apt-get install liblapack-dev -y
sudo apt-get install cmake -y
sudo apt-get install coreutils -y
sudo apt-get install gawk -y
sudo apt-get install tofrodos -y

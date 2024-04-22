#!/usr/bin/bash
# On Ubuntu linux, run this script by typing:
# "sudo ./install-all-you-need.sh"
sudo apt-get update -y
sudo apt-get upgrade -y
sudo apt-get install g++ -y
sudo apt-get install gfortran-9 -y
sudo apt-get install libblas-dev libblas3 -y
sudo apt-get install liblapack-dev -y
sudo apt-get install make -y
sudo apt-get install cmake -y
sudo apt-get install coreutils -y
sudo apt-get install gawk -y
sudo apt-get install tofrodos -y
# Clone 
git clone --recurse-submodules https://github.com/dylan-jayatilaka/tonto.git
cd tonto
# Link basis_sets
sudo ln -s basis_sets  /usr/local/bin
# Build FAST tonto version in fast/
mkdir fast && cd fast
cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=fast
make -j
sudo ln -s tonto /usr/local/bin
cd ..
# Build DEBUG tonto version in debug/
#mkdir debug && cd debug
#cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=debug
#make -j
#sudo ln -s tonto /usr/local/bin/tonto.debug
#cd ..

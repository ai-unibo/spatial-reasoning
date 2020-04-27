# spatial-reasoning

### Pre-requisites
spatial-reasoning needs the following libriaries to work properly:
* OpenCV 4.3.0
* Swipl 
* Tesseract OCR 4.00

#### Installing pre-requisites
The following steps will guide you trough the installation of the correct versions of the required libraries on a Debian distro.
##### OpenCV 
You need to build version 4.3.0 from source as follows.
```
sudo apt install build-essential cmake git pkg-config libgtk-3-dev \
    libavcodec-dev libavformat-dev libswscale-dev libv4l-dev \
    libxvidcore-dev libx264-dev libjpeg-dev libpng-dev libtiff-dev \
    gfortran openexr libatlas-base-dev python3-dev python3-numpy \
    libtbb2 libtbb-dev libdc1394-22-dev
mkdir ~/opencv_build && cd ~/opencv_build
git clone https://github.com/opencv/opencv.git 
git clone https://github.com/opencv/opencv_contrib.git
```
Be sure to clone version 4.3.0. Alternatively you can download it from [here](https://github.com/opencv/opencv/releases/tag/4.3.0).
```
cd ~/opencv_build/opencv
mkdir build && cd build
cmake -D CMAKE_BUILD_TYPE=RELEASE \
    -D CMAKE_INSTALL_PREFIX=/usr/local \
    -D INSTALL_C_EXAMPLES=ON \
    -D INSTALL_PYTHON_EXAMPLES=ON \
    -D OPENCV_GENERATE_PKGCONFIG=ON \
    -D OPENCV_EXTRA_MODULES_PATH=~/opencv_build/opencv_contrib/modules \
    -D BUILD_EXAMPLES=ON ..
```
Compile opencv concurrently using the number of processors you have. Reduce the number if it gives you some errors. The lower the number is, the slower the process will be. To compile on 4 processors:
```
make -j4 
sudo make install
pkg-config --modversion opencv4 
sudo ln -s /usr/local/include/opencv4/opencv2/ /usr/include/opencv2 
```
##### swipl stable release
Install the latest stable release as follows. This code has been tested with version 8.0.3
```
sudo apt-get install software-properties-common
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install swi-prolog
```
##### Tesseract-OCR
Install tesseract library for the required languages.
```
sudo apt-get install tesseract-ocr-eng
sudo apt-get install tesseract-ocr-ita
sudo apt-get install tesseract-ocr-ita-old
sudo apt-get install tesseract-ocr-fra
sudo apt install libtesseract-dev
```
Remember to set the TESSDATA_PREFIX environment variable to your tessdata folder, e.g.:
```
export TESSDATA_PREFIX=/usr/share/tesseract-ocr/4.00/tessdata 
```

### Compile the C++ code
Generate the .so library with:
```
cd primitives
g++ -shared -o prologPrimitives.so prologPrimitives.cpp win.cpp \
`pkg-config opencv4 --cflags --libs` \
`pkg-config tesseract --cflags --libs` \
`pkg-config swipl --cflags --libs` -fpic
```

### Run
Run spatial-reasoning on a mathematical puzzle as follows.
```
cd prologSrc
swipl
?- ['load.pl'].
?- solver:demo('<gameid>').
```
where `<gameid>` is one of the games in the `image` folder (without .png extension).
You can also set the prolog debug flag to see the steps of the spatial reasoning process.
```
set_prolog_flag(debug, true).
```

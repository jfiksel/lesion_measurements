# Lesion Measurment Shiny App

To run this app locally, simply add both the ui.R and server.R to the same directory, load the shiny library, the use the command runApp(). The app can also be accessed on https://jhubiostatistics.shinyapps.io/lesion_measurements/. 

The purpose of this Shiny application is to aid researchers in determining the percentage of an image, or subimage, that is taken up by a particular feature. The example that inspired the creation of this app is to find the percentage of a paw taken up by a lesion. This problem is complicated by the fact that images may not be consistent from sample to sample, so one must take into account how much of the image is taken up by background. For example, if one calculates the percentage of the full image taken up by a lesion, rather than the percentage of the paw taken up by a lesion, simply zooming out will artificially reduce the percentage of the image taken up by a lesion. 

The current version of the app relies on the user to crop out the background and to identify the feature of interest. In the future, fully automatic methods will be implemented that will be able to do these steps for high-quality images. 

To begin, upload the image of interest. To crop out the background, simply press "Trace paw" and begin clicking around the perimeter of the paw. If you wish to zoom in on a region, press "Pause tracing", click and drag the mouse to create a box that you would like to zoom in on, and then double click on the box. To continue tracing, you must press "Trace paw" again, and you can zoom out by again pausing the tracing and then double clicking anywhere on the image. Do not attempt to zoom in on an image while in tracing mode. As you trace around the paw, the area inside of the red lines will show you which part of the image would be included in your tracing. You can reset the tracing any time by clicking "Reset tracing". Once you are satisfied with your background cropping, click "Crop Background". 

After you have cropped out the background of image, switch to the "Calculate Tumor Size" tab. The same steps as above can be taken to find the lesions, or other features of interest, except with a few additions. First, there may be multiple, non-connecting lesions. To deal with this, once you have traced one lesion, press "Finishing Tracing Tumor", and then click on "Trace Tumor" to begin tracing another lesion. If you want to reset the current tracing of a single lesion, simply press "Resent Current Tumor Tracing." If you want to reset all lesion tracings, press "Reset All Tumor Tracings." The percentage of a paw taken up by lesions can be updated at any time by pressing "Calculate Lesion Percentage". You must press "Finishing Tracing Tumor" after tracing the most recent lesion to have that lesion be included in the calculation. 

Because the main use of this app will be for researchers to compare the effects of a treatment between groups, future updates of this app will store the results for each image in a data frame, which can be downloaded as a .csv file by the user. The user will also be able to supply how they would like each image labeled in the data frame.

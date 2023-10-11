Alcinoe
=======

Alcinoe is a library of visual and non-visual components for 
Delphi. These components can be used in commercial, shareware, 
freeware, and open-source projects at no cost. Alcinoe is 
compatible with Delphi Alexandria 11.3 (Patch #1). If you find 
this project useful, please consider giving it a 'star' on 
GitHub. It doesn't cost anything, but it helps to promote 
the code.

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-24.jpg?raw=true" alt="like" />


Example of app made with Alcinoe
--------------------------------

<a href="https://www.youtube.com/watch?v=IJzEuZTSXDI&vq=hd2160">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/youtube_kiskis2.jpg?raw=true" alt="KisKis" />
</a>


Embarcadero Quality Reports
---------------------------

Please vote for the resolution of these quality reports. The 
lack of resolution on these issues from Embarcadero has compelled 
us to patch the original Delphi source files:

* [Project option to define where to look/create the LaunchScreen.TemplateiOS directory](https://quality.embarcadero.com/browse/RSP-33503)
* [Support for the new Android Splash Screen standard](https://quality.embarcadero.com/browse/RSP-39331)
* [for android compilation, need to use aapt2 instead of aapt](https://quality.embarcadero.com/browse/RSP-27606)
* [Root Class Not Found' Error in Delphi IDE When Opening a Class Inherited via an Include Directive](https://quality.embarcadero.com/browse/RSP-42021)
* [Their is no propagation of mouse event under Firemonkey](https://quality.embarcadero.com/browse/RSP-24397)
* [Performance Issue - Comparing Equality Between Two Strings](https://quality.embarcadero.com/browse/RSP-42011)
* [Introduce IsVisibleObject function for improved optimization on TScrollBox](https://quality.embarcadero.com/browse/RSP-42357)
* [BeginUpdate/Endupdate block with add or remove of child objects : misconception](https://quality.embarcadero.com/browse/RSP-21013)
* [The width and height of a TContext3D object must be defined as single-precision, not as integers](https://quality.embarcadero.com/browse/RSP-41516)
* [TTextLayout.PositionAtPoint / TTextLayoutD2D.DoPositionAtPoint totally broken in Alexandria](https://quality.embarcadero.com/browse/RSP-39734)
* [Regression in Alexandria: FMX.StrokeBuilder.pas Revamp Leads to TARC Drawing Issues](https://quality.embarcadero.com/browse/RSP-41618)
* [GL_TEXTURE_EXTERNAL_OES not supported](https://quality.embarcadero.com/browse/RSP-16830)
* [Make CanvasHelper of TCanvasGpu public](https://quality.embarcadero.com/browse/RSP-18797)
* [Make that TTexture can define a material (GLSL shader) to use](https://quality.embarcadero.com/browse/RSP-23501)
* [On iOS we need to pass options: PNSDictionary with the message TApplicationEvent.FinishedLaunching](https://quality.embarcadero.com/browse/RSP-40351)
* [AVPlayerItem miss the function addOutput](https://quality.embarcadero.com/browse/RSP-16853)
* [CLVisit is missing from the implementation of CLLocationManager](https://quality.embarcadero.com/browse/RSP-18621)
* [Miss kCIInputImageKey in iOSapi.CoreImage.pas](https://quality.embarcadero.com/browse/RSP-19878)
* [Miss some definition in IOSapi.Security](https://quality.embarcadero.com/browse/RSP-20279)
* [Effect ProcessTexture is not working and function TFilter.InputTexture: TTexture do unecessary work](https://quality.embarcadero.com/browse/RSP-20825)
* [TVirtualKeyboardAndroid.GetVirtualKeyboardState not accurate](https://quality.embarcadero.com/browse/RSP-24737)
* [Add NSPersonNameComponents in iOSapi.Foundation.pas](https://quality.embarcadero.com/browse/RSP-28096)
* [FilterServerSupportFunctionProc WinApi badly converted in Delphi](https://quality.embarcadero.com/browse/RSP-33754)
* [Comparison of 2 interfaces result in bad behavior in TCommonCustomForm.SetHovered and similar](https://quality.embarcadero.com/browse/RSP-36612)
* [In android, TFormRender must use JChoreographer_FrameCallback not JRunnable](https://quality.embarcadero.com/browse/RSP-38660)
* [Need JNotificationClass.AUDIO_ATTRIBUTES_DEFAULT and JNotificationChannel.setSound](https://quality.embarcadero.com/browse/RSP-39511)
* [Declaration of JLocationListener miss somes methods](https://quality.embarcadero.com/browse/RSP-41343)
* [Miss CLLocationManager.accuracyAuthorization in iOSapi.CoreLocation.pas](https://quality.embarcadero.com/browse/RSP-41352)
* [Miss constant kCLLocationAccuracyReduced in iOSapi.CoreLocation.pas](https://quality.embarcadero.com/browse/RSP-41388)
* [iOS/OSX: Declaration for CLRegion.initCircularRegionWithCenter is incorrect](https://quality.embarcadero.com/browse/RSP-15717)
* [Missing function declaration for maximumFramesPerSecond in iOSapi.UIKit.UIScreen](https://quality.embarcadero.com/browse/RSP-42455)

    
Install Alcinoe
---------------

To set up Alcinoe, first execute [CompileAll.bat](https://github.com/MagicFoundation/Alcinoe/tree/master/CompileAll.bat). This 
batch file handles a series of tasks: it retrieves and 
patches the original Delphi source code, downloads the 
necessary iOS/Android libraries, constructs the Alcinoe 
JAR files, builds the BPL (Borland Package Library), 
compiles tools from the [{alcinoe}\Tools](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/) directory, and 
finally compiles all demos in the [{alcinoe}\Demos](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/) 
directory. Please be aware that some demos utilize 
components from [devexpress](https://www.devexpress.com/).

If your goal is to use only the non-visual components 
from Alcinoe, no further installation steps are necessary. 
Just ensure to include [{alcinoe}\Source](https://github.com/MagicFoundation/Alcinoe/tree/master/Source) in your project's 
search path.

For those wanting to use Alcinoe's visual components 
during design time, a few more steps are required. You 
will need to install the BPL: Open Delphi, navigate to 
Component > Install Packages... and select the BPL from 
[{alcinoe}\Libraries\bpl\Alcinoe\Win32\\{YourDelphiVersion}\Alcinoe{YourDelphiVersion}.bpl](https://github.com/MagicFoundation/Alcinoe/tree/master/Libraries/bpl/Alcinoe/Win32). 
Also, remember to include both [{alcinoe}\Source](https://github.com/MagicFoundation/Alcinoe/tree/master/Source) and all 
subdirectories from [{alcinoe}\Embarcadero\\{YourDelphiVersion}](https://github.com/MagicFoundation/Alcinoe/tree/master/Embarcadero/) 
in your search path.


Update Alcinoe
--------------

We occasionally need to rename units, classes, and 
functions. To assist you in automatically updating 
these names in your project to align with the latest 
version of Alcinoe, we offer a tool called CodeRenaming. 
You can find this tool at the following link: [CodeRenaming](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/CodeRenaming)


Propose a change using GitHub
-----------------------------

If you need to suggest a change to the Alcine directory, 
please follow these steps:

1. To make changes to the Alcinoe repository, you'll need 
   to fork it. Don't worry, forking simply involves 
   creating a copy of the Alcinoe source code that you can 
   edit as you wish.  
   
   ![Fork](https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-27.jpg?raw=true)

2. Make the necessary edits to the source code, and then 
   commit and push the changes to your forked repository.

3. Once you're satisfied with your changes, submit your 
   proposal. To do so, go to your forked repository, select 
   'Contribute,' and then choose 'Open a pull request.'  
   
   ![Contribute](https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-28.jpg?raw=true)


AndroidMerger: Integrate AAR SDK in FMX Android app
---------------------------------------------------
                                 
<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-26.jpg?raw=true" alt="video player for FireMonkey" width="540" style="width:540px;"/>

An Android library, also called as Android Archive, includes 
everything you need to build an app like source files, 
resource files, manifest etc. This is the reason why AARs are 
different from JARs. AARs can contain resource files as well 
other than compiled byte code. 

Adding such library to Delphi project is long and convoluted 
process consisting of extracting resources from library, 
manually adding them to Delphi deployment files, compiling 
R.Java class, checking dependancies, etc.

With AndroidMerger all of the above can now be done 
automatically in a single command line. In brief 
AndroidMerger will:
 
* Use graddle or internal implementation to list all dependencies.
* Download libraries and dependancies from local or central maven repository.
* Merge the resources of all AARs inside a single directory.
* Merge the AndroidManifest files of all AARs inside AndroidManifest.template.xml.
* Merge google-services.json in the resources of the project.
* Create the R.jar with all resource IDs using aapt or aapt2.
* Update the project file (.dproj) to include all resources.
* Generate the Delphi native bridge file from the Java libraries.

Learn more at [{alcinoe}/Tools/AndroidMerger](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/AndroidMerger)


DeployMan
---------
                                 
Simplify the deployment of files and folders for iOS and 
Android apps written in Delphi. It is especially useful 
if you need to deploy a lot of files, such as 3rd party SDKs. 
Learn more at [{alcinoe}/Tools/DeployMan](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/DeployMan)
  

DeployProjNormalizer
--------------------

Create a new deployproj file from scratch using the dproj 
as a reference, and then normalize it by ordering the nodes 
in such a way that you can compare different revisions with 
diff comparison tools. Learn more at 
[{alcinoe}/Tools/DeployProjNormalizer](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/DeployProjNormalizer)


DProjNormalizer
---------------

Order the nodes in a DProj file to ensure consistency 
across commits, making it easier to compare differences. 
Learn more at [{alcinoe}/Tools/DProjNormalizer](https://github.com/MagicFoundation/Alcinoe/tree/master/Tools/DProjNormalizer)


Opengl video player for FireMonkey
----------------------------------

ALVideoPlayer renders video to a texture, which is 
important for integrating the video into a Delphi form and 
supporting Z-ORDER for placing controls on top of it. 
Official Delphi video players are just native video player 
windows on top of the form and do not support Z-ORDER.

For Android, Alcinoe uses ExoPlayer, which supports 
features such as dynamic adaptive streaming over 
HTTP (DASH), HLS, SmoothStreaming, and common encryption 
that are not supported by MediaPlayer. ExoPlayer is 
designed to be easy to customize and extend. For iOS, I use 
AVPlayer with support for HLS, similar to ExoPlayer.

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-1.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-2.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-3.jpg?raw=true" alt="video player for FireMonkey" width="270" style="width:270px;"/>
</p>

Learn more at [{alcinoe}/Source/Alcinoe.FMX.VideoPlayer.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.FMX.VideoPlayer.pas) and you can also start exploring this feature 
with the demo located at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls) 
  

WebRTC Delphi wrapper
---------------------

WebRTC (Web Real-Time Communications) is a technology which 
enables Web applications and sites to capture and optionally 
stream audio and/or video media, as well as to exchange arbitrary 
data between browsers and mobile applications without requiring 
an intermediary. The set of standards that comprises WebRTC 
makes it possible to share data and perform teleconferencing 
peer-to-peer, without requiring that the user installs plug-ins 
or any other third-party software.

<p align="center">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-25.jpg?raw=true" alt="Delphi WebRTC wrapper" />
</p>

With the TALWebRTC component, you can easily incorporate video 
and audio chat into your applications, giving your users a more 
interactive and immersive experience! Learn more at 
[{alcinoe}/Demos/ALLiveVideoChat](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALLiveVideoChat)
  

Firemonkey native iOS/Android TEdit and TMemo
---------------------------------------------

The idea is to combine FireMonkey controls with native 
platform controls when the functionality of a specific 
control becomes difficult to implement solely with 
FireMonkey (such as web browser, edit, memo, date picker, 
etc.).

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-20.png?raw=true" alt="Drawing" width="240" style="width:240px;"/>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-21.png?raw=true" alt="Drawing" width="240" style="width:240px;"/>
</p>

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-22.png?raw=true" alt="Drawing" width="457" style="width:457px;"/>

Learn more at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls)


Fast/double-buffered FireMonkey controls with native draw
---------------------------------------------------------

* Rectangle
* Circle
* Text (Can also draw html text on iOS/android/win/macOS)
* Glyph
* etc.

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-23.png?raw=true" alt="TALRectangle" width="600" style="width:600px;" />

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-8.jpg?raw=true" alt="TALRectangle" width="600" style="width:600px;" />

#### The fact ####

Painting FireMonkey controls can be slow at times, which 
can affect the scrolling fluidity. For instance, the basic 
TRectangle with round corners takes approximately 3ms to 
paint. If you have 20 visible TRectangles on your screen, 
it would take around 60ms to repaint the entire screen 
(assuming you only have TRectangles and not other controls 
like TLabel or TCheckbox). Doing the math, if it takes 
100ms to repaint the screen, you can only achieve around 10 
frames per second (in reality, it would be much less). As a 
result, scrolling can't be smooth.

#### The solution #### 
 
I didn't want to rebuild the FireMonkey controls, which 
would have been a huge job for me. Instead, I tried to find 
an intermediate solution by adding a "double-buffered" 
property to the FireMonkey controls. This approach involves 
painting the control on a buffer stored directly in the GPU 
memory (through TTexture) instead of repainting the controls 
for every single pixel move of the scrollbox. When the 
system asks me to repaint the controls, I simply redraw the 
buffer TTexture, rather than calling the paint algorithm 
again.

#### The results ####  

As I mentioned before, it used to take 3ms just to paint a 
simple TRectangle with round corners. With my 
double-buffered property, it now takes around 0.1ms! As a 
result, the scrolling looks much more fluid now!

#### OpenGL draw => Replaced by native iOS/android draw ####

Most of the basic shapes in FireMonkey, such as TRectangle 
and TCircle, use OpenGL for drawing. However, this is not 
very efficient - for example, to draw a circle under 
OpenGL, you actually need to draw 50 triangles. This often 
results in poor quality. Drawing a round rectangle is even 
worse because it requires calculating the path and then 
drawing it, which is much slower than drawing a circle.

Another problem is that all of these shapes depend on the 
Form.quality setting. If you set form.quality to high 
quality, then everything you draw on the canvas will be 
anti-aliased, which can be problematic - for example, an 
image may be anti-aliased even if you don't want it to be. 
On the other hand, if you set form.quality to high 
performance, then the draw will be very rough, with no 
anti-aliasing.

To resolve these issues, I built the buffer of my control 
using the native Android/iOS API. In this way, we can have 
high-quality drawing at high speed without being dependent 
on the form.quality setting.

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-9.jpg?raw=true" alt="TALCircle" />
  
You can start exploring this feature with the demo located 
at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls)


Improved FireMonkey controls
----------------------------

* ScrollBox
* TabControl
* RangeTrackBar

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-5.jpg?raw=true" alt="RangeTrackBar" width="600" style="width:600px;"  />

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/tabcontrol.gif?raw=true" alt="tabcontrol" width="360" style="width:360px;"/>

Learn more at [{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls)
  

Confetti Falling Animation
--------------------------

ALConfetti is a vanilla Delphi library for creating a configurable, 
high-performance confetti falling animation. Learn more at 
[{alcinoe}/Demos/ALConfetti](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALConfetti)

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/confetti.gif?raw=true" alt="confetti" width="564" style="width:564px;" />


Interpolated Animation
----------------------

The TALAnimation component is a refined iteration of Delphi's foundational
TAnimation object, meticulously tailored for mobile platforms. By forgoing the
traditional Timer mechanism and instead adopting platform-specific technologies,
this component offers a vastly improved animation experience for mobile users.
On Android, animations are seamlessly integrated with the Choreographer,
ensuring they sync perfectly with the device's refresh rate. Meanwhile, on iOS,
the precision of DisplayLink is harnessed, leading to optimized and fluid
animation rendering. Beyond these foundational changes, one of the most notable
enhancements is the capability to support custom interpolation algorithms. This
offers developers the flexibility to design unique and intricate animation
patterns, moving beyond the traditional ease-in or ease-out sequences.

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/interpolated1.gif?raw=true" alt="Interpolated Animation" width="320" style="width:320px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/interpolated2.gif?raw=true" alt="Interpolated Animation" width="320" style="width:320px;"/>
</p>

Learn more at 
[{alcinoe}/Demos/ALAnimation](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALAnimation)
  

SpringForce Animation
---------------------

Inspired by Android's SpringForce, the TALSpringForceAnimation Component brings
the intrigue of physics-based animations to the Delphi platform. This component
captures the real-world dynamism of objects influenced by spring mechanics. The
resulting animations are ones that stretch, bounce, and settle, mirroring
real-world behaviors and offering a tangible sense of realism to users.
Developers have the added advantage of being able to adjust various physical
properties of the spring, such as its stiffness and damping ratio. This ensures
that a broad spectrum of animation behaviors can be realized, catering to the
specific nuances of different applications.
  
<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/springforce.gif?raw=true" alt="springforce" width="320" style="width:320px;" />

Learn more at 
[{alcinoe}/Demos/ALAnimation](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALAnimation)
  

Scrolling Engine
----------------

TALOverScroller and TALVelocityTracker are essential components of the
TALScrollEngine, playing crucial roles in refining user interface interactions.
While TALOverScroller is designed to animate and manage actions like scrolling
and flinging, offering a decelerating animation as users scroll past a view's
edge to indicate the boundary, TALVelocityTracker measures the velocity of
touch events, helping to determine the speed and direction of user gestures.
Together within the TALScrollEngine, they elevate the overall user experience
by offering smooth animations and intuitive touch feedback.

Building a scrolling engine is a complex endeavor, requiring expertise in
various domains, including physics. Rather than starting from scratch and
potentially reinventing the wheel, we looked towards proven solutions. To this
end, we utilized the robust and well-tested code from Android's VelocityTracker
and OverScroller. By translating their Java and C++ codes into Delphi, we've
ensured that the resultant TALScrollEngine not only meets but exceeds the
standard for scrolling dynamics. Leveraging the reliability and efficiency of
Android's scrolling mechanisms, the TALScrollEngine offers Delphi developers a
top-notch scrolling experience, rooted in established and trusted technologies.

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/scrollingengine.gif?raw=true" alt="scrollingengine" />

Learn more at 
[{alcinoe}/Demos/ALFmxControls](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls)
  

Firebase cloud messaging
------------------------

Delphi implementation of the latest version of Firebase 
cloud messaging (FCM) with HTTP V1 protocol that will 
permit you to send alert notifications with images in 
Android and iOS. Learn more at 
[{alcinoe}\Demos\ALNotificationService](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALNotificationService)


GeoPositioning for Android/iOS
------------------------------

The TALGeoPositionSensor component is a Delphi component that grants access to
location services on iOS and Android devices. It enables the retrieval of the
device's current location, and can provide location updates as the device's
location changes. It supports a range of location providers, including GPS,
cellular network triangulation, and Wi-Fi positioning.

Aside from granting access to location services, TALGeoPositionSensor also
automates the process of acquiring the user's permission to use the location
sensor on both iOS and Android devices. The component can handle situations
where the user has previously refused access to their location. The
TALGeoPositionSensor component provides a comprehensive solution for developers
seeking to integrate location-based functionality into their apps without
having to worry about low-level implementation details. Learn more at 
[{alcinoe}\Demos\ALGeoPositionSensor](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALGeoPositionSensor)


Google OAuth 2.0 Access Token
-----------------------------

Google APIs use the OAuth 2.0 protocol for authentication 
and authorization. You can use the function 
ALGenerateGoogleOAuth2AccessToken to generate an OAuth 2.0 
Access Token. Learn more at 
[{alcinoe}/Source/Alcinoe.Cipher.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.Cipher.pas) 
  

Android/iOS VKontakte/Facebook SDK login
----------------------------------------

The VKontakte/Facebook SDK for Android enables users to 
sign into your app using VKontakte/Facebook Login. Once 
logged in, users can grant permissions to your app, 
allowing you to retrieve information or perform actions on 
VKontakte/Facebook on their behalf. Learn more at 
[{alcinoe}\Demos\ALFacebookLogin](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFacebookLogin)


Photo Editor Filters for Android/iOS
------------------------------------

With TALColorAdjustEffect, you can apply gorgeous photo 
filters that enhance your images with just one tap. Make 
your photos beautiful and expressive in just minutes! 
Learn more at [{alcinoe}\Demos\ALFmxFilterEffects](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxFilterEffects)

<p align="left">
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-18.jpg?raw=true" alt="video player for FireMonkey" width="320" style="width:320px;"/>
  &nbsp;&nbsp;&nbsp;
  <img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-19.jpg?raw=true" alt="video player for FireMonkey" width="320" style="width:320px;"/>
</p>


Streamlining Object Initialization with TALInit
-----------------------------------------------

In the constant evolution of software development, we often find ourselves
seeking ways to reduce boilerplate code and enhance the maintainability of our
projects. One such instance where boilerplate can become cumbersome is in the
initialization of class fields. The traditional method involves explicitly
setting each field's value in the constructor, which can be tedious, especially
for classes with numerous fields. Enter TALInit—a feature that allows
automatic initialization of object fields based on their attributes.

#### The Traditional Way ####

In the typical approach, developers manually initialize object fields in the
constructor. Take the following class as an example:

```
    TAutoInitObject = class(TObject)
    public
      CharValue: Char;
      ChildObject: TChildObject;
    public
      constructor Create; virtual;
      destructor Destroy; override;
    End;
```

Here, each field is initialized in the Create constructor:

```
  constructor TAutoInitObject.create(const aOwner: Tform1; const AAutoInit: Boolean);
  begin
    CharValue := 'A';
    ChildObject := TChildObject.create;
    ChildObject.Name := 'AnObject';
    ChildObject.Value := 12.2;
  end;

  destructor TAutoInitObject.Destroy;
  begin
    ALFreeandNil(ChildObject);
    inherited;
  end;
```

While this method offers precise control, it can become tedious for large
classes with numerous fields.

#### The TALInit Way ####

Imagine having a mechanism that not only automates this but is also as fast as
the traditional way - yes, you read that right. TALInit achieves this
remarkable feat.

```
  TAutoInitObject = class(TObject)
  public
    [TALInit('A')]
    CharValue: Char;
    [TALInit('Name:AnObject;Value:12.2')]
    ChildObject: TChildObject;
  End;
```

By using custom attributes, every field within the object can be automatically
initialized based on its corresponding attribute. This eliminates the need for
manually setting each field within the constructor. The above snippet showcases
just how concise and readable object field initialization can become with
TALInit.

#### Performance - A Game Changer: ####

One of the strongest advantages of using TALInit is its performance. When
introducing automation, a natural concern is the overhead that might come with
it. However, TALInit is designed to be as efficient as the traditional way
of initializing fields. This means developers can enjoy the convenience
without having to worry about any hidden costs in execution time.

Learn more at [{alcinoe}/Alcinoe/tree/master/Demos/ALRTTI](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALRTTI)


Json Parser
-----------

TALJSONDocument is a Delphi parser/writer for JSON/BSON 
data formats. It supports both DOM and SAX parsers. (Note 
that a better name for SAX could be SAJ for Simple API for 
JSON instead of Simple API for XML, but as the concept of 
SAX is well-known, I will keep this name.) TALJSONDocument 
also supports BSON format and uses a syntax similar to 
TALXMLDocument/TXMLDocument. Additionally, TALJSONDocument 
can export Json/Bson data to TALStringList.

Example :

```
    {
      _id: 1, // comments
      name: { first: "John", last: "Backus" },
      birth: new Date('1999-10-21T21:04:54.234Z'),
      contribs: [ "Fortran", "ALGOL", "Backus-Naur Form", "FP" ],
      awards: [
                { award: "National Medal of Science",
                  year: 1975,
                  by: "National Science Foundation" },
                { award: "Turing Award",
                  year: 1977,
                  by: "ACM" }
              ],
      spouse: "",
      address: {},
      phones: []
    }
```

To access the document nodes :

```
    MyJsonDoc.GetChildNodeValueInt32('_id', 0{default if node not exists});
    MyJsonDoc.GetChildNodeValueText(['name','first'], ''{default if node not exists});
    MyJsonDoc.GetChildNodeValueDateTime('birth', Now{default if node not exists});
```

Learn more at [{alcinoe}/Source/Alcinoe.JSONDoc.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.JSONDoc.pas)
  

ImageMagick wrapper for Delphi
------------------------------

Use ImageMagick® to create, edit, compose, or convert bitmap 
images. It can read and write images in a variety of formats 
(over 200) including PNG, JPEG, GIF, HEIC, TIFF, DPX, EXR, 
WebP, Postscript, PDF, and SVG. Use ImageMagick to resize, 
flip, mirror, rotate, distort, shear and transform images, 
adjust image colors, apply various special effects, or draw 
text, lines, polygons, ellipses and Bézier curves.

Example :

```
    var LWand: PMagickWand;
    begin
    
      //Create the ImageMagick Library
      alCreateImageMagickLibrary({alcinoe} + '\Libraries\dll\imagemagick\win32\imagemagick', min(2, System.CPUCount){aThreadLimit});
      try
      
        //create the wand pointer
        LWand := ALImageMagickLib.NewMagickWand;
        try
      
          //load the image
          if ALImageMagickLib.MagickReadImage(LWand, pansiChar(aInputFilename)) <> MagickTrue then RaiseLastMagickWandError(LWand);
          
          //Set the compression quality
          if ALImageMagickLib.MagickSetImageCompressionQuality(LWand,80) <> MagickTrue then RaiseLastMagickWandError(LWand);
      
          //autorate the image
          if ALImageMagickLib.MagickAutoOrientImage(LWand) <> MagickTrue then RaiseLastMagickWandError(LWand);
      
          //Resize the image using the Lanczos filter
          if ALImageMagickLib.MagickResizeImage(LWand, 640, 480, LanczosFilter) <> MagickTrue then RaiseLastMagickWandError(LWand);
             
          //save the image
          ALImageMagickLib.MagickWriteImage(LWand, pansiChar(aOutputFilename));
      
        finally
          ALImageMagickLib.DestroyMagickWand(LWand);
        end;
    
      finally
        alFreeImageMagickLibrary;
      end;

    end;
```

Learn more at [{alcinoe}/Source/Alcinoe.ImageMagick.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.ImageMagick.pas)


MongoDb client
--------------

This is a Delphi driver (with connection pooling) for 
accessing a MongoDB server. Connection pooling is a cache 
of database connections that are maintained so that they 
can be reused when future requests to the database are 
required. After a connection is created in connection 
pooling, it is placed in the pool and is used over again so 
that a new connection does not have to be established. If 
all the connections are being used, a new connection is 
made and added to the pool. Connection pooling also reduces 
the amount of time a user must wait to establish a 
connection to the database. Learn more at 
[{alcinoe}/Source/Alcinoe.MongoDB.Client.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.MongoDB.Client.pas)


WebSocket client
----------------

The WebSocket client for Delphi is implemented on top of 
WinHTTP. WebSocket is a communication protocol that enables 
two-way interactive communication sessions between a user's 
browser and a server. This allows you to send messages to a 
server and receive event-driven responses without having to 
poll the server for a reply. Learn more at 
[{alcinoe}\Demos\ALWinHTTPWebSocketClient](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALWinHTTPWebSocketClient)


Fast TStringList
----------------

TALStringList works the same as Delphi's TStringList, 
except that it allows you to search for a name=value 
using a quicksort algorithm when the list is sorted. 
Additionally, TALStringList uses a locale-independent 
algorithm (based on the 8-bit ordinal value of each 
character) instead of the AnsiCompareText and 
AnsiCompareStr used by Delphi's TStringList. As a 
result, sorting in TALStringList is up to 10 times 
faster than in Delphi's TStringList. Furthermore, 
TALStringList is not a Unicode TStringList, but a 100% 
Ansi StringList. You can start exploring this feature 
with the demo located at 
[{alcinoe}\Demos\ALSortedListBenchmark](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALSortedListBenchmark)


PHP runner
----------

ALPHPRunnerEngine is a simple yet useful component that 
allows you to easily use PHP (any version) as a scripting 
language in Delphi applications. With ALPhpRunnerEngine, 
you can execute PHP scripts within the Delphi program 
without the need for a web server. The component uses 
the CGI/FastCGI interface (php-cgi.exe) of PHP to 
communicate with the PHP engine. Learn more at 
[{alcinoe}\Demos\ALPhpRunner](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALPhpRunner)


Memcached Client
----------------

What is Memcached? Free & open source, high-performance,
distributed memory object caching system, generic in
nature, but intended for use in speeding up dynamic web
applications by alleviating database load. Learn more at 
[{alcinoe}/Source/Alcinoe.MemCached.Client.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.MemCached.Client.pas)


GSM component
-------------

The TAlGSMComm component allows you to implement SMS text 
messaging using the text-mode interface defined in the GSM 
Technical Specification 07.05. Learn more at 
[{alcinoe}/Source/Alcinoe.GSMComm.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.GSMComm.pas)


SQLite3 Client
--------------

Query the SQLite3 database and get the results in XML format
or in JSON/BSON format. Learn more at 
[{alcinoe}/Source/Alcinoe.Sqlite3.Client.pas](https://github.com/MagicFoundation/Alcinoe/tree/master/Source/Alcinoe.Sqlite3.Client.pas)


And much more
-------------

* CGI runner 
* Http Client (WinInet/WinHTTP)
* MySQL Client 
* NNTP Client
* POP3 Client
* SMTP Client
* Xml Parser 
* etc ...


DELPHI D2009+ (UNICODE)
-----------------------

No mistake, Unicode was necessary for a product like Delphi. 
However, the way Embarcadero decided to implement it is 
questionable. They chose to migrate from 8-bit strings to 
16-bit strings instead of implementing Unicode through 
8-bit strings (UTF-8). This made the migration of some 
Delphi applications prior to D2009 very difficult, especially 
for applications that assumed that strings were 8-bit. 
Here's a very good article about why you should avoid 
using UTF-16: [utf8everywhere.org](http://www.utf8everywhere.org/)

Starting with D2009, AnsiString now has a codepage, 
and some transliteration (OldCodePage => UTF-16 => 
NewCodePage) will occur when assigning one AnsiString 
with a different codepage to another AnsiString with 
a different codepage. To avoid this, it's important to 
always set the project option to the code page you want 
(e.g., 65001 for UTF-8) and also to call 
SetMultiByteConversionCodePage(CP_UTF8) at the beginning 
of the program. Additionally, it's crucial to avoid using 
two different string types (e.g., UTF8String and 
AnsiString) even if they have the same codepage because 
the compiler, at compile time, doesn't know that the 
codepage is the same and will do a transliteration 
(e.g., MyAnsiStringUTF8 := MyUTF8String will result in 
UTF-8 => UTF-16 => UTF-8). This is why we use only 
AnsiString in all our code instead of UTF8String (even 
when we assume that the string contains only UTF-8 
characters) to avoid these transliterations. Always follow 
the rule of using only AnsiString with 
SetMultiByteConversionCodePage(CP_UTF8).
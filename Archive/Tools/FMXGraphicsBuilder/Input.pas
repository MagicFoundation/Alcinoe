// Resize the src image to make that one side fit w or h keeping the other side equal or lower than w or h
function <LOADFROM>AndFitIntoTo<TONAME>(<SOURCE>; const W, H: single): <TOTYPE>;
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as rect
function <LOADFROM>AndFitIntoAndCropTo<TONAME>(<SOURCE>; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): <TOTYPE>;
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as round rect
function <LOADFROM>AndFitIntoAndCropToRoundRect<TONAME>(<SOURCE>; const W, H: single; const XRadius, YRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): <TOTYPE>;
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as circle
function <LOADFROM>AndFitIntoAndCropToCircle<TONAME>(<SOURCE>; const W, H: single; const XCropCenter: single = -50; const YCropCenter: single = -50): <TOTYPE>;
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as rect
function <LOADFROM>AndFitIntoAndCropAndBlurTo<TONAME>(<SOURCE>; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): <TOTYPE>;
// Resize the src image to make that one side fit w or h keeping the other side equal or bigger than w or h and then crop the src image as circle
function <LOADFROM>AndFitIntoAndCropAndBlurToCircle<TONAME>(<SOURCE>; const W, H: single; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): <TOTYPE>;
// https://i.stack.imgur.com/CcESX.png - transparent pixel in the mask are removed from the resulting image
function <LOADFROM>AndFitIntoAndCropAndMaskTo<TONAME>(<SOURCE>; const AMask: <MASKTYPE>; const XCropCenter: single = -50; const YCropCenter: single = -50): <TOTYPE>;
// https://i.stack.imgur.com/CcESX.png - transparent pixel in the mask are removed from the resulting image
function <LOADFROM>AndFitIntoAndCropAndMaskAndBlurTo<TONAME>(<SOURCE>; const AMask: <MASKTYPE>; const ABlurRadius: single; const XCropCenter: single = -50; const YCropCenter: single = -50): <TOTYPE>;
// If any dimension of the image is greater than W or H then the image is scaled down to best fit W and H
function <LOADFROM>AndPlaceIntoTo<TONAME>(<SOURCE>; const W, H: single): <TOTYPE>;
// If any dimension of the image is greater than W or H then the image is scaled down to best fit W and H
function <LOADFROM>AndPlaceIntoAndBlurTo<TONAME>(<SOURCE>; const W, H: single; const ABlurRadius: single): <TOTYPE>;
// Resize the src image to make that width = w and height = h
function <LOADFROM>AndStretchTo<TONAME>(<SOURCE>; const W, H: single): <TOTYPE>;
// Wrap the image inside w and h
function <LOADFROM>AndWrapTo<TONAME>(<SOURCE>; const AWrapMode: TALImageWrapMode; const W, H: single): <TOTYPE>;
// Normalize the orientation
function <LOADFROM>AndNormalizeOrientationTo<TONAME>(<SOURCE>; <EXIFORIENTATIONINFO>): <TOTYPE>;
// Do not resize anything
function <LOADFROM>To<TONAME>(<SOURCE>): <TOTYPE>;
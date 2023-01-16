//
//  VKSdkFramework.h
//
//  Copyright (c) 2015 VK.com
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy of
//  this software and associated documentation files (the "Software"), to deal in
//  the Software without restriction, including without limitation the rights to
//  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
//  the Software, and to permit persons to whom the Software is furnished to do so,
//  subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
//  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
//  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
//  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
//  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#import <UIKit/UIKit.h>

//! Project version number for VKSdkFramework.
FOUNDATION_EXPORT double VKSdkFrameworkVersionNumber;

//! Project version string for VKSdkFramework.
FOUNDATION_EXPORT const unsigned char VKSdkFrameworkVersionString[];

// In this header, you should import all the public headers of your framework using statements like #import <VKSdkFramework/PublicHeader.h>
#import <VKSdkFramework/VKSdk.h>
#import <VKSdkFramework/VKAccessToken.h>
#import <VKSdkFramework/VKPermissions.h>
#import <VKSdkFramework/VKUtil.h>
#import <VKSdkFramework/VKApi.h>
#import <VKSdkFramework/VKApiConst.h>
#import <VKSdkFramework/VKSdkVersion.h>
#import <VKSdkFramework/VKCaptchaViewController.h>
#import <VKSdkFramework/VKRequest.h>
#import <VKSdkFramework/VKBatchRequest.h>
#import <VKSdkFramework/NSError+VKError.h>
#import <VKSdkFramework/VKApiModels.h>
#import <VKSdkFramework/VKUploadImage.h>
#import <VKSdkFramework/VKShareDialogController.h>
#import <VKSdkFramework/VKActivity.h>
#import <VKSdkFramework/OrderedDictionary.h>
#import <VKSdkFramework/VKAuthorizeController.h>
#import <VKSdkFramework/VKBundle.h>
#import <VKSdkFramework/VKCaptchaView.h>
#import <VKSdkFramework/VKUploadMessagesPhotoRequest.h>
#import <VKSdkFramework/VKUploadPhotoBase.h>
#import <VKSdkFramework/VKUploadPhotoRequest.h>
#import <VKSdkFramework/VKUploadWallPhotoRequest.h>
#import <VKSdkFramework/VKHTTPClient.h>
#import <VKSdkFramework/VKHTTPOperation.h>
#import <VKSdkFramework/VKJSONOperation.h>
#import <VKSdkFramework/VKRequestsScheduler.h>
#import <VKSdkFramework/VKSharedTransitioningObject.h>
#import <VKSdkFramework/NSString+MD5.h>

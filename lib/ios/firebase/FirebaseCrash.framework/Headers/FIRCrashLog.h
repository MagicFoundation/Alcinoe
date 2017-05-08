#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/**
 * @abstract Logs a message to the Firebase Crash Reporter system.
 *
 * @discussion This method adds a message to the crash reporter
 *   logging system.  The recent logs will be sent with the crash
 *   report when the application exits abnormally.  Note that the
 *   timestamp of this message and the timestamp of the console
 *   message may differ by a few milliseconds.
 *
 * Messages should be brief as the total size of the message payloads
 *   is limited by the uploader and may change between releases of the
 *   crash reporter.  Excessively long messages will be truncated
 *   safely but that will introduce a delay in submitting the message.
 *
 * @warning Raises an NSInvalidArgumentException if @p format is nil.
 *
 * @param format A format string.
 *
 * @param ap A variable argument list.
 */
FOUNDATION_EXTERN NS_FORMAT_FUNCTION(1, 0)
void FIRCrashLogv(NSString *format, va_list ap);

/**
 * @abstract Logs a message to the Firebase Crash Reporter system.
 *
 * @discussion This method adds a message to the crash reporter
 *   logging system.  The recent logs will be sent with the crash
 *   report when the application exits abnormally.  Note that the
 *   timestamp of this message and the timestamp of the console
 *   message may differ by a few milliseconds.
 *
 * Messages should be brief as the total size of the message payloads
 *   is limited by the uploader and may change between releases of the
 *   crash reporter.  Excessively long messages will be truncated
 *   safely but that will introduce a delay in submitting the message.
 *
 * @warning Raises an NSInvalidArgumentException if @p format is nil.
 *
 * @param format A format string.
 *
 * @param ... A comma-separated list of arguments to substitute into
 *   format.
 *
 * @see FIRCrashLogv(format, ap)
 */
FOUNDATION_STATIC_INLINE NS_FORMAT_FUNCTION(1, 2)
void FIRCrashLog(NSString *format, ...) {
  va_list ap;

  va_start(ap, format);
  FIRCrashLogv(format, ap);
  va_end(ap);
}

/**
 * @abstract Logs a message to the Firebase Crash Reporter system as
 *   well as <code>NSLog()</code>.
 *
 * @discussion This method adds a message to the crash reporter
 *   logging system.  The recent logs will be sent with the crash
 *   report when the application exits abnormally.  Note that the
 *   timestamp of this message and the timestamp of the console
 *   message may differ by a few milliseconds.
 *
 * Messages should be brief as the total size of the message payloads
 *   is limited by the uploader and may change between releases of the
 *   crash reporter.  Excessively long messages will be truncated
 *   safely but that will introduce a delay in submitting the message.
 *
 * @warning Raises an NSInvalidArgumentException if @p format is nil.
 *
 * @param format A format string.
 *
 * @param ap A variable argument list.
 */
FOUNDATION_STATIC_INLINE NS_FORMAT_FUNCTION(1, 0)
void FIRCrashNSLogv(NSString *format, va_list ap) {
  va_list ap2;

  va_copy(ap2, ap);
  NSLogv(format, ap);
  FIRCrashLogv(format, ap2);
  va_end(ap2);
}

/**
 * @abstract Logs a message to the Firebase Crash Reporter system as
 *   well as <code>NSLog()</code>.
 *
 * @discussion This method adds a message to the crash reporter
 *   logging system.  The recent logs will be sent with the crash
 *   report when the application exits abnormally.  Note that the
 *   timestamp of this message and the timestamp of the console
 *   message may differ by a few milliseconds.
 *
 * Messages should be brief as the total size of the message payloads
 *   is limited by the uploader and may change between releases of the
 *   crash reporter.  Excessively long messages will be truncated
 *   safely but that will introduce a delay in submitting the message.
 *
 * @warning Raises an NSInvalidArgumentException if @p format is nil.
 *
 * @param format A format string.
 *
 * @param ... A comma-separated list of arguments to substitute into
 *   format.
 *
 * @see FIRCrashLogv(format, ap)
 */
FOUNDATION_STATIC_INLINE NS_FORMAT_FUNCTION(1, 2)
void FIRCrashNSLog(NSString *format, ...) {
  va_list ap;

  va_start(ap, format);
  FIRCrashNSLogv(format, ap);
  va_end(ap);
}

/**
 * @abstract Logs a message to the Firebase Crash Reporter system in
 *   a way that is easily called from Swift code.
 *
 * @discussion This method adds a message to the crash reporter
 *   logging system.  Similar to FIRCrashLog, but with a call signature
 *   that is more Swift friendly.  Unlike FIRCrashLog, callers
 *   use string interpolation instead of formatting arguments.
 *
 * @code
 * public func mySwiftFunction() {
 *   let unexpected_number = 10;
 *   FIRCrashMessage("This number doesn't seem right: \(unexpected_number)");
 * }
 * @endcode
 *
 * Messages should be brief as the total size of the message payloads
 *   is limited by the uploader and may change between releases of the
 *   crash reporter.  Excessively long messages will be truncated
 *   safely but that will introduce a delay in submitting the message.
 *
 * @param Message A log message
 *
 * @see FIRCrashLog(format, ...)
 */
FOUNDATION_STATIC_INLINE
void FIRCrashMessage(NSString *message) {
  FIRCrashLog(@"%@", message);
}

NS_ASSUME_NONNULL_END

#ifdef FIRCRASH_REPLACE_NSLOG
#if defined(DEBUG) || defined(FIRCRASH_LOG_TO_CONSOLE)
#define NSLog(...) FIRCrashNSLog(__VA_ARGS__)
#define NSLogv(...) FIRCrashNSLogv(__VA_ARGS__)
#else
#define NSLog(...) FIRCrashLog(__VA_ARGS__)
#define NSLogv(...) FIRCrashLogv(__VA_ARGS__)
#endif
#endif

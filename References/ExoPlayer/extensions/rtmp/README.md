# ExoPlayer RTMP extension #

The RTMP extension is a [DataSource][] implementation for playing [RTMP][]
streams using [LibRtmp Client for Android][].

[DataSource]: https://google.github.io/ExoPlayer/doc/reference/com/google/android/exoplayer2/upstream/DataSource.html
[RTMP]: https://en.wikipedia.org/wiki/Real-Time_Messaging_Protocol
[LibRtmp Client for Android]: https://github.com/ant-media/LibRtmp-Client-for-Android

## License note ##

Please note that whilst the code in this repository is licensed under
[Apache 2.0][], using this extension requires depending on LibRtmp Client for
Android, which is licensed separately.

[Apache 2.0]: https://github.com/google/ExoPlayer/blob/release-v2/LICENSE

## Getting the extension ##

The easiest way to use the extension is to add it as a gradle dependency:

```gradle
compile 'com.google.android.exoplayer:extension-rtmp:rX.X.X'
```

where `rX.X.X` is the version, which must match the version of the ExoPlayer
library being used.

Alternatively, you can clone the ExoPlayer repository and depend on the module
locally. Instructions for doing this can be found in ExoPlayer's
[top level README][].

[top level README]: https://github.com/google/ExoPlayer/blob/release-v2/README.md

## Using the extension ##

ExoPlayer requests data through `DataSource` instances. These instances are
either instantiated and injected from application code, or obtained from
instances of `DataSource.Factory` that are instantiated and injected from
application code.

`DefaultDataSource` will automatically use uses the RTMP extension whenever it's
available. Hence if your application is using `DefaultDataSource` or
`DefaultDataSourceFactory`, adding support for RTMP streams is as simple as
adding a dependency to the RTMP extension as described above. No changes to your
application code are required. Alternatively, if you know that your application
doesn't need to handle any other protocols, you can update any `DataSource`s and
`DataSource.Factory` instantiations in your application code to use
`RtmpDataSource` and `RtmpDataSourceFactory` directly.

## Links ##

* [Javadoc][]: Classes matching `com.google.android.exoplayer2.ext.rtmp.*`
  belong to this module.

[Javadoc]: https://google.github.io/ExoPlayer/doc/reference/index.html

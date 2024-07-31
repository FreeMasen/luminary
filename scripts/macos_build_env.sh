#! /bin/bash

echo "PKG_CONFIG_PATH="/opt/homebrew/opt/zstd/lib/pkgconfig:/opt/homebrew/opt/lz4/lib/pkgconfig:/opt/homebrew/opt/xz/lib/pkgconfig:$PKG_CONFIG_PATH"" >> $GITHUB_ENV
echo "PKG_CONFIG_LIBDIR="/usr/lib/pkgconfig:/opt/homebrew/Library/Homebrew/os/mac/pkgconfig/14:$PKG_CONFIG_LIBDIR"" >> $GITHUB_ENV
echo "CMAKE_INCLUDE_PATH="/Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk/System/Library/Frameworks/OpenGL.framework/Versions/Current/Headers:$CMAKE_INCLUDE_PATH"" >> $GITHUB_ENV
echo "CMAKE_LIBRARY_PATH="/Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk/System/Library/Frameworks/OpenGL.framework/Versions/Current/Libraries:$CMAKE_LIBRARY_PATH"" >> $GITHUB_ENV
echo "PATH="/Users/runner/.cargo/bin:/opt/homebrew/opt/pkg-config/bin:/opt/homebrew/Library/Homebrew/shims/mac/super:/opt/homebrew/opt/zstd/bin:/opt/homebrew/opt/lz4/bin:/opt/homebrew/opt/xz/bin:/opt/homebrew/opt/cmake/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"" >> $GITHUB_ENV

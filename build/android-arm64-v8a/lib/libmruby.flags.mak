MRUBY_CFLAGS = -MMD -MP -D__android__ -DANDROID --sysroot="E:\UserData\Programs\AndroidSDK\ndk-bundle/platforms/android-24/arch-arm64" -target aarch64-none-linux-android -gcc-toolchain "E:\UserData\Programs\AndroidSDK\ndk-bundle/toolchains/aarch64-linux-android-4.9/prebuilt/windows-x86_64" -Wno-invalid-command-line-argument -Wno-unused-command-line-argument -I"E:\UserData\Programs\AndroidSDK\ndk-bundle/sources/cxx-stl/llvm-libc++/include" -I"E:\UserData\Programs\AndroidSDK\ndk-bundle/sysroot/usr/include" -I"E:\UserData\Programs\AndroidSDK\ndk-bundle/sysroot/usr/include/aarch64-linux-android" -fpic -ffunction-sections -funwind-tables -fstack-protector-strong -no-canonical-prefixes -I"E:/UserData/Programming/Ruby/MRuby/fork/mruby/include"
MRUBY_LDFLAGS = --sysroot="E:\UserData\Programs\AndroidSDK\ndk-bundle/platforms/android-24/arch-arm64" -LE:/UserData/Programming/Ruby/MRuby/fork/mruby/build/android-arm64-v8a/lib
MRUBY_LDFLAGS_BEFORE_LIBS = -gcc-toolchain "E:\UserData\Programs\AndroidSDK\ndk-bundle/toolchains/aarch64-linux-android-4.9/prebuilt/windows-x86_64" -target aarch64-none-linux-android -no-canonical-prefixes
MRUBY_LIBS = -lmruby -lm
MRUBY_LIBMRUBY_PATH = E:/UserData/Programming/Ruby/MRuby/fork/mruby/build/android-arm64-v8a/lib/libmruby.a

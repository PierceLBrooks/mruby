MRUBY_CFLAGS = -MMD -MP -D__android__ -DANDROID --sysroot="E:\UserData\Programs\AndroidSDK\ndk-bundle/platforms/android-24/arch-arm" -target armv7-none-linux-androideabi -mfpu=neon -mfloat-abi=hard -gcc-toolchain "E:\UserData\Programs\AndroidSDK\ndk-bundle/toolchains/arm-linux-androideabi-4.9/prebuilt/windows-x86_64" -Wno-invalid-command-line-argument -Wno-unused-command-line-argument -I"E:\UserData\Programs\AndroidSDK\ndk-bundle/sources/cxx-stl/llvm-libc++/include" -I"E:\UserData\Programs\AndroidSDK\ndk-bundle/sysroot/usr/include" -I"E:\UserData\Programs\AndroidSDK\ndk-bundle/sysroot/usr/include/arm-linux-androideabi" -fpic -ffunction-sections -funwind-tables -fstack-protector-strong -no-canonical-prefixes -DDISABLE_GEMS -DDISABLE_GEMS -I"E:/UserData/Programming/Ruby/MRuby/mruby/include"
MRUBY_LDFLAGS = --sysroot="E:\UserData\Programs\AndroidSDK\ndk-bundle/platforms/android-24/arch-arm" -LE:/UserData/Programming/Ruby/MRuby/mruby/build/android-armeabi-v7a-neon-hard/lib
MRUBY_LDFLAGS_BEFORE_LIBS = -gcc-toolchain "E:\UserData\Programs\AndroidSDK\ndk-bundle/toolchains/arm-linux-androideabi-4.9/prebuilt/windows-x86_64" -target armv7-none-linux-androideabi -Wl,--fix-cortex-a8,--no-warn-mismatch -no-canonical-prefixes
MRUBY_LIBS = -lmruby -lm
MRUBY_LIBMRUBY_PATH = E:/UserData/Programming/Ruby/MRuby/mruby/build/android-armeabi-v7a-neon-hard/lib/libmruby.a

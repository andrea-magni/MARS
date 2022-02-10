OUTPUT="../libyaml_ios.a"
PLATFORM="iPhoneOS"

DEVELOPER_DIR=`xcode-select -print-path`
if [ ! -d $DEVELOPER_DIR ]; then
  echo "Please set up Xcode correctly. '$DEVELOPER_DIR' is not a valid developer tools folder."
  exit 1
fi

SDK_ROOT=$DEVELOPER_DIR/Platforms/$PLATFORM.platform/Developer/SDKs/$PLATFORM.sdk
if [ ! -d $SDK_ROOT ]; then
  echo "The iOS SDK was not found in $SDK_ROOT."
  exit 1
fi

C_SOURCE="src/api.c src/dumper.c src/emitter.c src/loader.c src/parser.c src/reader.c src/scanner.c src/writer.c"
DEFINES="-DYAML_DECARE_STATIC -DHAVE_CONFIG_H"

rm armv7.a
rm arm64.a
rm *.o

clang -c -O3 -arch armv7 -isysroot $SDK_ROOT -I src $DEFINES $C_SOURCE
ar rcs armv7.a *.o
ranlib armv7.a
rm *.o

clang -c -O3 -arch arm64 -isysroot $SDK_ROOT -I src $DEFINES $C_SOURCE
ar rcs arm64.a *.o
ranlib arm64.a
rm *.o

rm $OUTPUT
lipo -create -arch armv7 armv7.a -arch arm64 arm64.a -output $OUTPUT

rm armv7.a
rm arm64.a

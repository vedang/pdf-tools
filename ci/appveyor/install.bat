@echo off

cd %APPVEYOR_BUILD_FOLDER%

echo Compiler: %COMPILER%
echo Architecture: %MSYS2_ARCH%
echo Platform: %PLATFORM%
echo MSYS2 directory: %MSYS2_DIR%
echo MSYS2 system: %MSYSTEM%
echo Bits: %BIT%

IF %COMPILER%==msys2 (
  @echo on
  SET "PATH=C:\%MSYS2_DIR%\%MSYSTEM%\bin;C:\%MSYS2_DIR%\usr\bin;C:\%MSYS2_DIR%\home\appveyor\.cask\bin;%PATH%"

  REM dependencies
  bash -lc "pacman -S --needed --noconfirm git base-devel automake autoconf mingw-w64-x86_64-zlib mingw-w64-x86_64-libpng mingw-w64-x86_64-poppler mingw-w64-x86_64-imagemagick openssl mingw-w64-x86_64-openssl mingw-w64-x86_64-python2 mingw-w64-x86_64-emacs"

  REM Set up Cask
  bash -lc "git clone https://github.com/cask/cask ~/.cask"
)

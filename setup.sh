#!/bin/sh

set -e # exit on error

print_usage_info()
{
    echo "Matcha Setup Script"
    echo ""
    echo "USAGE:"
    echo "./setup.sh [--help | [--prefix=PREFIX]"
    echo ""
    echo " --help             Display this help text"
    echo " --prefix=PREFIX    Install prerequisites in 'PREFIX/bin' (default: '\$HOME/.local/bin')"
    echo ""
    echo "For a non-interactive build with the 'yes' utility installed, execute"
    echo "yes | ./setup.sh"
}

while [ "$1" != "" ]; do
    PARAM=$(echo "$1" | awk -F= '{print $1}')
    VALUE=$(echo "$1" | awk -F= '{print $2}')
    case $PARAM in
        -h | --help)
            print_usage_info
            exit
            ;;
        --prereqs)
            list_prerequisites
            exit
            ;;
        --prefix)
            PREFIX=$VALUE
            ;;
        *)
            echo "ERROR: unknown parameter \"$PARAM\""
            usage
            exit 1
            ;;
    esac
    shift
done

set -u # error on use of undefined variable

DEPENDENCIES_DIR="build/dependencies"
mkdir -p $DEPENDENCIES_DIR
if [ ! -d $DEPENDENCIES_DIR/caffeine ] ; then
  git clone https://github.com/berkeleylab/caffeine build/dependencies/caffeine
fi

cd "$DEPENDENCIES_DIR/caffeine"
  if [ -z ${PREFIX+x} ]; then
    ./install.sh
  else
    ./install.sh --prefix=$PREFIX
  fi
cd -

if [ -z ${PREFIX+x} ]; then
  PREFIX="$HOME/.local"
else
  PREFIX=`$REALPATH ${PREFIX}`
fi

PKG_CONFIG=`which pkg-config`
if [ -z ${PKG_CONFIG_PATH+x} ]; then
  export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig"
else
  export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"
fi
echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"

FPM=`which fpm`

RUN_FPM_SH="build/run-fpm.sh"
echo "#!/bin/sh"                                                              >  $RUN_FPM_SH
echo "#-- DO NOT EDIT -- created by matcha/install.sh"                        >> $RUN_FPM_SH
echo "\"${FPM}\" \"\$@\" \\"                                                  >> $RUN_FPM_SH
echo "--c-compiler \"`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_CC`\" \\"  >> $RUN_FPM_SH
echo "--c-flag \"`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_CFLAGS`\" \\"  >> $RUN_FPM_SH
echo "--flag \"-O3 -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds -fcheck=array-temps -fbacktrace\" \\" >> $RUN_FPM_SH
echo "--link-flag \"`$PKG_CONFIG caffeine --variable=CAFFEINE_FPM_LDFLAGS`\"" >> $RUN_FPM_SH
chmod u+x $RUN_FPM_SH

cp templates/fpm.toml-template fpm.toml

echo ""
echo "________________ Matcha has been poured! ________________"
echo ""
echo "To rebuild or to run tests or examples via the Fortran Package"
echo "Manager (fpm) with the required compiler/linker flags, pass a"
echo "fpm command to the build/run-fpm.sh script. For example, run"
echo "the Matcha test suite as follows:"
echo ""
echo "./$RUN_FPM_SH test"

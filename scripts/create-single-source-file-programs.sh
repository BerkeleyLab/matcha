#!/bin/bash
# Copyright (c), The Regents of the University of California
# Terms of use are as specified in LICENSE.txt
# ----
# This script concatenates the matcha software stack into single-source-file programs.

export assert_dir="../build/dependencies/assert"
export julienne_dir="../build/dependencies/julienne"
export matcha_dir=".."
export test_dir="../test"
export destination_dir="../build/single-file-programs"

echo ""
echo "---------------------- create-single-source-file.sh ----------------------------"
if [ ! -d "$assert_dir" ] || [ ! -d $julienne_dir ]; then
  echo ""
  echo ""
  echo "Dependencies assert and/or julienne were not found in ../build/dependencies."
  echo "Running fpm build to download the dependencies."
  echo "This unavoidably builds matcha too."
  echo ""
  echo ""
  fpm build
fi

if [ -d $destination_dir ]; then
  echo "Destination directory exists: $destination_dir"
else
  echo "Creating destination directory: $destination_dir"
  mkdir "$destination_dir"
fi

if [ -d ../build/dependencies/assert ] && [ -d ../build/dependencies/julienne ]; then
  echo "Dependencies Assert and Julienne are in ../build/dependencies"
else
  echo "Dependencies Assert and Julienne are not in ../build/dependencies. Something went wrong."
  exit 1
fi

echo "Concatenating Assert ($assert_dir)."
assert_modules=$(find "$assert_dir/src" -name '*_m.?90')
cat $assert_modules > "$destination_dir"/assert.F90

echo "Concatenating Julienne."
julienne_modules=$(find "$julienne_dir/src" -name '*_m.?90')
cat \
  "$julienne_dir/src/julienne/julienne_string_m.f90" \
  "$julienne_dir/src/julienne/julienne_file_m.f90" \
  "$julienne_dir/src/julienne/julienne_bin_m.F90" \
  "$julienne_dir/src/julienne/julienne_test_diagnosis_m.F90" \
  "$julienne_dir/src/julienne/julienne_test_result_m.f90" \
  "$julienne_dir/src/julienne/julienne_user_defined_collectives_m.f90" \
  "$julienne_dir/src/julienne/julienne_command_line_m.f90" \
  "$julienne_dir/src/julienne/julienne_test_description_m.f90" \
  "$julienne_dir/src/julienne/julienne_vector_test_description_m.F90" \
  "$julienne_dir/src/julienne/julienne_test_m.F90" \
  "$julienne_dir/src/julienne/julienne_formats_m.F90" \
  "$julienne_dir/src/julienne/julienne_github_ci_m.f90" \
  "$julienne_dir/src/julienne_m.f90" \
  > "$destination_dir/julienne.F90"
julienne_submodules=$(find "$julienne_dir/src" -name '*_s.?90')
cat $julienne_submodules >> "$destination_dir"/julienne.F90

echo "Concatenating Matcha library."
cat \
  "$matcha_dir/src/matcha/distribution_m.f90" \
  "$matcha_dir/src/matcha/t_cell_collection_m.f90" \
  "$matcha_dir/src/matcha/input_m.f90" \
  "$matcha_dir/src/matcha/julienne_m.f90" \
  "$matcha_dir/src/matcha/subdomain_m.f90" \
  "$matcha_dir/src/matcha/output_m.f90" \
  "$matcha_dir/src/matcha_m.f90" \
  > "$destination_dir/matcha.F90"
matcha_submodules=$(find "$matcha_dir/src" -name '*_s.?90')
cat $matcha_submodules >> "$destination_dir"/matcha.F90

echo "Copying include files."
cp "$assert_dir/include/assert_macros.h" "$assert_dir/include/assert_features.h" "$destination_dir"
cat "$julienne_dir/include/language-support.F90" "$matcha_dir/include/language-support.F90" > "$destination_dir"/language-support.F90
cp "$julienne_dir/include/julienne-assert-macros.h" "$destination_dir"

echo "Concatenating Assert, Julienne, and Matcha src and app into $destination_dir/diffusion-app.F90"
cat "$destination_dir"/assert.F90 "$destination_dir"/julienne.F90 "$destination_dir"/matcha.F90 "$matcha_dir/app/diffusion.f90" > $destination_dir/diffusion-app.F90

echo "Concatenating Assert, Julienne, and Matcha src and test into $destination_dir/test-suite.F90"
test_modules=$(find "$test_dir" -name '*_m.?90')
cat "$destination_dir"/assert.F90 "$destination_dir"/julienne.F90 "$destination_dir"/matcha.F90 $test_modules "$test_dir"/main.F90 > $destination_dir/test-suite.F90

#!/bin/bash

read -p "Enter a name: " name

if [ -z "$name" ]; then
    echo "Error: Name cannot be empty"
    exit 1
fi

mkdir -p ./test

dir_path="./test/$name"
mkdir -p "$dir_path"

touch "$dir_path/$name.mvs"
touch "$dir_path/$name.out"
cp ./utils/config.toml "$dir_path/config.toml"

echo "Successfully created:"
echo "  Directory: $dir_path"
echo "  Files: $name.mvs and $name.out"
helix "$dir_path/$name.mvs" "$dir_path/$name.out" --vsplit


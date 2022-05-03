#!/bin/sh

echo "Copying from $1 to $2"
mkdir -p $2
cd $1

for i in *.clj
do
    echo $i
    cat $i | sed -e "s/$1/$2/g" > ../$2/$i
done    

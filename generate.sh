#! /bin/bash

# These will run out of memory when testing :'(
sbt "testOnly *Vgg7Suite"
bt "testOnly *AWSVggWrapperSuite"

# copy the files that hopefully generated
cp test_run_dir/*/Vgg7.v .
cp test_run_dir/*/AWSVggWrapper.v .

if [ ! -f Vgg7.v ] || [ ! -f AWSVggWrapper.v ]
then
  echo "Verilog generation failed ..."
  echo "This design requires an absurb amount of memory for chisel to generate"
  echo "Something on the order of 250GB"
  exit
fi

# YAY! files exist ... replace Queues ...
sed -i "s/Queue /Queue_1024 /g" Vgg7.v
sed -i "s/Queue_1 /Queue_2048 /g" Vgg7.v
sed -i "s/Queue_3 /Queue_4096 /g" Vgg7.v
sed -i "s/Queue /Queue_4096 /g" AWSVggWrapper.v
sed -i "s/module Queue/module QueueOutput/g" AWSVggWrapper.v
sed -i "s/SSILayer/SSILayerOut/g" AWSVggWrapper.v
sed -i "s/Fanout/FanoutAWS/g" AWSVggWrapper.v

if grep -q "module Vgg" AWSVggWrapper.v
then
  mv AWSVggWrapper.v AWSVggWrapper.v.bak
  head -n13 AWSVggWrapper.v.bak > AWSVggWrapper.v
  tail -n+3889 AWSVggWrapper.v.bak >> AWSVggWrapper.v
fi

# need to fix the SimpleSlidingWindow_3 in Vgg7.v
# generate new and replace in Vgg7.v then
head -n17168 Vgg7.v > Vgg7_tmp.v
cat SimpleSlidingWindow_3.v >> Vgg7_tmp.v
tail -n+19787 Vgg7.v >> Vgg7_tmp.v
mv Vgg7_tmp.v Vgg7.v
head -n50000 Vgg7.v > Vgg7_top.v
tail -n+50001 Vgg7.v > Vgg7_bot.v
sed -i "s#SimpleSlidingWindow_3 SimpleSlidingWindow\(.*\)#SimpleSlidingWindow_3 SimpleSlidingWindow\1\n    .io_dataOut_ready( 1'h1 ),#g" Vgg7_top.v
sed -i "s#SimpleSlidingWindow_3 SimpleSlidingWindow\(.*\)#SimpleSlidingWindow_3 SimpleSlidingWindow\1\n    .io_dataOut_ready( io_dataOut_ready ),#g" Vgg7_bot.v
cat Vgg7_bot.v >> Vgg7_top.v
mv Vgg7_top.v Vgg7.v

echo "Queue replacements done ... making tar ball all_code.tgz"
mkdir tmp
cp AWSVggWrapper.v Vgg7.v tmp/
cp src/main/verilog/* tmp/
cd tmp
tar zcvf all_code.tgz *.v *.xdc
cd ..
mv tmp/all_code.tgz .
rm -rf tmp

echo "done"

# Implementation in Chisel of Ternary Weight Networks

This paper creates an FPGA implementation of https://arxiv.org/abs/1605.04711
It implements a network with half the number of convolutional filters as in this paper achieving 91.9% on CIFAR10

# Network architecture
conv1 => 64x3x3x3
conv2 => 64x3x3x64
mp1 => 2x2 kernel with stride 2
conv3 => 128x3x3x64
conv4 => 128x3x3x128
mp2 => 2x2 kernel with stride 2
conv5 => 256x3x3x128
conv6 => 256x3x3x256
mp3 => 2x2 kernel with stride 2
dense => 4096x1024
dense => 1024x10

# Running
The complete verilog, including constraints is included in all_code.tgz
To generate, run the script generate.sh, note however it takes a long time (8hrs+) and alot of memory (250GB)

# FPGA version
There is no I/O included in this script but it targeted the AWS-F1 instance FPGA with part number xcvu9p-flgb2104-2-i


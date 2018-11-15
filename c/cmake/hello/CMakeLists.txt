project(main)
cmake_minimum_required(VERSION 3.5.1)
aux_source_directory(. DIR_SRCS)
add_executable(a.out ${DIR_SRCS})

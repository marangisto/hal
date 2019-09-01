# HAL
Device support and HAL for MCUs

A complete bottom-up from bare-metal HAL in C++. The low-level device layer is generated from vendor supplied SVD. Each peripheral is declared as a structure with register members including all defined field constants. The peripheral itself is then defined as a single global instance of this structure. In the HAL layer perihperals are treated as static resources and are accessed as such, i.e. via static methods. Generalization across multiple peripherals is done by templating over integers enumerating instances.

The package relies on the https://github.com/marangisto/karakul build tool. There are examples for various boards in the examples directory. To build and run an example, e.g. for the NUCLEO-G431KB board, simply run:

```sh
$ cd examples/NUCLEO-G431KB/Blink/
$ mk
```
which produces
```
# arm-none-eabi-g++ (for _build/hal/system.cpp.o)
# arm-none-eabi-g++ (for _build/hal/startup.cpp.o)
# arm-none-eabi-g++ (for _build/hal/memory.cpp.o)
# arm-none-eabi-ar (for _build/hal/hal.a)
# arm-none-eabi-g++ (for _build/Blink/Main.cpp.o)
# arm-none-eabi-gcc (for _build/image.elf)
# arm-none-eabi-size (for _build/image.elf)
   text    data     bss     dec     hex filename
    376     472       8     856     358 _build/image.elf
# arm-none-eabi-objdump (for _build/image.s)
Build completed in 0:04m
```
followed by, or collapsed into a single command after an edit:
```sh
$ mk upload
```
which produces
```
# arm-none-eabi-objcopy (for _build/image.bin)
# STM32_Programmer_CLI (for upload)
      -------------------------------------------------------------------
                       STM32CubeProgrammer v2.1.0
      -------------------------------------------------------------------

ST-LINK SN  : 004B002D3137510539383538
ST-LINK FW  : V3J2M1
Voltage     : 3.28V
SWD freq    : 24000 KHz
Connect mode: Under Reset
Reset mode  : Hardware reset
Device ID   : 0x468
Device name : STM32G43x/G44x
Flash size  : 128 KBytes
Device type : MCU
Device CPU  : Cortex-M4


Memory Programming ...
Opening and parsing file: image.bin
  File          : image.bin
  Size          : 848 Bytes
  Address       : 0x08000000

Erasing memory corresponding to segment 0:
Erasing internal memory sector 0
Download in Progress:
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100%

File download complete
Time elapsed during download operation: 00:00:00.248

Verifying ...

Read progress:
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100%

Download verified successfully

Hard reset is performed
Build completed in 0:02m
```

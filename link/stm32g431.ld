/* STM32G431 128k FLASH, 32k RAM */

ENTRY(__reset)

MEMORY
{
    FLASH (rx) : ORIGIN = 0x08000000, LENGTH = 128K
    RAM (rwx) : ORIGIN = 0x20000000, LENGTH = 32K
}

__estack = ORIGIN(RAM) + LENGTH(RAM);

SECTIONS
{
    .vectors :
    {
        . = ALIGN(4);
        KEEP(*(.vectors))
        . = ALIGN(4);
    } > FLASH

    .text :
    {
        . = ALIGN(4);
        *(.text)
        *(.text*)
        *(.glue_7)
        *(.glue_7t)
        . = ALIGN(8);
    } > FLASH

    .rodata :
    {
        . = ALIGN(4);
        *(.rodata)
        *(.rodata*)
        . = ALIGN(8);
    } > FLASH

    __sidata = LOADADDR(.data);

    .data :
    {
        . = ALIGN(4);
        __sdata = .;
        *(.data)
        *(.data*)
        . = ALIGN(8);
        __edata = .;
    } > RAM AT > FLASH

    . = ALIGN(4);
    .bss :
    {
        __sbss = .;
        *(.bss)
        *(.bss*)
        *(COMMON)
        . = ALIGN(8);
        __ebss = .;
        end = .;
    } > RAM
}


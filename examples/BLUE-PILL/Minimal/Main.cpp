#include <stdint.h>

extern uint32_t __estack;

struct rcc_t
{
    volatile uint32_t    CR;                   // Clock control register
    volatile uint32_t    CFGR;                 // Clock configuration register (RCC_CFGR)
    volatile uint32_t    CIR;                  // Clock interrupt register (RCC_CIR)
    volatile uint32_t    APB2RSTR;             // [Read-write] APB2 peripheral reset register (RCC_APB2RSTR)
    volatile uint32_t    APB1RSTR;             // [Read-write] APB1 peripheral reset register (RCC_APB1RSTR)
    volatile uint32_t    AHBENR;               // [Read-write] AHB Peripheral Clock enable register (RCC_AHBENR)
    volatile uint32_t    APB2ENR;              // [Read-write] APB2 peripheral clock enable register (RCC_APB2ENR)
    volatile uint32_t    APB1ENR;              // [Read-write] APB1 peripheral clock enable register (RCC_APB1ENR)
    volatile uint32_t    BDCR;                 // Backup domain control register (RCC_BDCR)
    volatile uint32_t    CSR;                  // Control/status register (RCC_CSR)
};

static rcc_t& RCC = *reinterpret_cast<rcc_t*>(0x40021000);

struct gpioc_t
{
    volatile uint32_t    CRL;                  // [Read-write] Port configuration register low (GPIOn_CRL)
    volatile uint32_t    CRH;                  // [Read-write] Port configuration register high (GPIOn_CRL)
    volatile uint32_t    IDR;                  // [Read-only] Port input data register (GPIOn_IDR)
    volatile uint32_t    ODR;                  // [Read-write] Port output data register (GPIOn_ODR)
    volatile uint32_t    BSRR;                 // [Write-only] Port bit set/reset register (GPIOn_BSRR)
    volatile uint32_t    BRR;                  // [Write-only] Port bit reset register (GPIOn_BRR)
    volatile uint32_t    LCKR;                 // [Read-write] Port configuration lock register
};

static gpioc_t& GPIOC = *reinterpret_cast<gpioc_t*>(0x40011000);

extern "C" void __reset()
{
    RCC.APB2ENR |= 0x10;                            // I/O port C clock enable
    GPIOC.CRH |= (0x3 << ((13 - 8) << 2));          // configure PC13 as output

    for (;;)
    {
        GPIOC.ODR ^= (1 << 13);                     // toggle output    
        for (volatile int j = 0; j < 100000; ++j);  // busy wait delay
    }
}

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack                     // -16: Initial stack pointer
    , __reset                                       // -15: Reset [fixed]
    };


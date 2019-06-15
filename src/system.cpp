#include <hal.h>

namespace hal
{

using namespace device;

namespace internal
{

template<uint32_t x, uint32_t b, uint8_t nbits>
static constexpr uint32_t encode()
{
    static_assert(x < (1 << nbits), "bit field overflow");
    return ((x & (1 << 0)) ? (b << 0) : 0)
         | ((x & (1 << 1)) ? (b << 1) : 0)
         | ((x & (1 << 2)) ? (b << 2) : 0)
         | ((x & (1 << 3)) ? (b << 3) : 0)
         | ((x & (1 << 4)) ? (b << 4) : 0)
         | ((x & (1 << 5)) ? (b << 5) : 0)
         | ((x & (1 << 6)) ? (b << 6) : 0)
         | ((x & (1 << 7)) ? (b << 7) : 0)
         | ((x & (1 << 8)) ? (b << 8) : 0)
         ;
}

} // namespace internal

void sys_tick::delay_ms(uint32_t ms)
{
    uint32_t now = ms_counter, then = now + ms;

    while (ms_counter >= now && ms_counter < then)
        ;   // busy wait
}

void sys_tick::init(uint32_t n)
{
    using namespace hal;
    typedef stk_t _;

    ms_counter = 0;                             // start new epoq

#if defined(STM32F0)
    STK.CSR = _::CSR_RESET_VALUE;               // reset controls
    STK.RVR = n - 1;                            // reload value
    STK.CVR = _::CVR_RESET_VALUE;               // current counter value
    STK.CSR |= _::CSR_CLKSOURCE;                // systick clock source
    STK.CSR |= _::CSR_ENABLE | _::CSR_TICKINT;  // enable counter & interrupts
#elif defined(STM32F4)
    STK.CTRL = _::CTRL_RESET_VALUE;                 // reset controls
    STK.LOAD = n - 1;                               // reload value
    STK.VAL = _::VAL_RESET_VALUE;                   // current counter value
    STK.CTRL |= _::CTRL_CLKSOURCE;                  // systick clock source
    STK.CTRL |= _::CTRL_ENABLE | _::CTRL_TICKINT;   // enable counter & interrupts
#else
    static_assert(false, "no featured sys-tick initialzation");
#endif
}

volatile uint32_t sys_tick::ms_counter = 0;

inline void sys_tick_init(uint32_t n) { sys_tick::init(n); }
inline void sys_tick_update() { ++sys_tick::ms_counter; } // N.B. wraps in 49 days!
}

extern "C" void SysTick_HDLR()
{
    hal::sys_tick_update();
}

extern "C" void system_init(void)
{
    using namespace hal;
    typedef rcc_t _;

#if defined(STM32F0)
    // reset clock control registers

    RCC.CR = _::CR_RESET_VALUE;
    RCC.CFGR = _::CFGR_RESET_VALUE;
    RCC.CFGR2 = _::CFGR2_RESET_VALUE;
    RCC.CFGR3 = _::CFGR3_RESET_VALUE;
    RCC.CR2 = _::CR2_RESET_VALUE;
    RCC.CIR = _::CIR_RESET_VALUE;

    // set system clock to HSI-PLL 48MHz

    Flash.ACR = flash_t::ACR_PRFTBE | flash_t::ACR_LATENCY<0x1>;

    RCC.CFGR |= _::CFGR_PLLMUL<0xa>;        // PLL multiplier 12
    RCC.CR |= _::CR_PLLON;                  // enable PLL
    while (!(RCC.CR & _::CR_PLLRDY));       // wait for PLL to be ready
    RCC.CFGR |= _::CFGR_SW<0x2>;            // select PLL as system clock

    // wait for PLL as system clock

    while ((RCC.CFGR & _::CFGR_SWS<0x3>) != _::CFGR_SWS<0x2>);

    // initialize sys-tick for milli-second counts

    hal::sys_tick_init(60000);
#elif defined(STM32F4)
    // reset clock control registers

    RCC.CR = _::CR_RESET_VALUE;
    RCC.CFGR = _::CFGR_RESET_VALUE;
    RCC.CIR = _::CIR_RESET_VALUE;

    // set system clock to HSI-PLL 100MHz

    constexpr uint8_t wait_states = 0x3;    // 3 wait-states for 100MHz at 2.7-3.3V

    FLASH.ACR = flash_t::ACR_PRFTEN | flash_t::ACR_LATENCY<wait_states>;
    while (((FLASH.ACR & flash_t::ACR_LATENCY<0x7>) & 0xf) != flash_t::ACR_LATENCY<wait_states>); // wait to take effect

    enum pllP_t { pllP_2 = 0x0, pllP_4 = 0x1, pllP_6 = 0x2, pllP_8 = 0x3 };

    // fVCO = hs[ei] * pllN / pllM                      // must be 100MHz - 400MHz
    // fSYS = fVCO / pllP                               // <= 100MHz
    // fUSB = fVCO / pllQ                               // <= 48MHz

    // pllN = 200, pllM = 8, pllP = 4, pllQ = 9, fSYS = 1.0e8, fUSB = 4.4445e7

    constexpr uint16_t pllN = 200;                      // 9 bits, valid range [50..432]
    constexpr uint8_t pllM = 8;                         // 6 bits, valid range [2..63]
    constexpr pllP_t pllP = pllP_4;                     // 2 bits, enum range [2, 4, 6, 8]
    constexpr uint8_t pllQ = 9;                         // 4 bits, valid range [2..15]
    constexpr uint8_t pllSRC = 0;                       // 0 = HSI, 1 = HSE 

    using internal::encode;

    RCC.PLLCFGR = encode<pllSRC, _::PLLCFGR_PLLSRC, 1>()
                | encode<pllN, _::PLLCFGR_PLLN0, 9>()
                | encode<pllM, _::PLLCFGR_PLLM0, 6>()
                | encode<pllP, _::PLLCFGR_PLLP0, 2>()
                | encode<pllQ, _::PLLCFGR_PLLQ0, 4>()
                ;

    RCC.CR |= _::CR_PLLON;                              // enable PLL
    while (!(RCC.CR & _::CR_PLLRDY));                   // wait for PLL to be ready

    RCC.CFGR |= encode<0x2, _::CFGR_SW0, 2>();          // select PLL as system clock

    // wait for PLL as system clock

    while ((RCC.CFGR & encode<0x3, _::CFGR_SWS0, 2>()) != encode<0x2, _::CFGR_SWS0, 2>());

    // initialize sys-tick for milli-second counts

    hal::sys_tick_init(100000);
#else
    static_assert(false, "no featured clock initialzation");
#endif
}


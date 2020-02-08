#pragma once

#include <gpio.h>

namespace hal
{
namespace i2c
{

enum i2c_interrupt_t
    { i2c_error                 = 0x01
    , i2c_transfer_complete     = 0x02
    , i2c_stop_detection        = 0x04
    , i2c_nack_received         = 0x08
    , i2c_address_match         = 0x10
    , i2c_receive               = 0x20
    , i2c_transmit              = 0x40
    };

template<int NO> struct i2c_traits {};

template<> struct i2c_traits<1>
{
    typedef device::i2c1_t T;
    static inline T& I2C() { return device::I2C1; }
    static const gpio::internal::alternate_function_t scl = gpio::internal::I2C1_SCL;
    static const gpio::internal::alternate_function_t sda = gpio::internal::I2C1_SDA;
};

template<> struct i2c_traits<2>
{
    typedef device::i2c2_t T;
    static inline T& I2C() { return device::I2C2; }
    static const gpio::internal::alternate_function_t scl = gpio::internal::I2C2_SCL;
    static const gpio::internal::alternate_function_t sda = gpio::internal::I2C2_SDA;
};

template<int NO, gpio::gpio_pin_t SCL, gpio::gpio_pin_t SDA>
struct i2c_t
{
    typedef typename i2c_traits<NO>::T _;

    template
        < uint32_t              transfer_speed  = 100000
        , gpio::output_speed_t  pin_speed       = gpio::high_speed
        >
    static void setup()
    {
        using namespace gpio::internal;

        alternate_t<SCL, i2c_traits<NO>::scl>::template setup<pin_speed, open_drain>();
        alternate_t<SDA, i2c_traits<NO>::sda>::template setup<pin_speed, open_drain>();

        device::peripheral_traits<_>::enable();     // enable peripheral clock

        I2C().TIMINGR = static_cast<uint32_t>(0x00F02B86);  // FIXME: compute speed!
        I2C().CR1 = _::CR1_RESET_VALUE              // reset control register 1
                  | _::CR1_PE                       // enable i2c peripheral
                  ;
    }

    // FIXME: template type to use 10-bit
    static void own_address(uint8_t addr)
    {
        I2C().OAR1 = _::OAR1_RESET_VALUE            // reset own address register
                   | _::OAR1_OA1EN                  // enable own address (ACK)
                   | addr
                   ;
    }

    static void enable_interrupt(uint32_t x)
    {
        I2C().CR1 |= ((x & i2c_error) ? _::CR1_ERRIE : 0)
                  |  ((x & i2c_transfer_complete) ? _::CR1_TCIE : 0)
                  |  ((x & i2c_stop_detection) ? _::CR1_STOPIE : 0)
                  |  ((x & i2c_nack_received) ? _::CR1_NACKIE : 0)
                  |  ((x & i2c_address_match) ? _::CR1_ADDRIE : 0)
                  |  ((x & i2c_receive) ? _::CR1_RXIE : 0)
                  |  ((x & i2c_transmit) ? _::CR1_TXIE : 0)
                  ;
    }

    // FIXME: template type to use 10-bit
    static void write(uint8_t addr, const uint8_t *buf, uint8_t nbytes)
    {
        I2C().CR2 = _::CR2_RESET_VALUE              // reset control register 2
                  | nbytes << 16                    // transmit message size FIXME: shift hack!
                  | _::CR2_AUTOEND                  // stop condition after n bytes
                  | (false ? _::CR2_ADD10 : 0)      // enable for 10-bit addressing
                  | (false ? _::CR2_RD_WRN : 0)     // master read (true) or write (false)
                  | _::CR2_START                    // generate start condition when free
                  | addr                            // slave address FIXME: check range!
                  ;

        while (!(I2C().ISR & _::ISR_STOPF))         // while we don't see stop condition
        {
                                                    // FIXME: insert time-out handling code here
            if (I2C().ISR & _::ISR_TXIS)            // transmit buffer is empty (why not TXE?)
                I2C().TXDR = *buf++;                // send next byte
        }

        I2C().ICR &= ~_::ICR_STOPCF;                // clear stop condition flag
    }

private:
    static inline typename i2c_traits<NO>::T& I2C() { return i2c_traits<NO>::I2C(); }
};

} // namespace i2c
} // namespace hal


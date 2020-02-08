#pragma once

#include <gpio.h>

namespace hal
{
namespace i2c
{
namespace internal
{

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

        I2C().ICR |= _::ICR_STOPCF;                 // clear stop condition flag
    }

private:
    static inline typename i2c_traits<NO>::T& I2C() { return i2c_traits<NO>::I2C(); }
};

} // namespace internal

template<int NO, gpio::gpio_pin_t SCL, gpio::gpio_pin_t SDA>
struct i2c_master_t
{
    template<uint32_t SPEED = 100000>
    static void setup()
    {
        internal::i2c_t<NO, SCL, SDA>::template setup<SPEED>();
    }

    static void write(uint8_t addr, const uint8_t *buf, uint8_t nbytes)
    {
        internal::i2c_t<NO, SCL, SDA>::write(addr, buf, nbytes);
    }
};

template<int NO, gpio::gpio_pin_t SCL, gpio::gpio_pin_t SDA>
class i2c_slave_t
{
public:
    typedef void (*callback_t)(uint8_t *buf, uint8_t nbytes);

    // FIXME: template type to use 10-bit
    template<uint32_t SPEED = 100000>
    static void setup(uint8_t addr, callback_t cb, uint8_t *buf, uint8_t bufsize)
    {
        m_callback = cb;
        m_buf = m_ptr = buf;
        m_bufsize = bufsize;

        internal::i2c_t<NO, SCL, SDA>::template setup<SPEED>();

        I2C().OAR1 = _::OAR1_RESET_VALUE            // reset own address register
                   | _::OAR1_OA1EN                  // enable own address (ACK)
                   | addr
                   ;
        I2C().CR1 |= _::CR1_ERRIE                   // enable receive interrupt
                  |  _::CR1_NACKIE                  // enable nack received interrupt
                  |  _::CR1_ADDRIE                  // enable address match interrupt
                  |  _::CR1_STOPIE                  // enable stop condition interrupt
                  ;
    }

    static void isr()
    {
        uint32_t sts = I2C().ISR;

        if (sts & _::ISR_ADDR)                      // address matched
        {
            I2C().ICR |= _::ICR_ADDRCF;             // clear the address matched flag

            if (sts & _::ISR_DIR)                   // direction (true = read / slave transmit)
            {
                                                    // FIXME: do the right thing!
            }
            else
            {
                m_ptr = m_buf;                      // reset buffer pointer
                I2C().CR1 |= _::CR1_RXIE;           // enable receive interrupt
            }
        }
        else if (sts & _::ISR_RXNE)
        {
            if (m_ptr < m_buf + m_bufsize)          // check for buffer overrun
                *m_ptr++ = I2C().RXDR;              // read byte into buffer slot
            else
                ;                                   // FIXME: signal error somehow
        }
        else if (sts & _::ISR_STOPF)                // stop condition detected
        {
            I2C().ICR |= _::ICR_STOPCF;             // clear the stop condition flag
            I2C().CR1 &= ~_::CR1_RXIE;              // disable receive interrupt
            m_callback(m_buf, m_ptr - m_buf);       // invoke slave callback
        }
        else
            ;   // FIXME: handle error condition
    }

private:
    typedef typename internal::i2c_traits<NO>::T _;
    static inline typename internal::i2c_traits<NO>::T& I2C()
    {
        return internal::i2c_traits<NO>::I2C();
    }

    static callback_t   m_callback;
    static uint8_t      *m_buf, *m_ptr;
    static uint8_t      m_bufsize;
};

template<int NO, gpio::gpio_pin_t SCL, gpio::gpio_pin_t SDA>
typename i2c_slave_t<NO, SCL, SDA>::callback_t i2c_slave_t<NO, SCL, SDA>::m_callback = 0;

template<int NO, gpio::gpio_pin_t SCL, gpio::gpio_pin_t SDA>
uint8_t *i2c_slave_t<NO, SCL, SDA>::m_buf = 0;

template<int NO, gpio::gpio_pin_t SCL, gpio::gpio_pin_t SDA>
uint8_t *i2c_slave_t<NO, SCL, SDA>::m_ptr = 0;

template<int NO, gpio::gpio_pin_t SCL, gpio::gpio_pin_t SDA>
uint8_t i2c_slave_t<NO, SCL, SDA>::m_bufsize = 0;

} // namespace i2c
} // namespace hal


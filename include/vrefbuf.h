#pragma once

namespace hal
{

namespace vrefbuf
{

enum vrs_t
    { vrs_2048 = 0x0
    , vrs_2500 = 0x1
    , vrs_2900 = 0x2
    };

struct vrefbuf_t
{
    template<vrs_t vrs>
    static void setup()
    {
        using namespace device;
        typedef device::vrefbuf_t _;

        peripheral_traits<device::syscfg_t>::enable();  // enable vrefbuf clock
        VREFBUF.CSR = _::CSR_RESET_VALUE;               // reset control register
        VREFBUF.CSR &= ~_::CSR_HIZ;                     // clear high-impedance mode
        VREFBUF.CSR |= _::CSR_VRS<vrs>                  // set voltage reference scale
                    |  _::CSR_ENVR                      // enable voltage reference buffer
                    ;
        while (!(VREFBUF.CSR & _::CSR_VRR));            // wait for voltage reference ready
    }
};

} // namespace vrefbuf

} // namespace hal


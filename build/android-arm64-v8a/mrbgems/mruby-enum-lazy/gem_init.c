/*
 * This file is loading the irep
 * Ruby GEM code.
 *
 * IMPORTANT:
 *   This file was generated!
 *   All manual changes will get lost.
 */
#include <stdlib.h>
#include <mruby.h>
#include <mruby/irep.h>
/* dumped in little endian order.
   use `mrbc -E` option for big endian CPU. */
#include <stdint.h>
extern const uint8_t gem_mrblib_irep_mruby_enum_lazy[];
const uint8_t
#if defined __GNUC__
__attribute__((aligned(4)))
#elif defined _MSC_VER
__declspec(align(4))
#endif
gem_mrblib_irep_mruby_enum_lazy[] = {
0x45,0x54,0x49,0x52,0x30,0x30,0x30,0x35,0xb4,0x2d,0x00,0x00,0x0c,0xc6,0x4d,0x41,
0x54,0x5a,0x30,0x30,0x30,0x30,0x49,0x52,0x45,0x50,0x00,0x00,0x0b,0x12,0x30,0x30,
0x30,0x32,0x00,0x00,0x00,0x88,0x00,0x01,0x00,0x03,0x00,0x02,0x00,0x00,0x00,0x15,
0x0f,0x01,0x5b,0x01,0x00,0x5c,0x01,0x00,0x0f,0x01,0x0f,0x02,0x5a,0x01,0x01,0x5c,
0x01,0x01,0x37,0x01,0x67,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x0a,0x45,
0x6e,0x75,0x6d,0x65,0x72,0x61,0x62,0x6c,0x65,0x00,0x00,0x0a,0x45,0x6e,0x75,0x6d,
0x65,0x72,0x61,0x74,0x6f,0x72,0x00,0x00,0x00,0x00,0x55,0x00,0x01,0x00,0x03,0x00,
0x01,0x00,0x00,0x00,0x0d,0x00,0x00,0x00,0x61,0x01,0x56,0x02,0x00,0x5d,0x01,0x00,
0x0e,0x01,0x00,0x37,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x01,0x00,0x04,0x6c,
0x61,0x7a,0x79,0x00,0x00,0x00,0x00,0x7c,0x00,0x02,0x00,0x05,0x00,0x00,0x00,0x00,
0x00,0x12,0x00,0x00,0x33,0x00,0x00,0x00,0x1b,0x02,0x01,0x1d,0x02,0x00,0x10,0x03,
0x2e,0x02,0x02,0x01,0x37,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x03,0x00,0x04,
0x4c,0x61,0x7a,0x79,0x00,0x00,0x0a,0x45,0x6e,0x75,0x6d,0x65,0x72,0x61,0x74,0x6f,
0x72,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,0x00,0x00,0x62,0x00,0x01,0x00,0x03,
0x00,0x01,0x00,0x00,0x00,0x0d,0x00,0x00,0x0f,0x01,0x1b,0x02,0x00,0x5a,0x01,0x01,
0x5c,0x01,0x00,0x37,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x0a,0x45,
0x6e,0x75,0x6d,0x65,0x72,0x61,0x74,0x6f,0x72,0x00,0x00,0x04,0x4c,0x61,0x7a,0x79,
0x00,0x00,0x00,0x02,0xbc,0x00,0x01,0x00,0x03,0x00,0x0d,0x00,0x00,0x00,0x7b,0x00,
0x61,0x01,0x56,0x02,0x00,0x5d,0x01,0x00,0x61,0x01,0x56,0x02,0x01,0x5d,0x01,0x01,
0x5e,0x02,0x01,0x61,0x01,0x56,0x02,0x02,0x5d,0x01,0x03,0x5e,0x04,0x03,0x61,0x01,
0x56,0x02,0x03,0x5d,0x01,0x05,0x5e,0x06,0x05,0x61,0x01,0x56,0x02,0x04,0x5d,0x01,
0x07,0x61,0x01,0x56,0x02,0x05,0x5d,0x01,0x08,0x61,0x01,0x56,0x02,0x06,0x5d,0x01,
0x09,0x61,0x01,0x56,0x02,0x07,0x5d,0x01,0x0a,0x61,0x01,0x56,0x02,0x08,0x5d,0x01,
0x0b,0x61,0x01,0x56,0x02,0x09,0x5d,0x01,0x0c,0x61,0x01,0x56,0x02,0x0a,0x5d,0x01,
0x0d,0x5e,0x0e,0x0d,0x61,0x01,0x56,0x02,0x0b,0x5d,0x01,0x0f,0x61,0x01,0x56,0x02,
0x0c,0x5d,0x01,0x10,0x5e,0x11,0x12,0x0f,0x01,0x37,0x01,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x13,0x00,0x0a,0x69,0x6e,0x69,0x74,0x69,0x61,0x6c,0x69,0x7a,0x65,0x00,
0x00,0x07,0x74,0x6f,0x5f,0x65,0x6e,0x75,0x6d,0x00,0x00,0x08,0x65,0x6e,0x75,0x6d,
0x5f,0x66,0x6f,0x72,0x00,0x00,0x03,0x6d,0x61,0x70,0x00,0x00,0x07,0x63,0x6f,0x6c,
0x6c,0x65,0x63,0x74,0x00,0x00,0x06,0x73,0x65,0x6c,0x65,0x63,0x74,0x00,0x00,0x08,
0x66,0x69,0x6e,0x64,0x5f,0x61,0x6c,0x6c,0x00,0x00,0x06,0x72,0x65,0x6a,0x65,0x63,
0x74,0x00,0x00,0x04,0x67,0x72,0x65,0x70,0x00,0x00,0x04,0x64,0x72,0x6f,0x70,0x00,
0x00,0x0a,0x64,0x72,0x6f,0x70,0x5f,0x77,0x68,0x69,0x6c,0x65,0x00,0x00,0x04,0x74,
0x61,0x6b,0x65,0x00,0x00,0x0a,0x74,0x61,0x6b,0x65,0x5f,0x77,0x68,0x69,0x6c,0x65,
0x00,0x00,0x08,0x66,0x6c,0x61,0x74,0x5f,0x6d,0x61,0x70,0x00,0x00,0x0e,0x63,0x6f,
0x6c,0x6c,0x65,0x63,0x74,0x5f,0x63,0x6f,0x6e,0x63,0x61,0x74,0x00,0x00,0x03,0x7a,
0x69,0x70,0x00,0x00,0x04,0x75,0x6e,0x69,0x71,0x00,0x00,0x05,0x66,0x6f,0x72,0x63,
0x65,0x00,0x00,0x04,0x74,0x6f,0x5f,0x61,0x00,0x00,0x00,0x00,0x5a,0x00,0x03,0x00,
0x06,0x00,0x01,0x00,0x00,0x00,0x10,0x00,0x33,0x04,0x00,0x01,0x32,0x04,0x00,0x00,
0x55,0x04,0x00,0x31,0x03,0x00,0x37,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0xed,0x00,0x03,0x00,0x05,0x00,0x01,0x00,0x00,0x00,0x2f,0x00,0x00,
0x33,0x04,0x00,0x00,0x25,0x00,0x15,0x1f,0x03,0x01,0x00,0x55,0x04,0x00,0x2f,0x03,
0x00,0x00,0x21,0x00,0x2b,0x26,0x03,0x1b,0x04,0x01,0x27,0x03,0x04,0x22,0x04,0x00,
0x24,0x21,0x00,0x29,0x0f,0x03,0x21,0x00,0x2d,0x29,0x03,0x28,0x01,0x37,0x03,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x04,0x65,0x61,0x63,0x68,0x00,0x00,0x0d,
0x53,0x74,0x6f,0x70,0x49,0x74,0x65,0x72,0x61,0x74,0x69,0x6f,0x6e,0x00,0x00,0x00,
0x00,0xd2,0x00,0x03,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x2b,0x33,0x04,0x00,0x00,
0x1f,0x03,0x02,0x01,0x23,0x03,0x00,0x1e,0x1f,0x03,0x02,0x01,0x1f,0x04,0x01,0x00,
0x01,0x05,0x01,0x2e,0x03,0x00,0x02,0x21,0x00,0x29,0x1f,0x03,0x01,0x00,0x01,0x04,
0x01,0x2e,0x03,0x01,0x01,0x37,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,
0x04,0x63,0x61,0x6c,0x6c,0x00,0x00,0x02,0x3c,0x3c,0x00,0x00,0x00,0x02,0x2a,0x00,
0x05,0x00,0x09,0x00,0x00,0x00,0x00,0x00,0x6a,0x00,0x00,0x00,0x33,0x00,0x30,0x01,
0x21,0x00,0x0a,0x21,0x00,0x0d,0x0e,0x01,0x00,0x10,0x05,0x01,0x06,0x01,0x2e,0x05,
0x01,0x01,0x23,0x05,0x00,0x1d,0x21,0x00,0x33,0x10,0x05,0x1b,0x06,0x03,0x4f,0x07,
0x00,0x01,0x08,0x01,0x50,0x07,0x4f,0x08,0x01,0x50,0x07,0x2e,0x05,0x02,0x02,0x1b,
0x05,0x04,0x10,0x06,0x01,0x07,0x03,0x2f,0x05,0x05,0x01,0x01,0x04,0x05,0x10,0x05,
0x01,0x06,0x04,0x01,0x07,0x05,0x2e,0x06,0x06,0x01,0x01,0x05,0x01,0x01,0x06,0x04,
0x01,0x07,0x05,0x2e,0x06,0x07,0x01,0x01,0x05,0x02,0x01,0x06,0x04,0x01,0x07,0x05,
0x2e,0x06,0x08,0x01,0x37,0x04,0x00,0x00,0x00,0x02,0x00,0x00,0x11,0x75,0x6e,0x64,
0x65,0x66,0x69,0x6e,0x65,0x64,0x20,0x6d,0x65,0x74,0x68,0x6f,0x64,0x20,0x00,0x00,
0x00,0x00,0x00,0x00,0x09,0x00,0x04,0x65,0x61,0x63,0x68,0x00,0x00,0x0b,0x72,0x65,
0x73,0x70,0x6f,0x6e,0x64,0x5f,0x74,0x6f,0x3f,0x00,0x00,0x05,0x72,0x61,0x69,0x73,
0x65,0x00,0x00,0x0d,0x4e,0x6f,0x4d,0x65,0x74,0x68,0x6f,0x64,0x45,0x72,0x72,0x6f,
0x72,0x00,0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,
0x04,0x6f,0x62,0x6a,0x3d,0x00,0x00,0x05,0x6d,0x65,0x74,0x68,0x3d,0x00,0x00,0x05,
0x61,0x72,0x67,0x73,0x3d,0x00,0x00,0x00,0x00,0x6f,0x00,0x02,0x00,0x05,0x00,0x01,
0x00,0x00,0x00,0x12,0x33,0x00,0x00,0x01,0x1b,0x02,0x00,0x10,0x03,0x55,0x04,0x00,
0x2f,0x02,0x01,0x01,0x37,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x04,
0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,0x00,0x00,0x86,0x00,
0x04,0x00,0x08,0x00,0x00,0x00,0x00,0x00,0x18,0x00,0x00,0x00,0x33,0x08,0x00,0x00,
0x01,0x04,0x01,0x1f,0x05,0x01,0x00,0x01,0x06,0x02,0x2e,0x05,0x01,0x01,0x2e,0x04,
0x00,0x01,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x02,0x3c,0x3c,
0x00,0x00,0x04,0x63,0x61,0x6c,0x6c,0x00,0x00,0x00,0x00,0x6f,0x00,0x02,0x00,0x05,
0x00,0x01,0x00,0x00,0x00,0x12,0x00,0x00,0x33,0x00,0x00,0x01,0x1b,0x02,0x00,0x10,
0x03,0x55,0x04,0x00,0x2f,0x02,0x01,0x01,0x37,0x02,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x02,0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,
0x00,0x00,0xb6,0x00,0x04,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x24,0x00,0x00,0x00,
0x33,0x08,0x00,0x00,0x1f,0x04,0x01,0x00,0x01,0x05,0x02,0x2e,0x04,0x00,0x01,0x23,
0x04,0x00,0x20,0x01,0x04,0x01,0x01,0x05,0x02,0x2e,0x04,0x01,0x01,0x21,0x00,0x22,
0x0f,0x04,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x04,0x63,0x61,
0x6c,0x6c,0x00,0x00,0x02,0x3c,0x3c,0x00,0x00,0x00,0x00,0x6f,0x00,0x02,0x00,0x05,
0x00,0x01,0x00,0x00,0x00,0x12,0x00,0x00,0x33,0x00,0x00,0x01,0x1b,0x02,0x00,0x10,
0x03,0x55,0x04,0x00,0x2f,0x02,0x01,0x01,0x37,0x02,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x02,0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,
0x00,0x00,0xb6,0x00,0x04,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x24,0x00,0x00,0x00,
0x33,0x08,0x00,0x00,0x1f,0x04,0x01,0x00,0x01,0x05,0x02,0x2e,0x04,0x00,0x01,0x23,
0x04,0x00,0x18,0x0f,0x04,0x21,0x00,0x22,0x01,0x04,0x01,0x01,0x05,0x02,0x2e,0x04,
0x01,0x01,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x04,0x63,0x61,
0x6c,0x6c,0x00,0x00,0x02,0x3c,0x3c,0x00,0x00,0x00,0x00,0x6f,0x00,0x03,0x00,0x06,
0x00,0x01,0x00,0x00,0x00,0x12,0x00,0x00,0x33,0x04,0x00,0x00,0x1b,0x03,0x00,0x10,
0x04,0x55,0x05,0x00,0x2f,0x03,0x01,0x01,0x37,0x03,0x00,0x00,0x00,0x00,0x00,0x00,
0x00,0x02,0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,
0x00,0x00,0xb5,0x00,0x04,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x24,0x00,0x00,0x00,
0x33,0x08,0x00,0x00,0x1f,0x04,0x01,0x00,0x01,0x05,0x02,0x2e,0x04,0x00,0x01,0x23,
0x04,0x00,0x20,0x01,0x04,0x01,0x01,0x05,0x02,0x2e,0x04,0x01,0x01,0x21,0x00,0x22,
0x0f,0x04,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x03,0x3d,0x3d,
0x3d,0x00,0x00,0x02,0x3c,0x3c,0x00,0x00,0x00,0x00,0x77,0x00,0x04,0x00,0x07,0x00,
0x01,0x00,0x00,0x00,0x14,0x00,0x00,0x00,0x33,0x04,0x00,0x00,0x06,0x03,0x1b,0x04,
0x00,0x10,0x05,0x55,0x06,0x00,0x2f,0x04,0x01,0x01,0x37,0x04,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x02,0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,
0x00,0x00,0x00,0x00,0xdf,0x00,0x04,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x2e,0x00,
0x33,0x08,0x00,0x00,0x1f,0x04,0x03,0x00,0x1f,0x05,0x01,0x00,0x42,0x04,0x00,0x23,
0x04,0x00,0x22,0x1f,0x04,0x03,0x00,0x3c,0x04,0x01,0x01,0x20,0x04,0x03,0x00,0x21,
0x00,0x2c,0x01,0x04,0x01,0x01,0x05,0x02,0x2e,0x04,0x02,0x01,0x37,0x04,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x03,0x00,0x01,0x3c,0x00,0x00,0x01,0x2b,0x00,0x00,0x02,
0x3c,0x3c,0x00,0x00,0x00,0x00,0x77,0x00,0x03,0x00,0x06,0x00,0x01,0x00,0x00,0x00,
0x14,0x00,0x00,0x00,0x33,0x00,0x00,0x01,0x11,0x02,0x1b,0x03,0x00,0x10,0x04,0x55,
0x05,0x00,0x2f,0x03,0x01,0x01,0x37,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,
0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,0x00,0x01,
0x36,0x00,0x04,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x43,0x00,0x33,0x08,0x00,0x00,
0x1f,0x04,0x02,0x00,0x23,0x04,0x00,0x37,0x1f,0x04,0x01,0x00,0x01,0x05,0x02,0x2e,
0x04,0x00,0x01,0x2e,0x04,0x01,0x00,0x23,0x04,0x00,0x32,0x01,0x04,0x01,0x01,0x05,
0x02,0x2e,0x04,0x02,0x01,0x12,0x04,0x20,0x04,0x02,0x00,0x21,0x00,0x34,0x0f,0x04,
0x21,0x00,0x41,0x01,0x04,0x01,0x01,0x05,0x02,0x2e,0x04,0x02,0x01,0x37,0x04,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x03,0x00,0x04,0x63,0x61,0x6c,0x6c,0x00,0x00,0x01,
0x21,0x00,0x00,0x02,0x3c,0x3c,0x00,0x00,0x00,0x00,0xe4,0x00,0x04,0x00,0x07,0x00,
0x02,0x00,0x00,0x00,0x2e,0x00,0x00,0x00,0x33,0x04,0x00,0x00,0x01,0x04,0x01,0x06,
0x05,0x41,0x04,0x00,0x23,0x04,0x00,0x1e,0x1b,0x04,0x01,0x10,0x05,0x55,0x06,0x00,
0x2f,0x04,0x02,0x01,0x37,0x04,0x06,0x03,0x1b,0x04,0x01,0x10,0x05,0x55,0x06,0x01,
0x2f,0x04,0x02,0x01,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x03,0x00,0x02,
0x3d,0x3d,0x00,0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,
0x00,0x00,0x00,0x5e,0x00,0x01,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x0b,0x00,0x00,
0x10,0x01,0x1b,0x02,0x01,0x2e,0x01,0x00,0x01,0x37,0x01,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x02,0x00,0x05,0x72,0x61,0x69,0x73,0x65,0x00,0x00,0x0d,0x53,0x74,0x6f,
0x70,0x49,0x74,0x65,0x72,0x61,0x74,0x69,0x6f,0x6e,0x00,0x00,0x00,0x01,0x24,0x00,
0x04,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x39,0x00,0x00,0x00,0x33,0x08,0x00,0x00,
0x01,0x04,0x01,0x01,0x05,0x02,0x2e,0x04,0x00,0x01,0x1f,0x04,0x03,0x00,0x3c,0x04,
0x01,0x01,0x20,0x04,0x03,0x00,0x1f,0x04,0x03,0x00,0x1f,0x05,0x01,0x00,0x45,0x04,
0x02,0x23,0x04,0x00,0x35,0x10,0x04,0x1b,0x05,0x04,0x2e,0x04,0x03,0x01,0x21,0x00,
0x37,0x0f,0x04,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x05,0x00,0x02,0x3c,
0x3c,0x00,0x00,0x01,0x2b,0x00,0x00,0x02,0x3e,0x3d,0x00,0x00,0x05,0x72,0x61,0x69,
0x73,0x65,0x00,0x00,0x0d,0x53,0x74,0x6f,0x70,0x49,0x74,0x65,0x72,0x61,0x74,0x69,
0x6f,0x6e,0x00,0x00,0x00,0x00,0x6f,0x00,0x02,0x00,0x05,0x00,0x01,0x00,0x00,0x00,
0x12,0x00,0x00,0x00,0x33,0x00,0x00,0x01,0x1b,0x02,0x00,0x10,0x03,0x55,0x04,0x00,
0x2f,0x02,0x01,0x01,0x37,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x04,
0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,0x00,0x00,0xea,0x00,
0x04,0x00,0x07,0x00,0x00,0x00,0x00,0x00,0x2b,0x00,0x00,0x00,0x33,0x08,0x00,0x00,
0x1f,0x04,0x01,0x00,0x01,0x05,0x02,0x2e,0x04,0x00,0x01,0x23,0x04,0x00,0x20,0x01,
0x04,0x01,0x01,0x05,0x02,0x2e,0x04,0x01,0x01,0x21,0x00,0x29,0x10,0x04,0x1b,0x05,
0x03,0x2e,0x04,0x02,0x01,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x04,0x00,
0x04,0x63,0x61,0x6c,0x6c,0x00,0x00,0x02,0x3c,0x3c,0x00,0x00,0x05,0x72,0x61,0x69,
0x73,0x65,0x00,0x00,0x0d,0x53,0x74,0x6f,0x70,0x49,0x74,0x65,0x72,0x61,0x74,0x69,
0x6f,0x6e,0x00,0x00,0x00,0x00,0x6f,0x00,0x02,0x00,0x05,0x00,0x01,0x00,0x00,0x00,
0x12,0x00,0x00,0x00,0x33,0x00,0x00,0x01,0x1b,0x02,0x00,0x10,0x03,0x55,0x04,0x00,
0x2f,0x02,0x01,0x01,0x37,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x04,
0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,0x00,0x00,0x94,0x00,
0x05,0x00,0x08,0x00,0x01,0x00,0x00,0x00,0x1b,0x00,0x00,0x00,0x33,0x08,0x00,0x00,
0x1f,0x05,0x01,0x00,0x01,0x06,0x02,0x2e,0x05,0x00,0x01,0x01,0x04,0x05,0x55,0x06,
0x00,0x2f,0x05,0x01,0x00,0x37,0x05,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,
0x04,0x63,0x61,0x6c,0x6c,0x00,0x00,0x04,0x65,0x61,0x63,0x68,0x00,0x00,0x00,0x00,
0x63,0x00,0x03,0x00,0x06,0x00,0x00,0x00,0x00,0x00,0x11,0x00,0x33,0x04,0x00,0x00,
0x1f,0x03,0x01,0x00,0x01,0x04,0x01,0x2e,0x03,0x00,0x01,0x37,0x03,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x01,0x00,0x02,0x3c,0x3c,0x00,0x00,0x00,0x00,0xab,0x00,0x04,
0x00,0x07,0x00,0x01,0x00,0x00,0x00,0x20,0x33,0x00,0x10,0x01,0x10,0x04,0x46,0x04,
0x01,0x01,0x05,0x01,0x3b,0x04,0x00,0x01,0x03,0x04,0x1b,0x04,0x01,0x10,0x05,0x55,
0x06,0x00,0x2f,0x04,0x02,0x01,0x37,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x03,
0x00,0x01,0x2b,0x00,0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,
0x00,0x00,0x00,0x01,0x18,0x00,0x05,0x00,0x09,0x00,0x01,0x00,0x00,0x00,0x3b,0x00,
0x33,0x08,0x00,0x00,0x1f,0x05,0x03,0x00,0x55,0x06,0x00,0x2f,0x05,0x00,0x00,0x01,
0x04,0x05,0x1f,0x05,0x02,0x00,0x23,0x05,0x00,0x2f,0x01,0x05,0x01,0x1f,0x06,0x02,
0x00,0x01,0x07,0x04,0x2e,0x06,0x02,0x01,0x2e,0x05,0x01,0x01,0x21,0x00,0x39,0x01,
0x05,0x01,0x01,0x06,0x04,0x2e,0x05,0x01,0x01,0x37,0x05,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x03,0x00,0x03,0x6d,0x61,0x70,0x00,0x00,0x02,0x3c,0x3c,0x00,0x00,0x04,
0x63,0x61,0x6c,0x6c,0x00,0x00,0x00,0x00,0x55,0x00,0x03,0x00,0x05,0x00,0x00,0x00,
0x00,0x00,0x0d,0x00,0x33,0x04,0x00,0x00,0x01,0x03,0x01,0x2e,0x03,0x00,0x00,0x37,
0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x01,0x00,0x04,0x6e,0x65,0x78,0x74,0x00,
0x00,0x00,0x00,0x87,0x00,0x03,0x00,0x06,0x00,0x01,0x00,0x00,0x00,0x18,0x00,0x00,
0x33,0x00,0x00,0x01,0x51,0x03,0x00,0x01,0x02,0x03,0x1b,0x03,0x00,0x10,0x04,0x55,
0x05,0x00,0x2f,0x03,0x01,0x01,0x37,0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,
0x00,0x04,0x4c,0x61,0x7a,0x79,0x00,0x00,0x03,0x6e,0x65,0x77,0x00,0x00,0x00,0x01,
0x93,0x00,0x05,0x00,0x0a,0x00,0x00,0x00,0x00,0x00,0x57,0x00,0x33,0x08,0x00,0x00,
0x1f,0x05,0x01,0x00,0x23,0x05,0x00,0x1d,0x1f,0x05,0x01,0x00,0x01,0x06,0x02,0x2e,
0x05,0x00,0x01,0x01,0x04,0x05,0x21,0x00,0x23,0x01,0x05,0x02,0x01,0x04,0x05,0x1f,
0x05,0x02,0x00,0x01,0x06,0x04,0x2e,0x05,0x01,0x01,0x23,0x05,0x00,0x37,0x0f,0x05,
0x21,0x00,0x55,0x01,0x05,0x01,0x01,0x06,0x02,0x2e,0x05,0x02,0x01,0x01,0x05,0x02,
0x1f,0x06,0x02,0x00,0x01,0x07,0x04,0x01,0x08,0x05,0x2e,0x06,0x03,0x02,0x01,0x05,
0x05,0x37,0x05,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x04,0x00,0x04,0x63,0x61,0x6c,
0x6c,0x00,0x00,0x08,0x69,0x6e,0x63,0x6c,0x75,0x64,0x65,0x3f,0x00,0x00,0x02,0x3c,
0x3c,0x00,0x00,0x03,0x5b,0x5d,0x3d,0x00,0x4c,0x56,0x41,0x52,0x00,0x00,0x01,0x96,
0x00,0x00,0x00,0x13,0x00,0x01,0x26,0x00,0x03,0x6f,0x62,0x6a,0x00,0x05,0x62,0x6c,
0x6f,0x63,0x6b,0x00,0x07,0x79,0x69,0x65,0x6c,0x64,0x65,0x72,0x00,0x01,0x78,0x00,
0x04,0x6d,0x65,0x74,0x68,0x00,0x04,0x61,0x72,0x67,0x73,0x00,0x02,0x6c,0x7a,0x00,
0x03,0x76,0x61,0x6c,0x00,0x07,0x70,0x61,0x74,0x74,0x65,0x72,0x6e,0x00,0x01,0x6e,
0x00,0x07,0x64,0x72,0x6f,0x70,0x70,0x65,0x64,0x00,0x08,0x64,0x72,0x6f,0x70,0x70,
0x69,0x6e,0x67,0x00,0x05,0x74,0x61,0x6b,0x65,0x6e,0x00,0x03,0x61,0x72,0x79,0x00,
0x05,0x65,0x6e,0x75,0x6d,0x73,0x00,0x01,0x65,0x00,0x04,0x68,0x61,0x73,0x68,0x00,
0x01,0x76,0x00,0x00,0x00,0x01,0x00,0x01,0x00,0x01,0x00,0x02,0x00,0x02,0x00,0x03,
0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x04,0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x05,
0x00,0x01,0x00,0x06,0x00,0x02,0x00,0x02,0x00,0x03,0x00,0x07,0x00,0x04,0x00,0x02,
0x00,0x01,0x00,0x03,0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x02,
0x00,0x01,0x00,0x03,0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x02,
0x00,0x01,0x00,0x03,0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x09,
0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x03,0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,
0x00,0x03,0x00,0x0a,0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x0b,0x00,0x03,0x00,0x03,
0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x02,0x00,0x01,0x00,0x0c,
0x00,0x02,0x00,0x03,0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x0a,
0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x0d,0x00,0x03,0x00,0x03,0x00,0x01,0x00,0x08,
0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x02,0x00,0x01,0x00,0x03,0x00,0x01,0x00,0x08,
0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x02,0x00,0x01,0x00,0x03,0x00,0x01,0x00,0x08,
0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x0e,0x00,0x04,0x00,0x04,0x00,0x01,0x00,0x00,
0x00,0x02,0x00,0x06,0x00,0x01,0x00,0x02,0x00,0x02,0x00,0x0f,0x00,0x03,0x00,0x03,
0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x0e,0x00,0x04,0x00,0x10,
0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x02,0x00,0x01,0x00,0x11,0x00,0x02,0x00,0x03,
0x00,0x01,0x00,0x08,0x00,0x02,0x00,0x00,0x00,0x03,0x00,0x12,0x00,0x04,0x45,0x4e,
0x44,0x00,0x00,0x00,0x00,0x08,
};
void mrb_mruby_enum_lazy_gem_init(mrb_state *mrb);
void mrb_mruby_enum_lazy_gem_final(mrb_state *mrb);

void GENERATED_TMP_mrb_mruby_enum_lazy_gem_init(mrb_state *mrb) {
  int ai = mrb_gc_arena_save(mrb);
  mrb_load_irep(mrb, gem_mrblib_irep_mruby_enum_lazy);
  if (mrb->exc) {
    mrb_print_error(mrb);
    mrb_close(mrb);
    exit(EXIT_FAILURE);
  }
  mrb_gc_arena_restore(mrb, ai);
}

void GENERATED_TMP_mrb_mruby_enum_lazy_gem_final(mrb_state *mrb) {
}

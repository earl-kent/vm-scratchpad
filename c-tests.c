#include <stdio.h>
#include <stdint.h>

/* Types used by the macro */
typedef uint8_t  uintB;
typedef uint32_t uintL;

/* bit(n) macro from your environment */
#define bit(n)  (1UL << (n))

/* Your U_operand macro exactly as given */
#define U_operand(where)  \
    { where = *byteptr++;           /* read first Byte */ \
      if ((uintB)where & bit(7))    /* Bit 7 set? */ \
        { where &= ~bit(7);         /* yes -> delete */ \
          where = where << 8;       \
          where |= *byteptr++;      /* and read next Byte */ \
        } \
    }

/* Test routine */
int main(void)
{
    /* Test byte sequences */
    uint8_t test1[] = { 0x05 };              // single-byte operand (5)
    uint8_t test2[] = { 0x85, 0x34 };        // two-byte operand: (0x05 << 8) | 0x34 = 0x0534
    uint8_t test3[] = { 0xFF, 0x10 };        // two-byte operand: (0x7F << 8) | 0x10 = 0x7F10

    uintL where;
    uint8_t *byteptr;

    /* ---- Test 1 ---- */
    byteptr = test1;
    U_operand(where);
    printf("Test1: result = 0x%X (decimal %u)\n", where, where);

    /* ---- Test 2 ---- */
    byteptr = test2;
    U_operand(where);
    printf("Test2: result = 0x%X (decimal %u)\n", where, where);

    /* ---- Test 3 ---- */
    byteptr = test3;
    U_operand(where);
    printf("Test3: result = 0x%X (decimal %u)\n", where, where);

    return 0;
}

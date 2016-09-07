#include <stdint.h>
#include <stdbool.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

#ifdef __clang__
#if !  __has_builtin(__builtin_uadd_overflow)
#define NEED 1
#endif
#else
#if (__GNUC__ * 100 + __GNUC_MINOR__) < 501
#define NEED 1
#endif
#endif

#ifdef NEED
#define __unsigned_add_overflow(a, b, d) ({	\
	typeof(a) __a = (a);			\
	typeof(b) __b = (b);			\
	typeof(d) __d = (d);			\
	(void) (&__a == &__b);			\
	(void) (&__a == __d);			\
	*__d = __a + __b;			\
	*__d < __a;				\
})

#define __type_half_max(type) ((type)1 << (8*sizeof(type) - 1))
#define type_max(T) ((T)((__type_half_max(T) - 1) + __type_half_max(T)))
#define __unsigned_mul_overflow(a, b, d) ({		\
	typeof(a) __a = (a);				\
	typeof(b) __b = (b);				\
	typeof(d) __d = (d);				\
	(void) (&__a == &__b);				\
	(void) (&__a == __d);				\
	*__d = __a * __b;				\
	__b > 0 && __a > type_max(typeof(__a)) / __b;	\
})

#define __unsigned_sub_overflow(a, b, d) ({	\
	typeof(a) __a = (a);			\
	typeof(b) __b = (b);			\
	typeof(d) __d = (d);			\
	(void) (&__a == &__b);			\
	(void) (&__a == __d);			\
	*__d = __a - __b;			\
	__a < __b;				\
})

#else
#define __unsigned_add_overflow __builtin_add_overflow
#define __unsigned_mul_overflow __builtin_mul_overflow
#define __unsigned_sub_overflow __builtin_sub_overflow
#endif

#define op(t, n, x, c, f) \
  CAMLprim value                                         \
  caml_ ## n ## _ ## f ## _overflow (value a, value b) { \
    t ua, ub, uc;                                        \
    bool carry;                                          \
    CAMLparam2(a, b);                                    \
    ua = x(a);                                           \
    ub = x(b);                                           \
    carry = __unsigned_ ## f ## _overflow (ua, ub, &uc); \
    CAMLlocal1(res);                                     \
    res = caml_alloc_tuple(2);                           \
    Store_field(res, 0, c(uc));                          \
    Store_field(res, 1, Val_bool(carry));                \
    CAMLreturn(res);                                     \
  }

#define add_mul_sub(t, n, x, c) \
  op(t, n, x, c, add)           \
  op(t, n, x, c, mul)           \
  op(t, n, x, c, sub)

add_mul_sub(uint8_t, _uint8, Int_val, Val_int)

add_mul_sub(uint16_t, _uint16, Int_val, Val_int)

#define Uint32_val(v) (*((uint32_t *) Data_custom_val(v)))
add_mul_sub(uint32_t, uint32, Uint32_val, caml_copy_int32)

#define Uint64_val(v) (*((uint64_t *) Data_custom_val(v)))
add_mul_sub(uint64_t, uint64, Uint64_val, caml_copy_int64)
